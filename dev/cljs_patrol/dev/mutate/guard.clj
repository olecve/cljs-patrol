(ns cljs-patrol.dev.mutate.guard
  "Find the files that the tests of a mutant add, change or delete in the working directory, which the runner cannot undo.

  The guard compares a snapshot of every file, its path, size and time of change, with the one
  taken after the baseline. A snapshot costs a walk of the tree, so a mutant gets one only when
  its tests could write a file: when one of them wrote a file in the baseline, as Java Flight
  Recorder saw it, or added or deleted a name directly in the working directory, or when the
  baseline left a file changed. One more snapshot at the end of the run finds what a mutant
  without a snapshot left."
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.nio.file Files LinkOption Path]
   [java.nio.file.attribute BasicFileAttributes FileAttribute]
   [java.security MessageDigest]
   [java.time Duration Instant]
   [jdk.jfr Recording]
   [jdk.jfr.consumer RecordedEvent RecordingFile]))

(def ^:private skipped
  "Folders a snapshot does not enter: the files of git, and the cache that the run writes as it goes."
  #{".git" cache/default-dir})

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root-path ^Path [root]
  (.toPath (.getAbsoluteFile (io/file root))))

(defn- walk [^Path root ^Path dir files]
  (with-open [entries (Files/newDirectoryStream dir)]
    (reduce (fn [files ^Path path]
              (if (contains? skipped (str (.getFileName path)))
                files
                (let [attributes (Files/readAttributes path BasicFileAttributes ^"[Ljava.nio.file.LinkOption;" no-follow)]
                  (if (.isDirectory attributes)
                    (walk root path files)
                    (assoc! files (str (.relativize root path)) [(.size attributes) (.lastModifiedTime attributes)])))))
            files
            entries)))

(defn folder-name
  "Return the absolute path of `root` without `.` or `..` parts, to name it in a message."
  [root]
  (-> root root-path .normalize str))

(defn snapshot
  "Map the path of each file under `root`, relative to it, to its size and time of change.

  A link is a file of its own, so the walk does not follow it."
  [root]
  (let [path (root-path root)]
    (persistent! (walk path path (transient {})))))

(defn- content-hash [root path]
  (vec (.digest (MessageDigest/getInstance "SHA-256") (-> root root-path (.resolve ^String path) Files/readAllBytes))))

(defn changes
  "Return the files that snapshot `after` adds, changes or deletes against `before`, sorted, as [path change] pairs.

  A change is :added, :changed or :deleted. A file in `hashes`, path to the hash of its bytes,
  whose time alone changed is unchanged while its bytes hash the same, since a test that wrote
  it in the baseline writes it again for each mutant."
  [root before after hashes]
  (->> (into (set (keys before)) (keys after))
       sort
       (keep (fn [path]
               (let [old (get before path)
                     new (get after path)]
                 (cond
                   (nil? old) [path :added]
                   (nil? new) [path :deleted]
                   (= old new) nil
                   (and (= (first old) (first new)) (contains? hashes path) (= (get hashes path) (content-hash root path))) nil
                   :else [path :changed]))))
       vec))

(defn entries
  "Return the names directly in `root`.

  A test that makes a folder or runs a process there writes no file this JVM records. The OS
  temporary folder is not read, since other processes add and delete names there all the
  time."
  [root]
  (set (.list (io/file root))))

(defn start-recording
  "Start a Java Flight Recorder recording of every file write of this JVM, however short."
  ^Recording []
  (let [recording (Recording.)]
    (-> recording (.enable "jdk.FileWrite") (.withThreshold Duration/ZERO))
    (.start recording)
    recording))

(defn stop-recording
  "Stop `recording`, and return when each write to a file began, sorted; a write to the console names no file."
  [^Recording recording]
  (let [file (Files/createTempFile "mutate-writes" ".jfr" (make-array FileAttribute 0))]
    (try
      (.stop recording)
      (.dump recording file)
      (->> (RecordingFile/readAllEvents file)
           (filter #(.getString ^RecordedEvent % "path"))
           (map #(.getStartTime ^RecordedEvent %))
           sort
           vec)
      (finally
        (.close recording)
        (Files/deleteIfExists file)))))

(defn writers
  "Return the tests of `runs` that wrote a file at a time in `writes`, or whose run changed [[entries]].

  Each run is [test-var started ended entries-changed?], with the start and end as Instants."
  [writes runs]
  (into #{}
        (keep (fn [[test-var ^Instant started ^Instant ended entries-changed?]]
                (when (or entries-changed?
                          (some (fn [^Instant write] (not (or (.isBefore write started) (.isAfter write ended)))) writes))
                  test-var)))
        runs))

(defn start
  "Return the guard of a run in `root`, from the snapshots `before` and `after` the baseline, and the baseline's `writers`.

  `writers` are the tests that [[writers]] returns, or nil when the
  baseline could not record them. The guard looks after every mutant when that is nil, or when
  the baseline left a file changed."
  [root before after writers]
  (let [written (map first (changes root before after {}))]
    {:root root
     :files after
     :hashes (into {}
                   (keep #(when (contains? after %) [% (content-hash root %)]))
                   written)
     :every-mutant? (boolean (or (nil? writers) (seq written)))
     :writers writers}))

(defn check?
  "True when the tests that a mutant runs by `plan` could write a file, so the guard takes a snapshot after it."
  [{:keys [every-mutant? writers]} {:keys [kind tests]}]
  (boolean (or every-mutant?
               (if (= :subset kind)
                 (some writers tests)
                 (seq writers)))))

(defn changed-files
  "Return the files changed since the baseline, as [[changes]] does, or nil when no file changed."
  [{:keys [root files hashes]}]
  (not-empty (changes root files (snapshot root) hashes)))

(def ^:private listed-files 10)

(defn describe
  "Name the first 10 `changed` files, each on a line of its own with its change, and say how many more there are."
  [changed]
  (str/join "\n"
            (cond-> (mapv (fn [[path change]] (str "  " (name change) " " path)) (take listed-files changed))
              (< listed-files (count changed)) (conj (str "  and " (- (count changed) listed-files) " more")))))
