(ns cljs-patrol.dev.mutate.guard
  "Find the files that the tests of a mutant add, change or delete in the working directory, which the runner cannot undo.

  The guard compares a snapshot of every file, its path, size and time of change, with the one
  taken after the baseline. A snapshot costs a walk of the tree, so a mutant gets one only when
  its tests could write a file: when one of them wrote a file in the baseline, as Java Flight
  Recorder (JFR) saw it, or added or deleted a name directly in the working directory, or when
  the baseline left a file changed. One more snapshot at the end of the run, and at an early
  stop, finds what a mutant without a snapshot left. JFR also records the writes during the
  mutants, so a file that another process wrote, such as a source file that an editor saved,
  gives a warning, not a stop."
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.nio.file AccessDeniedException DirectoryIteratorException Files LinkOption NoSuchFileException Path]
   [java.nio.file.attribute BasicFileAttributes]
   [java.time Duration Instant]))

(def tool-caches
  "Folders, relative to the working directory, that editors, linters and the Clojure CLI write while a run goes on."
  #{".lsp" ".clj-kondo/.cache" ".cpcache" ".shadow-cljs"})

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root-path ^Path [root]
  (.toPath (.getAbsoluteFile (io/file root))))

(defn skipped-dirs
  "Return the folders that a snapshot of `root` does not enter, relative to `root`.

  They are the `.git` folder of `root`, the [[tool-caches]], and `cache-dir`, the folder of the
  run's cache, when it is under `root`."
  [root cache-dir]
  (let [base (.normalize (root-path root))
        cache-path (some-> cache-dir root-path .normalize)]
    (cond-> (conj tool-caches ".git")
      (and cache-path (.startsWith cache-path base) (not= cache-path base))
      (conj (str (.relativize base cache-path))))))

(defn file-attributes
  "Return the attributes of the file or folder at `path`, not following a link, or nil when it is gone."
  ^BasicFileAttributes [^Path path]
  (try
    (Files/readAttributes path BasicFileAttributes ^"[Ljava.nio.file.LinkOption;" no-follow)
    (catch NoSuchFileException _ nil)))

(defn- walk [^Path root skipped ^Path dir ^java.util.Map files]
  (try
    (with-open [entries (Files/newDirectoryStream dir)]
      (doseq [^Path path entries]
        (let [relative (str (.relativize root path))]
          (when-not (contains? skipped relative)
            (when-let [attributes (file-attributes path)]
              (if (.isDirectory attributes)
                (walk root skipped path files)
                (.put files relative [(.size attributes) (.lastModifiedTime attributes)])))))))
    (catch NoSuchFileException _ nil)
    (catch AccessDeniedException _
      (.put files (str (.relativize root dir) "/") :unreadable))
    (catch DirectoryIteratorException _ nil)))

(defn folder-name
  "Return the absolute path of `root` without `.` or `..` parts, to name it in a message."
  [root]
  (-> root root-path .normalize str))

(defn snapshot
  "Map the path of each file under `root`, relative to it, to its size and time of change.

  The walk does not enter the `skipped` folders, given relative to `root`. A link is a file of
  its own, so the walk does not follow it. A file that is deleted while the walk goes on is left
  out. A folder that cannot be read maps its path, with a `/` at the end, to :unreadable."
  [root skipped]
  (let [path (root-path root)
        files (java.util.HashMap.)]
    (walk path skipped path files)
    (into {} files)))

(defn- content-hash [root path]
  (try
    (-> root root-path (.resolve ^String path) Files/readAllBytes cache/bytes-hash)
    (catch NoSuchFileException _ nil)))

(defn changes
  "Return the files that snapshot `after` adds, changes or deletes against `before`, sorted, as [path change] pairs.

  A change is :added, :changed or :deleted. A file in `hashes`, path to the hash of its bytes,
  whose time alone changed is unchanged while its bytes hash the same, since a test that wrote
  it in the baseline writes it again for each mutant."
  [root before after hashes]
  (if (= before after)
    []
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
         vec)))

(defn entries
  "Return the names directly in `root`.

  A test that makes a folder or runs a process there writes no file this JVM records. The OS
  temporary folder is not read, since other processes add and delete names there all the
  time."
  [root]
  (set (.list (io/file root))))

(defn recorder-available?
  "True when this JDK has Java Flight Recorder, the `jdk.jfr` module."
  []
  (try
    (Class/forName "jdk.jfr.Recording")
    true
    (catch ClassNotFoundException _ false))) ; mutate: skip false: the test JDK has jdk.jfr, so no test reaches this

(defn start-recording
  "Start a recording of every file write of this JVM, or return nil when the JDK has no Java Flight Recorder."
  []
  (when (recorder-available?)
    ((requiring-resolve 'cljs-patrol.dev.mutate.recorder/start))))

(defn recorded-writes
  "Return each file write that `recording` holds so far, as {:path … :at …}, with an absolute path and its start time."
  [recording]
  ((requiring-resolve 'cljs-patrol.dev.mutate.recorder/writes) recording))

(defn close-recording
  "Stop `recording`, when there is one, and free what it holds."
  [recording]
  (when recording
    ((requiring-resolve 'cljs-patrol.dev.mutate.recorder/close) recording)))

(def ^:private clock-margin
  "How far a write may lie outside a time window and still count in it.

  [[recorded-writes]] moves each JFR time to the clock of Instant/now by one marker write, so a
  little of the offset between the two clocks can stay. A test that ran this close to a write
  counts as one that may write, which costs only a snapshot more."
  (Duration/ofMillis 2))

(defn- in-window? [^Instant at ^Instant started ^Instant ended]
  (not (or (.isBefore at (.minus started clock-margin))
           (.isAfter at (.plus ended clock-margin)))))

(defn writers
  "Return the tests that may write a file, from the baseline's `writes` and the `runs` of its tests.

  `writes` are {:path … :at …} maps, as [[recorded-writes]] returns them. Each run is
  [test-var started ended entries-changed?], with Instants. A test may write when a write
  falls in its run, give or take [[clock-margin]], or when the names directly in the working
  directory changed during it. A write that falls in no run comes from a fixture or a
  background thread, so every test of `test-vars` may write then."
  [writes runs test-vars]
  (if (some (fn [{:keys [at]}] (not-any? (fn [[_ started ended]] (in-window? at started ended)) runs)) writes)
    (set test-vars)
    (into #{}
          (keep (fn [[test-var started ended entries-changed?]]
                  (when (or entries-changed? (some #(in-window? (:at %) started ended) writes))
                    test-var)))
          runs)))

(defn baseline-writers
  "Return the tests that may write a file in the recorded baseline `run`, as [[writers]] does, and close its recording.

  Return nil when the run has no recording, so the guard takes a snapshot after every mutant."
  [{:keys [recording test-runs]} test-vars]
  (when recording
    (try
      (writers (recorded-writes recording) test-runs test-vars)
      (finally (close-recording recording)))))

(def ^:private no-recorder-note
  "This JDK has no Java Flight Recorder, so every mutant gets a snapshot, and every new or changed file stops the run.")

(defn start
  "Return the guard of a run in `root`, and start the recording of the writes during the mutants.

  `before` and `after` are the snapshots of `root` before and after the baseline, without the
  `skipped` folders. `writers` are the tests that may write a file, or nil when the baseline
  could not record them. The guard takes a snapshot after every mutant when that is nil, or
  when the baseline left a file changed. On a JDK without Java Flight Recorder, :note says so."
  [root skipped before after writers]
  (let [written (map first (changes root before after {}))
        recording (start-recording)]
    (cond-> {:root root
             :skipped skipped
             :files (atom after)
             :hashes (into {}
                           (keep #(when (contains? after %) [% (content-hash root %)]))
                           written)
             :every-mutant? (boolean (or (nil? writers) (seq written)))
             :writers writers
             :recording recording
             :windows (atom [])}
      (not recording) (assoc :note no-recorder-note))))

(defn close!
  "Stop the recording of `guard`."
  [guard]
  (close-recording (:recording guard)))

(defn mark-mutant!
  "Note that the tests of the mutant named `id` ran from `started` to `ended`, so a write then is the mutant's."
  [guard id started ended]
  (swap! (:windows guard) conj [id started ended]))

(defn check?
  "True when the tests that a mutant runs by `plan` could write a file, so the guard takes a snapshot after it."
  [{:keys [every-mutant? writers]} {:keys [kind tests]}]
  (boolean (or every-mutant?
               (if (= :subset kind)
                 (some writers tests)
                 (seq writers)))))

(defn- writer-of
  "Return the id of the mutant whose tests wrote the file at `path`, :unnamed for a write outside every mutant, or :other.

  :other means that this JVM did not write the file."
  [root windows writes current-id path]
  (let [absolute (-> root root-path (.resolve ^String path) .normalize str)
        times (keep #(when (= absolute (:path %)) (:at %)) writes)
        ids (keep (fn [[id started ended]] (when (some #(in-window? % started ended) times) id)) windows)]
    (cond
      (empty? times) :other
      (some #{current-id} ids) current-id
      (seq ids) (first ids)
      :else :unnamed)))

(defn look
  "Take a snapshot, and return the changes since the baseline that stop the run, or nil when there are none.

  The result is {:changed [[path change] …] :by id}, where :by names the mutant whose tests
  changed a file, or is nil when no mutant can be named. `current-id` names the mutant that just
  ran, or is nil at the end of the run. A deleted file always stops the run, since JFR records
  no delete, and it counts as the current mutant's. A new or changed file that this JVM did not
  write prints one warning, and the guard takes its new state as the reference, so it does not
  warn again. Without a recording, every new or changed file is the current mutant's."
  [{:keys [root skipped files hashes recording windows]} current-id]
  (let [now (snapshot root skipped)
        changed (changes root @files now hashes)]
    (when (seq changed)
      (let [writes (when recording (recorded-writes recording))
            by (fn [[path change]]
                 (if (and recording (not= :deleted change))
                   (writer-of root @windows writes current-id path)
                   (or current-id :unnamed)))
            classified (mapv (juxt identity by) changed)
            others (filter #(= :other (second %)) classified)
            own (remove #(= :other (second %)) classified)]
        (doseq [[[path]] others]
          (println (str "Warning: " path " changed during the run, but not by this run's tests."))
          (swap! files assoc path (get now path)))
        (when (seq own)
          (let [ids (filter string? (map second own))]
            {:changed (mapv first own)
             :by (or (some #{current-id} ids) (first ids))}))))))

(def ^:private listed-files 10)

(defn describe
  "Name the first 10 `changed` files, each on a line of its own with its change, and say how many more there are."
  [changed]
  (str/join "\n"
            (cond-> (mapv (fn [[path change]] (str "  " (name change) " " path)) (take listed-files changed))
              (< listed-files (count changed)) (conj (str "  and " (- (count changed) listed-files) " more")))))
