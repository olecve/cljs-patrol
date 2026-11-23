(ns cljs-patrol.dev.mutate.guard
  "Find the files that the tests of a mutant add, change or delete in the working directory, which the runner cannot undo.

  The guard compares a snapshot of every file, its path, size and time of change, with the one
  taken after the baseline, after each mutant whose tests ran."
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.nio.file AccessDeniedException DirectoryIteratorException Files LinkOption NoSuchFileException Path]
   [java.nio.file.attribute BasicFileAttributes]))

(def tool-caches
  "Folders, relative to the working directory, that editors, linters and the Clojure CLI write while a run goes on."
  #{".lsp" ".clj-kondo/.cache" ".cpcache" ".shadow-cljs"})

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root-path ^Path [root]
  (.toPath (.getAbsoluteFile (io/file root))))

(defn skipped-dirs
  "Return the folders that a snapshot of `root` does not enter, relative to `root`.

  They are the `.git` folder of `root`, the caches of tools such as clojure-lsp and clj-kondo,
  and `cache-dir`, the folder of the run's cache, when it is under `root`."
  [root cache-dir]
  (let [base (.normalize (root-path root))
        cache-path (some-> cache-dir root-path .normalize)]
    (cond-> (conj tool-caches ".git")
      (and cache-path (.startsWith cache-path base) (not= cache-path base))
      (conj (str (.relativize base cache-path))))))

(defn editor-file?
  "True when a file named `file-name` is a swap, lock or backup file that an editor writes while a person edits.

  Vim writes `*.swp`, `*.swo` and, to test that it may write a folder, `4913`. Emacs writes `.#*`
  locks, and several editors write `*~` backups."
  [file-name]
  (or (str/ends-with? file-name ".swp")
      (str/ends-with? file-name ".swo")
      (str/ends-with? file-name "~")
      (str/starts-with? file-name ".#")
      (= "4913" file-name)))

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
          (when-not (or (contains? skipped relative) (editor-file? (str (.getFileName path))))
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

  The walk does not enter the `skipped` folders, given relative to `root`, and leaves out the
  swap, lock and backup files of editors. A link is a file of its own, so the walk does not follow it. A file
  that is deleted while the walk goes on is left out. A folder that cannot be read maps its
  path, with a `/` at the end, to :unreadable."
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

(defn start
  "Return the guard of a run in `root`, from the snapshots `before` and `after` the baseline, without the `skipped` folders.

  The guard keeps `after` as the reference, and the hash of the bytes of each file that the
  baseline added or changed, since its test writes it again for each mutant."
  [root skipped before after]
  {:root root
   :skipped skipped
   :files after
   :hashes (into {}
                 (keep (fn [[path change]] (when-not (= :deleted change) [path (content-hash root path)])))
                 (changes root before after {}))})

(defn look
  "Take a snapshot, and return the files added, changed or deleted since the baseline as [path change] pairs, or nil."
  [{:keys [root skipped files hashes]}]
  (not-empty (changes root files (snapshot root skipped) hashes)))

(def ^:private listed-files 10)

(defn describe
  "Name the first 10 `changed` files, each on a line of its own with its change, and say how many more there are.

  A changed file that the baseline wrote too says so, since a test that writes the same bytes
  each run does not stop the run, and this one wrote other bytes."
  [changed hashes]
  (str/join "\n"
            (cond-> (mapv (fn [[path change]]
                            (str "  " (name change) " " path
                                 (when (and (= :changed change) (contains? hashes path))
                                   " (the baseline wrote it too, and its bytes are not the same now)")))
                          (take listed-files changed))
              (< listed-files (count changed)) (conj (str "  and " (- (count changed) listed-files) " more")))))
