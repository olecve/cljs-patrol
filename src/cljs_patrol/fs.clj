(ns cljs-patrol.fs
  "File system helpers used by the analyzer, baseline, and tests.

  The `Path` functions work on the file system of the `Path` or `FileSystem` they get, and throw on a nil path.
  The string functions are adapters on the default file system for callers that still use strings."
  (:require
   [clojure.string :as str])
  (:import
   [java.io IOException]
   [java.nio.file
    AtomicMoveNotSupportedException
    CopyOption
    DirectoryIteratorException
    FileSystem
    FileSystemException
    FileSystems
    FileVisitResult
    FileVisitor
    Files
    InvalidPathException
    LinkOption
    NoSuchFileException
    Path
    StandardCopyOption]
   [java.nio.file.attribute FileAttribute]))

(def ^:private no-link-options (make-array LinkOption 0))

(defn path
  "Make a `Path` on `file-system` from `first-part` and the `more` parts."
  ^Path [^FileSystem file-system ^String first-part & more]
  (.getPath file-system first-part (into-array String more)))

(defn resolve-path ^Path [^Path base ^String leaf]
  (.resolve base leaf))

(defn absolute ^Path [^Path p]
  (.toAbsolutePath p))

(defn absolute? [^Path p]
  (.isAbsolute p))

(defn parent
  "The parent of `p`, or nil when `p` has none."
  ^Path [^Path p]
  (.getParent p))

(defn exists? [^Path p]
  (Files/exists p no-link-options))

(defn create-dirs!
  "Create the folder `dir` and each missing folder above it.

  Do nothing when `dir` exists. Throw with the reason when a folder cannot be made."
  ^Path [^Path dir]
  (Files/createDirectories dir (make-array FileAttribute 0)))

(defn relative-path
  "Give `target` relative to `base` when `target` is under `base`, else `target` unchanged.

  Both are made absolute first, so a relative `target` is compared from the working directory of its file system."
  ^Path [^Path base ^Path target]
  (let [base-path (absolute base)
        target-path (absolute target)]
    (if (.startsWith target-path base-path)
      (.relativize base-path target-path)
      target)))

(defn delete-if-exists!
  "Delete the file or empty folder `p`, and give true when it was there."
  [^Path p]
  (Files/deleteIfExists p))

(defn move-over!
  "Move `source` onto `target`, and replace the file at `target`.

  The move is atomic when the file system can do it, so a reader never sees half of `target`.
  When it cannot, the move is a plain replace."
  ^Path [^Path source ^Path target]
  (try
    (Files/move source target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (catch AtomicMoveNotSupportedException _
      (Files/move source target (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))))

(defn- deleting-visitor
  "A visitor that deletes each file and each folder after its entries, and throws the first failure."
  ^FileVisitor []
  (reify FileVisitor
    (preVisitDirectory [_ _ _]
      FileVisitResult/CONTINUE)
    (visitFile [_ file _]
      (Files/delete file)
      FileVisitResult/CONTINUE)
    (visitFileFailed [_ _ exception]
      (throw exception))
    (postVisitDirectory [_ folder exception]
      (when exception
        (throw exception))
      (Files/delete folder)
      FileVisitResult/CONTINUE)))

(defn delete-recursively!
  "Delete `p` with all it holds, and do nothing when `p` is missing.

  The walk does not follow a link, so a link to a folder goes, and that folder stays."
  [^Path p]
  (when (exists? p)
    (Files/walkFileTree p (deleting-visitor))
    nil))

(defn- source-file? [^String path extensions]
  (boolean (some #(str/ends-with? path %) extensions)))

(def skipped-dirs
  "Directory names a scan never descends into.

  Build output holds copies of the sources, and a copy is reported a second time and goes
  on being reported after the original is fixed. `target/classes` is the one that bites,
  since an uberjar puts every `.clj` there, but a compiled `.cljs` tree is the same
  problem. A dot-directory is skipped whatever it is called."
  #{"target" "node_modules" "out"})

(defn- directory? [^Path p]
  (Files/isDirectory p no-link-options))

(defn- skipped-dir? [^Path p]
  (let [name (str (.getFileName p))]
    (and (directory? p)
         (or (contains? skipped-dirs name)
             (and (str/starts-with? name ".") (not (#{"." ".."} name)))))))

(defn- directory-entries [^Path dir]
  (with-open [stream (Files/newDirectoryStream dir)]
    (vec stream)))

(defn- failure-reason
  "The class name of `e`, with its reason when it has one.

  The message of a `FileSystemException` holds the path, so its reason comes from `getReason`."
  [^IOException e]
  (let [detail (if (instance? FileSystemException e)
                 (.getReason ^FileSystemException e)
                 (.getMessage e))]
    (cond-> (.getSimpleName (.getClass e))
      detail (str " (" detail ")"))))

(defn- warn-unreadable [dir ^IOException e]
  (binding [*out* *err*]
    (println (str "WARN: could not read folder " dir ", skipped it: " (failure-reason e)))))

(defn- children
  "The entries of the folder `dir`, in the order of the file system.

  A folder that cannot be read gives no entries and one warning on stderr, because one such folder must not stop a run."
  [^Path dir]
  (try
    (directory-entries dir)
    (catch IOException e
      (warn-unreadable dir e)
      [])
    (catch DirectoryIteratorException e
      (warn-unreadable dir (.getCause e))
      [])))

(defn- walk-files [^Path root]
  (if (skipped-dir? root)
    []
    (lazy-seq
     (if (directory? root)
       (mapcat walk-files (children root))
       [root]))))

(defn source-paths
  "Give every file under `root` that has one of `extensions`, in walk order.

  Which extensions count is the caller's, since the set is the union of what the enabled
  groups declare rather than a constant. `skipped-dirs` names what the walk does not enter.
  A link to a folder is followed."
  [^Path root extensions]
  (->> (walk-files root)
       (filter (fn [^Path p] (Files/isRegularFile p no-link-options)))
       (filterv #(source-file? (str %) extensions))))

(defn missing-paths
  "Give each of `paths` that does not exist.

  A scan of a missing path finds nothing and says nothing, so callers check first."
  [paths]
  (filterv (complement exists?) paths))

(defn- default-path
  "Make a `Path` from the string `p` on the default file system, or give nil when `p` names no file.

  The empty string names no file, as in `java.io`, though NIO reads it as the working directory.
  A string that the file system does not accept, such as one with a NUL character, names no file too."
  ^Path [^String p]
  (when-not (= "" p)
    (try
      (path (FileSystems/getDefault) p)
      (catch InvalidPathException _
        nil))))

(defn- existing-default-path
  "Make a `Path` as [[default-path]] does, and throw `NoSuchFileException` when `p` names no file."
  ^Path [^String p]
  (or (default-path p)
      (throw (NoSuchFileException. p))))

(defn absolute-path
  "The absolute form of `path`.

  A `path` that names no file comes back unchanged."
  [^String path]
  (if-let [p (default-path path)]
    (-> p absolute str)
    path))

(defn absolute-path? [^String path]
  (boolean (some-> (default-path path) absolute?)))

(defn parent-dir
  "Path of `path`'s parent directory, or nil when there is none."
  [^String path]
  (some-> (default-path path) parent str))

(defn file-exists? [^String path]
  (boolean (some-> (default-path path) exists?)))

(defn mkdirs
  "Create `dir` and any missing intermediate directories, and give true when `dir` was missing.

  Do nothing and give false when `dir` exists or names no file. Throw with the reason when a folder cannot be made."
  [^String dir]
  (if-let [p (default-path dir)]
    (let [missing? (not (exists? p))]
      (create-dirs! p)
      missing?)
    false))

(defn join-path
  "Combine `base` and `leaf` into a single path with the OS separator.

  As in `java.io`, an empty `base` stands for the root, and an absolute `leaf` goes under `base`."
  [^String base ^String leaf]
  (let [file-system (FileSystems/getDefault)]
    (str (path file-system (if (= "" base) (.getSeparator file-system) base) leaf))))

(defn relativize
  "Return `target` as a path relative to `base`.
  Falls back to the original `target` when it doesn't sit below `base`."
  [^String base ^String target]
  (let [base-path (some-> (default-path base) absolute)
        target-path (some-> (default-path target) absolute)]
    (if (and base-path target-path (.startsWith ^Path target-path ^Path base-path))
      (str (.relativize ^Path base-path ^Path target-path))
      target)))

(defn nano-time []
  (System/nanoTime))

(defn delete-file!
  "Delete the file or empty folder `path`, and give true when it was removed.

  Give false when `path` is missing, names no file, or cannot be deleted."
  [^String path]
  (if-let [p (default-path path)]
    (try
      (delete-if-exists! p)
      (catch IOException _
        false))
    false))

(defn move-replace!
  "Move `source` onto `target`, replacing any file already there.
  Atomic when the filesystem supports it, so `target` is never observed
  half-written; falls back to a plain replace when it does not."
  [^String source ^String target]
  (move-over! (existing-default-path source) (existing-default-path target)))

(defn delete-tree!
  "Recursively delete `path` if it exists.
  No-op when `path` is missing or names no file."
  [^String path]
  (some-> (default-path path) delete-recursively!))

(defn list-source-files
  "Recursively return every file under `root-dir` carrying one of `extensions`, as string paths.
  Which extensions count is the caller's, since the set is the union of what the enabled
  groups declare rather than a constant. `skipped-dirs` names what the walk does not enter."
  [^String root-dir extensions]
  (if-let [p (default-path root-dir)]
    (mapv str (source-paths p extensions))
    []))

(defn missing-dirs
  "Which of `paths` do not exist. A scan of one silently finds nothing, so callers check first."
  [paths]
  (remove file-exists? paths))
