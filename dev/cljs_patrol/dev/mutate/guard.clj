(ns cljs-patrol.dev.mutate.guard
  "Find the files that a mutant adds, changes or deletes in the working directory, which the runner cannot undo.

  The tests of a mutant can change files, and so can its form while it loads. After each mutant
  that loaded, the guard compares a snapshot of every file and folder, with the size and time of
  change of each file, with the reference: the last snapshot with no change, which is the one
  after the baseline at first."
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io IOException]
   [java.nio.file AccessDeniedException DirectoryIteratorException Files LinkOption NoSuchFileException Path]
   [java.nio.file.attribute BasicFileAttributes]))

(def tool-files
  "What editors, IDEs, linters and the Clojure CLI write while a run goes on, which a snapshot leaves out.

  :folders are relative to the working directory. :names match the name of a file or folder at
  any depth: the swap files of Vim (`*.swp`, `*.swo`, and `4913`, which Vim writes to test that
  it may write a folder), the locks (`.#*`) and auto-saves (`#*#`) of Emacs, the backups of
  several editors (`*~`), the folder files of macOS (`.DS_Store`), and the port file of nREPL."
  {:folders #{".lsp" ".clj-kondo/.cache" ".cpcache" ".shadow-cljs" ".idea" ".calva"}
   :names [#"\.sw[po]$" #"^4913$" #"^\.#" #"^#.*#$" #"~$" #"^\.DS_Store$" #"^\.nrepl-port$"]})

(def ^:private new-kondo-folder
  "The folder that clj-kondo makes when it first lints the project, which counts as no change while the reference has none."
  ".clj-kondo/")

(defn tool-file?
  "True when `file-name` is the name of a file or folder that editors and tools write, at any depth."
  [file-name]
  (boolean (some #(re-find % file-name) (:names tool-files))))

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root-path ^Path [root]
  (.toPath (.getAbsoluteFile (io/file root))))

(defn skipped-dirs
  "Return the folders that a snapshot of `root` does not enter, relative to `root`.

  They are the `.git` folder of `root`, the folders of tools, and `cache-dir`, the folder of
  the run's cache, when it is under `root`."
  [root cache-dir]
  (let [base (.normalize (root-path root))
        cache-path (some-> cache-dir root-path .normalize)]
    (cond-> (conj (:folders tool-files) ".git")
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
          (when-not (or (contains? skipped relative) (tool-file? (str (.getFileName path))))
            (when-let [attributes (file-attributes path)]
              (if (.isDirectory attributes)
                (do (.put files (str relative "/") :folder)
                    (walk root skipped path files))
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
  "Map the path of each file and folder under `root`, relative to it, to what a later snapshot compares.

  A file maps to its size and time of change. A folder maps its path, with a `/` at the end, to
  :folder, so a new or deleted folder counts, an empty one too, but a folder whose files change
  does not; a folder that cannot be read maps to :unreadable. The walk does not enter the
  `skipped` folders, given relative to `root`, and leaves out the files that tools write. A link
  is a file of its own, so the walk does not follow it. A file that is deleted while the walk goes
  on is left out."
  [root skipped]
  (let [path (root-path root)
        files (java.util.HashMap.)]
    (walk path skipped path files)
    (into {} files)))

(defn- content-hash
  "Hash the bytes of the file at `path` under `root`, or return nil when it cannot be read, such as a folder."
  [root path]
  (try
    (-> root root-path (.resolve ^String path) Files/readAllBytes cache/bytes-hash)
    (catch IOException _ nil)))

(defn changes
  "Return the files that snapshot `after` adds, changes or deletes against `before`, sorted, as [path change] pairs.

  A change is :added, :changed or :deleted. A file in `hashes`, path to the hash of its bytes,
  whose time alone changed is unchanged while its bytes hash the same, since a test that wrote
  it in the baseline writes it again for each mutant. A file whose bytes cannot be read is
  changed."
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
                     (and (vector? old) (vector? new) (= (first old) (first new)) (contains? hashes path)
                          (= (get hashes path) (content-hash root path))) nil
                     :else [path :changed]))))
         vec)))

(defn start
  "Return the guard of a run in `root`, from the snapshots `before` and `after` the baseline, without the `skipped` folders.

  The guard keeps `after` as the reference, and the hash of the bytes of each file that the
  baseline added or changed, since its test writes it again for each mutant. A file whose bytes
  cannot be read gets no hash, so any change of it counts."
  [root skipped before after]
  {:root root
   :skipped skipped
   :files (atom after)
   :hashes (into {}
                 (keep (fn [[path change]]
                         (when-not (= :deleted change)
                           (some->> (content-hash root path) (vector path)))))
                 (changes root before after {}))})

(defn look
  "Take a snapshot, and return the files added, changed or deleted since the reference as [path change] pairs, or nil.

  The reference is the last snapshot with no change, which is the one after the baseline at
  first. When nothing changed, the new snapshot becomes the reference, so a file that the
  baseline wrote and a test wrote again with the same bytes is not hashed again. A `.clj-kondo/`
  folder that the reference does not have counts as no change, since clj-kondo makes it when it
  first lints the project; once the reference has it, its files count."
  [{:keys [root skipped files hashes]}]
  (let [now (snapshot root skipped)
        changed (cond->> (changes root @files now hashes)
                  (not (contains? @files new-kondo-folder))
                  (remove #(str/starts-with? (first %) new-kondo-folder)))]
    (if (seq changed)
      changed
      (do (reset! files now)
          nil))))

(def ^:private listed-files 10)

(defn describe
  "Name the first 10 `changed` paths, files before folders, each on a line of its own with its change, and count the rest.

  A changed file that the baseline wrote too says so, since a test that writes the same bytes
  each run does not stop the run, and this one wrote other bytes."
  [changed hashes]
  (str/join "\n"
            (cond-> (->> changed
                         (sort-by #(str/ends-with? (first %) "/"))
                         (take listed-files)
                         (mapv (fn [[path change]]
                                 (str "  " (name change) " " path
                                      (when (and (= :changed change) (contains? hashes path))
                                        " (the baseline wrote it too, and its bytes are not the same now)")))))
              (< listed-files (count changed)) (conj (str "  and " (- (count changed) listed-files) " more")))))
