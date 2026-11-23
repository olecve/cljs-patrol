(ns cljs-patrol.dev.mutate.guard
  "Find the files that a mutant adds, changes or deletes in the working directory, which the runner cannot undo.

  The tests of a mutant can change files, and so can its form while it loads. After each mutant
  that loaded, the guard compares a snapshot of every file and folder, with the size and time of
  change of each file, with the reference: the last snapshot with no change, which is the one
  after the baseline at first. The snapshot skips what git ignores."
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.git :as git]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io IOException]
   [java.nio.file AccessDeniedException DirectoryIteratorException Files LinkOption NoSuchFileException Path]
   [java.nio.file.attribute BasicFileAttributes]))

(def editor-files
  "What editors, IDEs, their language servers, macOS and nREPL write while a run goes on, which a snapshot leaves out.

  The person's own setup writes them, so the project's `.gitignore` may not name them, and git
  may not ignore them. :folders are relative to the working directory: those of clojure-lsp,
  clj-kondo, IntelliJ and Calva. :names match the name of a file or folder at any depth: the
  swap files of Vim (`*.swp`, `*.swo`, and `4913`, which Vim writes to test that it may write a
  folder), the locks (`.#*`) and auto-saves (`#*#`) of Emacs, the backups of several editors
  (`*~`), the folder files of macOS (`.DS_Store`), and the port file of nREPL."
  {:folders #{".lsp" ".clj-kondo/.cache" ".clj-kondo/imports" ".clj-kondo/inline-configs" ".idea" ".calva"}
   :names [#"\.sw[po]$" #"^4913$" #"^\.#" #"^#.*#$" #"~$" #"^\.DS_Store$" #"^\.nrepl-port$"]})

(def build-folders
  "The folders of the Clojure CLI and shadow-cljs, which a snapshot leaves out when git cannot tell what it ignores."
  #{".cpcache" ".shadow-cljs"})

(def no-git-note
  "Note: git cannot tell which files it ignores here, so the file guard also checks them, such as target/.")

(defn tool-file?
  "True when `file-name` is the name of a file or folder that editors and tools write, at any depth."
  [file-name]
  (boolean (some #(re-find % file-name) (:names editor-files))))

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root-path ^Path [root]
  (.toPath (.getAbsoluteFile (io/file root))))

(defn skipped-dirs
  "Return the folders that a snapshot of `root` never enters, relative to `root`.

  They are the `.git` folder of `root`, and `cache-dir`, the folder of the run's cache, when it
  is under `root`."
  [root cache-dir]
  (let [base (.normalize (root-path root))
        cache-path (some-> cache-dir root-path .normalize)]
    (cond-> #{".git"}
      (and cache-path (.startsWith cache-path base) (not= cache-path base))
      (conj (str (.relativize base cache-path))))))

(defn- bare
  "Return `path` without the `/` at the end that marks a folder."
  [path]
  (str/replace path #"/$" ""))

(defn with-ignored
  "Add to `skipped` the paths that git ignores, `ignored`, and the folders of editors.

  `ignored` holds paths relative to the root of the snapshot, and a folder ends with `/`. When
  `ignored` is nil, since git cannot tell, add the folders of build tools instead."
  [skipped ignored]
  (into skipped
        (concat (:folders editor-files)
                (if ignored
                  (map bare ignored)
                  build-folders))))

(defn- parent-folders
  "Return the folders above relative `path`, from the top, such as `a` and `a/b` for `a/b/c`."
  [path]
  (butlast (reductions #(str %1 "/" %2) (str/split path #"/"))))

(defn- holding-folders
  "Return the folders that hold a path of `skipped`."
  [skipped]
  (into #{} (mapcat parent-folders) skipped))

(defn file-attributes
  "Return the attributes of the file or folder at `path`, not following a link, or nil when it is gone."
  ^BasicFileAttributes [^Path path]
  (try
    (Files/readAttributes path BasicFileAttributes ^"[Ljava.nio.file.LinkOption;" no-follow)
    (catch NoSuchFileException _ nil)))

(defn- walk [^Path root skipped holders ^Path dir ^java.util.Map files]
  (try
    (with-open [entries (Files/newDirectoryStream dir)]
      (doseq [^Path path entries]
        (let [relative (str (.relativize root path))]
          (when-not (or (contains? skipped relative) (tool-file? (str (.getFileName path))))
            (when-let [attributes (file-attributes path)]
              (if (.isDirectory attributes)
                (do (when-not (contains? holders relative)
                      (.put files (str relative "/") :folder))
                    (walk root skipped holders path files))
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
  does not; a folder that cannot be read maps to :unreadable. The walk leaves out the `skipped`
  paths, given relative to `root`, with all that a skipped folder holds, and the files that
  tools write. A folder that holds a skipped path, such as `.clj-kondo/`, maps to nothing
  itself, since a tool can make it, but its other files count. A link is a file of its own, so
  the walk does not follow it. A file that is deleted while the walk goes on is left out."
  ([root skipped] (snapshot root skipped (holding-folders skipped)))
  ([root skipped holders]
   (let [path (root-path root)
         files (java.util.HashMap.)]
     (walk path skipped holders path files)
     (into {} files))))

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
  "Return the guard of a run in `root`, from the snapshots `before` and `after` the baseline, without the `skipped` paths.

  The guard keeps `after` as the reference, and the hash of the bytes of each file that the
  baseline added or changed, since its test writes it again for each mutant. A file whose bytes
  cannot be read gets no hash, so any change of it counts."
  [root skipped before after]
  {:root root
   :skipped (atom skipped)
   :holders (atom (holding-folders skipped))
   :files (atom after)
   :hashes (into {}
                 (keep (fn [[path change]]
                         (when-not (= :deleted change)
                           (some->> (content-hash root path) (vector path)))))
                 (changes root before after {}))})

(defn- newly-ignored
  "Return the added paths of `changed` that git ignores, without the paths in a folder among them."
  [root changed]
  (let [ignored (->> changed
                     (keep (fn [[path change]] (when (= :added change) path)))
                     (git/ignored-among root)
                     (into #{} (map bare)))]
    (into #{} (remove #(some ignored (parent-folders %))) ignored)))

(defn look
  "Take a snapshot, and return the files added, changed or deleted since the reference as [path change] pairs, or nil.

  The reference is the last snapshot with no change, which is the one after the baseline at
  first. When nothing changed, the new snapshot becomes the reference, so a file that the
  baseline wrote and a test wrote again with the same bytes is not hashed again. A new path that
  git ignores is no change, and neither is a new folder that holds one: the guard skips them,
  drops the folders that now hold a skipped path from the reference, and looks again."
  [{:keys [root skipped holders files hashes]
    :as guard}]
  (let [now (snapshot root @skipped @holders)
        changed (changes root @files now hashes)
        ignored (when (some #(= :added (second %)) changed)
                  (newly-ignored root changed))]
    (cond
      (seq ignored) (do (swap! skipped into ignored)
                        (reset! holders (holding-folders @skipped))
                        (swap! files #(apply dissoc % (map (fn [folder] (str folder "/")) (holding-folders ignored))))
                        (look guard))
      (seq changed) changed
      :else (do (reset! files now)
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
