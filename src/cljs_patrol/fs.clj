(ns cljs-patrol.fs
  "Filesystem helpers used by the analyzer, baseline, and tests.
  Thin wrappers around java.io so call sites stay uniform and grep-able."
  (:require
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file AtomicMoveNotSupportedException CopyOption Files Path StandardCopyOption]))

(defn absolute-path [^String path]
  (.getAbsolutePath (File. path)))

(defn absolute-path? [^String path]
  (.isAbsolute (File. path)))

(defn parent-dir
  "Path of `path`'s parent directory, or nil when there is none."
  [^String path]
  (some-> (File. path) .getParentFile .getPath))

(defn file-exists? [^String path]
  (.exists (File. path)))

(defn mkdirs
  "Create `dir` and any missing intermediate directories.
  No-op if `dir` already exists."
  [^String dir]
  (.mkdirs (File. dir)))

(defn join-path
  "Combine `base` and `leaf` into a single path with the OS separator."
  [^String base ^String leaf]
  (.getPath (File. base leaf)))

(defn relativize
  "Return `target` as a path relative to `base`.
  Falls back to the original `target` when it doesn't sit below `base`."
  [^String base ^String target]
  (let [base-p (-> base File. .getAbsoluteFile .toPath)
        targ-p (-> target File. .getAbsoluteFile .toPath)]
    (if (.startsWith targ-p base-p)
      (str (.relativize base-p targ-p))
      target)))

(defn tmp-dir []
  (System/getProperty "java.io.tmpdir"))

(defn nano-time []
  (System/nanoTime))

(defn tmp-file-path
  "Return a unique path under the OS temp dir formatted as `prefix<time>suffix`.
  Does not create the file; caller writes to it."
  [prefix suffix]
  (join-path (tmp-dir) (str prefix (nano-time) suffix)))

(defn- as-path ^Path [^String path]
  (.toPath (File. path)))

(defn delete-file!
  "Delete `path`, returning true when a file was actually removed.
  No-op when `path` is missing."
  [^String path]
  (.delete (File. path)))

(defn move-replace!
  "Move `source` onto `target`, replacing any file already there.
  Atomic when the filesystem supports it, so `target` is never observed
  half-written; falls back to a plain replace when it does not."
  [^String source ^String target]
  (let [src (as-path source)
        dst (as-path target)]
    (try
      (Files/move src dst (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
      (catch AtomicMoveNotSupportedException _
        (Files/move src dst (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))))

(defn delete-tree!
  "Recursively delete `path` if it exists.
  No-op when `path` is missing."
  [^String path]
  (let [f (File. path)]
    (when (.exists f)
      (run! #(.delete ^File %) (reverse (file-seq f))))))

(defn source-file? [^String path]
  (or (str/ends-with? path ".cljs")
      (str/ends-with? path ".cljc")))

(defn list-source-files
  "Recursively return every .cljs/.cljc file under `root-dir` as string paths."
  [^String root-dir]
  (->> (file-seq (File. root-dir))
       (filter (fn [^File f] (.isFile f)))
       (map (fn [^File f] (.getPath f)))
       (filter source-file?)
       vec))
