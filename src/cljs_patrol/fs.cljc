(ns cljs-patrol.fs
  "Filesystem helpers shared by core and the reporters.
  JVM implementation uses java.io; Node implementation uses fs / path."
  (:require
   [clojure.string :as str])
  #?(:cljs (:require ["fs" :as node-fs]
                     ["path" :as node-path]))
  #?(:clj (:import [java.io File])))

(defn absolute-path [path]
  #?(:clj (.getAbsolutePath (File. ^String path))
     :cljs (node-path/resolve path)))

(defn parent-dir
  "Return the parent directory of `path`, or nil when there is none."
  [path]
  #?(:clj (some-> (File. ^String path) .getParentFile .getPath)
     :cljs (let [p (node-path/dirname path)]
             (when (not= p path) p))))

(defn file-exists? [path]
  #?(:clj (.exists (File. ^String path))
     :cljs (node-fs/existsSync path)))

(defn mkdirs
  "Create `dir` and any missing intermediate directories. No-op if present."
  [dir]
  #?(:clj (.mkdirs (File. ^String dir))
     :cljs (node-fs/mkdirSync dir #js {:recursive true})))

(defn slurp-file [path]
  #?(:clj (slurp path)
     :cljs (node-fs/readFileSync path "utf8")))

(defn spit-file [path content]
  #?(:clj (spit path content)
     :cljs (node-fs/writeFileSync path content)))

(defn relativize
  "Return `target` expressed relative to `base`, or the original `target`
  when it doesn't sit below `base`."
  [base target]
  #?(:clj (let [base-p (-> base File. .getAbsoluteFile .toPath)
                targ-p (-> target File. .getAbsoluteFile .toPath)]
            (if (.startsWith targ-p base-p)
              (str (.relativize base-p targ-p))
              target))
     :cljs (let [base-abs (node-path/resolve base)
                 targ-abs (node-path/resolve target)]
             (if (or (= targ-abs base-abs)
                     (str/starts-with? targ-abs (str base-abs node-path/sep)))
               (node-path/relative base-abs targ-abs)
               target))))

(defn join-path [base leaf]
  #?(:clj (str (File. ^String base ^String leaf))
     :cljs (node-path/join base leaf)))

(defn source-file? [path]
  (or (str/ends-with? path ".cljs")
      (str/ends-with? path ".cljc")))

(defn tmp-dir
  "Return the OS temporary directory path."
  []
  #?(:clj (System/getProperty "java.io.tmpdir")
     :cljs (node-fs/realpathSync (or (.-TMPDIR (.-env js/process))
                                     (.-TMP (.-env js/process))
                                     "/tmp"))))

(defn nano-time
  "Return a monotonically increasing counter, used for unique names."
  []
  #?(:clj (System/nanoTime)
     :cljs (js/Date.now)))

(defn tmp-file-path
  "Return a unique path under `tmp-dir` for `prefix<random>suffix`.
  Does not create the file — caller writes to it."
  [prefix suffix]
  (let [name (str prefix (nano-time) suffix)]
    #?(:clj (str (java.io.File. ^String (tmp-dir) ^String name))
       :cljs (node-path/join (tmp-dir) name))))

(defn delete-tree!
  "Recursively delete `path` if it exists. No-op otherwise."
  [path]
  #?(:clj (let [f (java.io.File. ^String path)]
            (when (.exists f)
              (run! #(.delete ^java.io.File %) (reverse (file-seq f)))))
     :cljs (when (node-fs/existsSync path)
             (node-fs/rmSync path #js {:recursive true :force true}))))

(defn absolute-path?
  "True when `path` is already absolute."
  [path]
  #?(:clj (.isAbsolute (File. ^String path))
     :cljs (node-path/isAbsolute path)))

(defn list-source-files [root-dir]
  #?(:clj (->> (file-seq (File. ^String root-dir))
               (filter (fn [^File f] (.isFile f)))
               (map (fn [^File f] (.getPath f)))
               (filter source-file?)
               vec)
     :cljs (letfn [(walk [dir]
                     (mapcat (fn [entry]
                               (let [full (node-path/join dir entry)]
                                 (if (.isDirectory (node-fs/statSync full))
                                   (walk full)
                                   [full])))
                             (node-fs/readdirSync dir)))]
             (vec (filter source-file? (walk root-dir))))))
