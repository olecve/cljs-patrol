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

(defn slurp-file [path]
  #?(:clj (slurp path)
     :cljs (node-fs/readFileSync path "utf8")))

(defn source-file? [path]
  (or (str/ends-with? path ".cljs")
      (str/ends-with? path ".cljc")))

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
