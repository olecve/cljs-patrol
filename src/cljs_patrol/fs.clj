(ns cljs-patrol.fs
  "Filesystem helpers shared by core and the reporters."
  (:import
   [java.io File]))

(defn absolute-path [^String path]
  (.getAbsolutePath (File. path)))
