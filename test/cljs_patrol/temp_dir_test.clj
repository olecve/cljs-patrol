(ns cljs-patrol.temp-dir-test
  (:require
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(defn- with-link
  "Make `link` point to the folder `target`, and call `f`, unless the file system has no symbolic links."
  [link target f]
  (when (try
          (java.nio.file.Files/createSymbolicLink (.toPath (io/file link))
                                                  (.toPath (io/file target))
                                                  (make-array java.nio.file.attribute.FileAttribute 0))
          (catch UnsupportedOperationException _ nil)
          (catch java.io.IOException _ nil))
    (f)))

(deftest delete-dir-does-not-follow-link-to-folder-outside
  (temp-dir/with-dir "outside-"
    (fn [outside]
      (spit (io/file outside "kept.txt") "x")
      (temp-dir/with-dir "inside-"
        (fn [dir]
          (spit (io/file dir "a.txt") "x")
          (with-link (io/file dir "link") outside
            (fn []
              (temp-dir/delete-dir! dir)
              (is (not (.exists (io/file dir))))
              (is (.exists (io/file outside "kept.txt"))
                  "folder behind link keeps its files")
              (temp-dir/delete-dir! dir)
              (is (not (.exists (io/file dir)))
                  "folder that is gone is no error"))))))))

(deftest with-dir-deletes-folder-after-call
  (let [seen (atom nil)]
    (temp-dir/with-dir "with-"
      (fn [dir]
        (reset! seen dir)
        (spit (io/file dir "a.txt") "x")))
    (is (not (.exists ^java.io.File @seen)))))

(deftest delete-created-deletes-folders-of-file-path
  (let [path (atom nil)]
    (temp-dir/delete-created #(do (reset! path (temp-dir/file-path "a.txt"))
                                  (spit @path "x")))
    (is (not (.exists (.getParentFile (io/file @path)))))))
