(ns cljs-patrol.temp-dir-test
  (:require
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(deftest delete-dir-does-not-follow-link-to-folder-outside
  (temp-dir/with-dir "outside-"
    (fn [outside]
      (spit (io/file outside "kept.txt") "x")
      (let [dir (temp-dir/create-dir "inside-")]
        (spit (io/file dir "a.txt") "x")
        (java.nio.file.Files/createSymbolicLink (.toPath (io/file dir "link"))
                                                (.toPath outside)
                                                (make-array java.nio.file.attribute.FileAttribute 0))
        (temp-dir/delete-dir! dir)
        (is (not (.exists (io/file dir))))
        (is (.exists (io/file outside "kept.txt"))
            "folder behind link keeps its files")
        (temp-dir/delete-dir! dir)
        (is (not (.exists (io/file dir)))
            "folder that is gone is no error")))))

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
