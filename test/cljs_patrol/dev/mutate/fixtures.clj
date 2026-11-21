(ns cljs-patrol.dev.mutate.fixtures
  "A target namespace and its tests loaded from text, and a temporary directory, for the mutation runner's tests."
  (:require
   [cljs-patrol.dev.mutate.run :as run]
   [clojure.java.io]))

(defn sample-ns
  "Create a namespace that refers clojure.core, as a file's ns form would."
  [ns-sym]
  (binding [*ns* (create-ns ns-sym)]
    (refer-clojure))
  ns-sym)

(def target-source
  (str "(ns cljs-patrol.dev.mutate-test.target)\n"
       "(defn ^:checked positive? [x] (> x 0))\n"
       "(defn label [x] (if (positive? x) :up :down))\n"
       "(defn unused [x] (when x 1))\n"))

(def target-tests
  (str "(ns cljs-patrol.dev.mutate-test.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate-test.target :as target]))\n"
       "(deftest labels (is (= :up (target/label 1))))\n"))

(defn with-target
  ([tests f] (with-target target-source tests f))
  ([source tests f]
   (sample-ns 'cljs-patrol.dev.mutate-test.target)
   (sample-ns 'cljs-patrol.dev.mutate-test.target-test)
   (try
     (run/load-text 'cljs-patrol.dev.mutate-test.target source)
     (run/load-text 'cljs-patrol.dev.mutate-test.target-test tests)
     (f)
     (finally
       (remove-ns 'cljs-patrol.dev.mutate-test.target)
       (remove-ns 'cljs-patrol.dev.mutate-test.target-test)))))

(defn with-temp-dir
  "Call `f` with a new temporary directory, and delete the directory after."
  [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "mutate" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (run! clojure.java.io/delete-file (reverse (file-seq dir)))))))
