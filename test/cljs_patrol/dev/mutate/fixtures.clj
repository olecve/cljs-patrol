(ns cljs-patrol.dev.mutate.fixtures
  "A target namespace and its tests loaded from text, and a temporary directory, for the mutation runner's tests."
  (:require
   [cljs-patrol.dev.mutate.run :as run]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]))

(defn sample-ns
  "Create a namespace that refers clojure.core, as a file's ns form would."
  [ns-sym]
  (binding [*ns* (create-ns ns-sym)]
    (refer-clojure))
  ns-sym)

(def target-source
  (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
       "(defn ^:checked positive? [x] (> x 0))\n"
       "(defn label [x] (if (positive? x) :up :down))\n"
       "(defn unused [x] (when x 1))\n"))

(def target-tests
  (str "(ns cljs-patrol.dev.mutate.fixtures.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate.fixtures.target :as target]))\n"
       "(deftest labels (is (= :up (target/label 1))))\n"))

(defn with-target
  ([tests f] (with-target target-source tests f))
  ([source tests f]
   (sample-ns 'cljs-patrol.dev.mutate.fixtures.target)
   (sample-ns 'cljs-patrol.dev.mutate.fixtures.target-test)
   (try
     (run/load-text 'cljs-patrol.dev.mutate.fixtures.target source)
     (run/load-text 'cljs-patrol.dev.mutate.fixtures.target-test tests)
     (f)
     (finally
       (remove-ns 'cljs-patrol.dev.mutate.fixtures.target)
       (remove-ns 'cljs-patrol.dev.mutate.fixtures.target-test)))))

(defn with-temp-dir
  "Call `f` with a new temporary directory, and delete the directory after."
  [f]
  (temp-dir/with-dir "mutate" f))

(defn git-repo!
  "Make `dir` a git work tree whose `.gitignore` holds `ignore-lines`, and no ignore file of the person counts."
  [dir & ignore-lines]
  (let [{:keys [exit err]} (shell/sh "git" "-C" (str dir) "init" "-q")]
    (assert (zero? exit) err))
  (shell/sh "git" "-C" (str dir) "config" "core.excludesFile" (str (io/file dir "no-such-ignore-file")))
  (spit (io/file dir ".gitignore") (apply str (map #(str % "\n") ignore-lines))))
