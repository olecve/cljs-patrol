(ns cljs-patrol.integration.jobs-test
  "Runs the CLI in a JVM of its own, since it exits, to compare what it prints on one thread and on several."
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dirs
  ["test-projects/a11y-app/src" "test-projects/clj-app" "test-projects/re-frame-spade-app/src"])

(defn- run-cli [& args]
  (apply sh "java" "-cp" (System/getProperty "java.class.path") "clojure.main" "-m" "cljs-patrol.core" args))

(deftest jobs-leave-output-unchanged
  (doseq [output [[] ["--output" "edn"]]]
    (let [args (concat ["--experimental-clj"] output fixture-dirs)
          sequential (apply run-cli "--jobs" "1" args)
          parallel (apply run-cli "--jobs" "4" args)]
      (is (str/includes? (:out sequential) "test-projects/a11y-app/src")
          (str "findings printed for " (pr-str output)))
      (is (= sequential parallel)
          (str "stdout, stderr and exit identical for " (pr-str output))))))

(deftest invalid-jobs-exit-with-error
  (let [{:keys [exit out]} (run-cli "--jobs" "0" "test-projects/a11y-app/src")]
    (is (= 1 exit))
    (is (= "Failed to validate \"--jobs 0\": Must be a positive integer\n" out)
        "same message shape as other bad arguments")))

(deftest help-lists-jobs
  (is (str/includes? (:out (run-cli "--help")) "--jobs N")))
