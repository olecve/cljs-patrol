(ns cljs-patrol.integration.timings-test
  "Runs the CLI in a JVM of its own, since it exits, to compare what it prints with and without --timings."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-src "test-projects/a11y-app/src")

(defn- run-cli [& args]
  (apply sh "java" "-cp" (System/getProperty "java.class.path") "clojure.main" "-m" "cljs-patrol.core" args))

(defn- compare-runs [args source-dir]
  {:plain (apply run-cli (concat args [source-dir]))
   :timed (apply run-cli (concat args ["--timings" source-dir]))})

(defn- phase-rows [err]
  (->> (str/split-lines err)
       (keep #(second (re-matches #"  (discovery|parse|walk|analyze|report|total) .*" %)))
       vec))

(deftest timings-leave-console-stdout-unchanged
  (let [{:keys [plain timed]} (compare-runs ["--only" "a11y,tests"] fixture-src)]
    (is (= (:out plain) (:out timed))
        "stdout byte-identical")
    (is (= (:exit plain) (:exit timed))
        "exit code unchanged")
    (is (not (str/includes? (:err plain) "Timings"))
        "no timings without flag")
    (is (= ["discovery" "parse" "walk" "analyze" "report" "total"] (phase-rows (:err timed)))
        "every phase on stderr, in run order")
    (is (str/includes? (:err timed) "slowest files")
        "slowest files listed")))

(deftest timings-leave-edn-stdout-unchanged
  (let [{:keys [plain timed]} (compare-runs ["--output" "edn"] fixture-src)]
    (is (= (:out plain) (:out timed))
        "stdout byte-identical")
    (is (str/includes? (:err timed) "  a11y ")
        "group rows on stderr")))

(deftest timings-leave-baseline-stdout-unchanged
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "cljs-patrol" (make-array java.nio.file.attribute.FileAttribute 0)))
        source-dir (str dir)]
    (try
      (doseq [^java.io.File file (file-seq (io/file fixture-src))
              :when (.isFile file)]
        (let [target (io/file dir (-> (io/file fixture-src) .toPath (.relativize (.toPath file)) str))]
          (io/make-parents target)
          (io/copy file target)))
      (let [written (run-cli "--only" "a11y" "--baseline-write" "--timings" source-dir)
            {:keys [plain timed]} (compare-runs ["--only" "a11y" "--baseline"] source-dir)]
        (is (zero? (:exit written))
            "baseline written")
        (is (str/includes? (:err written) "Timings")
            "baseline write prints timings")
        (is (= (:out plain) (:out timed))
            "stdout byte-identical")
        (is (str/includes? (:err timed) "Timings")
            "baseline comparison prints timings"))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file))))))

(deftest timings-leave-error-exit-unchanged
  (let [{:keys [plain timed]} (compare-runs ["--only" "a11y" "--baseline"] fixture-src)]
    (is (str/includes? (:out plain) "Baseline file not found")
        "run stops on missing baseline")
    (is (= (:out plain) (:out timed))
        "stdout byte-identical")
    (is (= 1 (:exit plain) (:exit timed))
        "exit code 1 with and without flag")
    (is (= ["discovery" "parse" "walk" "analyze" "report" "total"] (phase-rows (:err timed)))
        "timings on stderr after error")))
