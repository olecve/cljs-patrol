(ns cljs-patrol.integration.baseline-test
  "Integration tests exercising the full baseline workflow against a fixture project."
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.reporters.console :as console]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test/projects/baseline-app/src/baseline_app")
(def ^:private enabled-groups [re-frame/group spade/group reagent/group])

(defn- run-analysis []
  (let [run-results [(core/run fixture-dir enabled-groups)]]
    {:run-results run-results
     :identities (baseline/collect-identities run-results)}))

(defn- tmp-dir []
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "cljs-patrol-integration-" (System/nanoTime)))]
    (.mkdirs dir)
    dir))

(defn- cleanup [dir]
  (run! #(.delete %) (reverse (file-seq dir))))

(deftest baseline-write-and-read-test
  (let [{:keys [identities]} (run-analysis)
        dir (tmp-dir)
        path (str (.getAbsolutePath dir) "/baseline.edn")]
    (try
      (baseline/write-baseline path identities)
      (let [{:keys [ok error]} (baseline/read-baseline path)]
        (is (nil? error))
        (is (= identities ok)))
      (finally
        (cleanup dir)))))

(deftest baseline-all-present-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline identities identities)]
    (is (empty? new)
        "no new issues when all are baselined")
    (is (= identities present))
    (is (empty? fixed))
    (is (not (core/baseline-failed? {} new fixed)))))

(deftest baseline-empty-baseline-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline #{} identities)]
    (is (= identities new)
        "everything is new against empty baseline")
    (is (empty? present))
    (is (empty? fixed))
    (is (core/baseline-failed? {} new fixed))))

(deftest baseline-detects-fixed-issues-test
  (let [{:keys [identities]} (run-analysis)
        extra {:rule :unused-subs
               :key :baseline-app.subs/removed-thing}
        baseline-with-extra (conj identities extra)
        {:keys [new present fixed]} (baseline/diff-baseline baseline-with-extra identities)]
    (is (empty? new))
    (is (= identities present))
    (is (= #{extra} fixed))
    (is (not (core/baseline-failed? {} new fixed))
        "non-strict does not fail on fixed")
    (is (core/baseline-failed? {:strict-baseline true} new fixed)
        "strict fails on fixed")))

(deftest baseline-expected-identities-test
  (let [{:keys [identities]} (run-analysis)]
    (is (contains? identities {:rule :unused-subs
                               :key :baseline-app.subs/old-dashboard})
        "unused sub")
    (is (contains? identities {:rule :unused-events
                               :key :baseline-app.events/legacy-reset})
        "unused event")
    (is (contains? identities {:rule :phantom-subs
                               :key :baseline-app.subs/deleted-feature})
        "phantom sub")
    (is (contains? identities {:rule :unused-styles
                               :ns "baseline-app.styles"
                               :var "legacy-panel"})
        "unused style")
    (is (contains? identities {:rule :defclass-as-sole-attr
                               :ns "baseline-app.styles"
                               :var "container"})
        "defclass-as-sole-attr")))

(deftest baseline-console-output-test
  (let [{:keys [run-results]} (run-analysis)
        out (with-out-str
              (doseq [{:keys [group-results]} run-results]
                (doseq [result group-results]
                  (console/report-with-baseline result #{}))))]
    (is (str/includes? out "[BASE]")
        "baseline issues tagged")
    (is (not (str/includes? out "[NEW]"))
        "no new issues when none match")))

(deftest baseline-console-quiet-test
  (let [{:keys [run-results]} (run-analysis)
        out (with-out-str
              (doseq [{:keys [group-results]} run-results]
                (doseq [result group-results]
                  (console/report-with-baseline result #{} true))))]
    (is (= "" out)
        "quiet mode suppresses all baseline issues")))

(deftest baseline-edn-output-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline identities identities)
        out (with-out-str
              (edn-reporter/print-baseline-report [fixture-dir] new present fixed 0))
        parsed (edn/read-string out)]
    (is (= 0 (:exit-code parsed)))
    (is (empty? (:new-issues parsed)))
    (is (seq (:baseline-issues parsed)))
    (is (empty? (:fixed-issues parsed)))))

(deftest baseline-identities-use-relative-paths-test
  (let [absolute-dir (.getAbsolutePath (io/file fixture-dir))
        run-results [(core/run absolute-dir enabled-groups)]
        identities (baseline/collect-identities run-results)
        ids-with-file (filter :file identities)]
    (is (seq ids-with-file)
        "some identities have file paths")
    (doseq [id ids-with-file]
      (is (not (.isAbsolute (io/file (:file id))))
          (str "path should not be absolute, got: " (:file id)))
      (is (not (re-find #"\.\." (:file id)))
          (str "path should not contain .., got: " (:file id))))))

(deftest baseline-round-trip-with-absolute-source-dir-test
  (let [absolute-dir (.getAbsolutePath (io/file fixture-dir))
        run-results [(core/run absolute-dir enabled-groups)]
        identities (baseline/collect-identities run-results)
        dir (tmp-dir)
        path (str (.getAbsolutePath dir) "/baseline.edn")]
    (try
      (baseline/write-baseline path identities)
      (let [{:keys [ok]} (baseline/read-baseline path)
            rerun-results [(core/run absolute-dir enabled-groups)]
            rerun-identities (baseline/collect-identities rerun-results)
            {:keys [new fixed]} (baseline/diff-baseline ok rerun-identities)]
        (is (empty? new)
            "no new issues on re-analysis with absolute path")
        (is (empty? fixed)
            "no fixed issues on re-analysis with absolute path"))
      (finally
        (cleanup dir)))))

(deftest baseline-round-trip-survives-reanalysis-test
  (let [{:keys [identities]} (run-analysis)
        dir (tmp-dir)
        path (str (.getAbsolutePath dir) "/baseline.edn")]
    (try
      (baseline/write-baseline path identities)
      (let [{:keys [ok]} (baseline/read-baseline path)
            {reanalyzed :identities} (run-analysis)
            {:keys [new present fixed]} (baseline/diff-baseline ok reanalyzed)]
        (is (empty? new)
            "re-analysis produces no new issues")
        (is (= reanalyzed present))
        (is (empty? fixed)))
      (finally
        (cleanup dir)))))
