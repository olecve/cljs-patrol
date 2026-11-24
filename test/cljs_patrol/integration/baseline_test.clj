(ns cljs-patrol.integration.baseline-test
  "Integration tests exercising the full baseline workflow against a fixture project."
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.memory-fs :as memory-fs]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test-projects/baseline-app/src/baseline_app")
(def ^:private enabled-groups [re-frame/group spade/group reagent/group])

(defn- run-analysis []
  (let [run-results [(core/run (disk/typed-path fixture-dir) enabled-groups)]]
    {:run-results run-results
     :identities (baseline/collect-identities run-results)}))

(defn- with-baseline-path
  "Call `f` with the `Path` of a baseline file that does not exist yet, on a new in-memory file system."
  [f]
  (with-open [file-system (memory-fs/unix)]
    (f (memory-fs/path file-system "/work/baseline.edn"))))

(deftest baseline-write-and-read-test
  (let [{:keys [identities]} (run-analysis)]
    (with-baseline-path
      (fn [path]
        (baseline/write-baseline path identities)
        (let [{:keys [ok error]} (baseline/read-baseline (memory-fs/typed path))]
          (is (nil? error))
          (is (= identities ok)))))))

(deftest baseline-all-present-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline identities identities)]
    (is (empty? new)
        "no new issues when all are baselined")
    (is (= identities present))
    (is (empty? fixed))
    (is (not (core/baseline-failed? {:new-issues new
                                     :fixed-issues fixed})))))

(deftest baseline-empty-baseline-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline #{} identities)]
    (is (= identities new)
        "everything is new against empty baseline")
    (is (empty? present))
    (is (empty? fixed))
    (is (core/baseline-failed? {:new-issues new
                                :fixed-issues fixed}))))

(deftest baseline-detects-fixed-issues-test
  (let [{:keys [identities]} (run-analysis)
        extra {:rule :unused-subs
               :key :baseline-app.subs/removed-thing}
        baseline-with-extra (conj identities extra)
        {:keys [new present fixed]} (baseline/diff-baseline baseline-with-extra identities)]
    (is (empty? new))
    (is (= identities present))
    (is (= #{extra} fixed))
    (is (not (core/baseline-failed? {:new-issues new
                                     :fixed-issues fixed}))
        "non-strict does not fail on fixed")
    (is (core/baseline-failed? {:new-issues new
                                :fixed-issues fixed
                                :strict-baseline true})
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
        {:keys [findings present]} (core/baseline-console-rows enabled-groups run-results #{} nil false)]
    (is (= #{"baseline"} (set (map :severity findings)))
        "every finding labeled baseline when none is new")
    (is (= (count findings) present)
        "baseline count counts rows, as listed")))

(deftest baseline-console-count-test
  (let [{:keys [run-results identities]} (run-analysis)
        {:keys [findings present]} (core/baseline-console-rows enabled-groups run-results #{(first identities)} nil false)]
    (is (= (count (filter #(= "baseline" (:severity %)) findings)) present)
        "baseline count leaves out new rows")
    (is (some #(= "error" (:severity %)) findings)
        "new row labeled error")))

(deftest baseline-console-quiet-test
  (let [{:keys [run-results]} (run-analysis)
        {:keys [findings present]} (core/baseline-console-rows enabled-groups run-results #{} nil true)]
    (is (empty? findings)
        "quiet mode leaves out every baselined finding")
    (is (pos? present)
        "quiet mode still counts baselined rows")))

(deftest baseline-edn-output-test
  (let [{:keys [identities]} (run-analysis)
        {:keys [new present fixed]} (baseline/diff-baseline identities identities)
        out (with-out-str
              (edn-reporter/print-baseline-report [(disk/typed-path fixture-dir)] new present fixed 0))
        parsed (edn/read-string out)]
    (is (= 0 (:exit-code parsed)))
    (is (empty? (:new-issues parsed)))
    (is (seq (:baseline-issues parsed)))
    (is (empty? (:fixed-issues parsed)))))

(def ^:private absolute-dir (-> fixture-dir disk/path .toAbsolutePath str))

(deftest baseline-identities-use-relative-paths-test
  (let [run-results [(core/run (disk/typed-path absolute-dir) enabled-groups)]
        identities (baseline/collect-identities run-results)
        ids-with-file (filter :file identities)]
    (is (seq ids-with-file)
        "some identities have file paths")
    (doseq [id ids-with-file]
      (is (not (-> id :file disk/path .isAbsolute))
          (str "path should not be absolute, got: " (:file id)))
      (is (not (re-find #"\.\." (:file id)))
          (str "path should not contain .., got: " (:file id))))))

(deftest baseline-round-trip-with-absolute-source-dir-test
  (let [run-results [(core/run (disk/typed-path absolute-dir) enabled-groups)]
        identities (baseline/collect-identities run-results)]
    (with-baseline-path
      (fn [path]
        (baseline/write-baseline path identities)
        (let [{:keys [ok]} (baseline/read-baseline (memory-fs/typed path))
              rerun-results [(core/run (disk/typed-path absolute-dir) enabled-groups)]
              rerun-identities (baseline/collect-identities rerun-results)
              {:keys [new fixed]} (baseline/diff-baseline ok rerun-identities)]
          (is (empty? new)
              "no new issues on re-analysis with absolute path")
          (is (empty? fixed)
              "no fixed issues on re-analysis with absolute path"))))))

(deftest baseline-round-trip-survives-reanalysis-test
  (let [{:keys [identities]} (run-analysis)]
    (with-baseline-path
      (fn [path]
        (baseline/write-baseline path identities)
        (let [{:keys [ok]} (baseline/read-baseline (memory-fs/typed path))
              {reanalyzed :identities} (run-analysis)
              {:keys [new present fixed]} (baseline/diff-baseline ok reanalyzed)]
          (is (empty? new)
              "re-analysis produces no new issues")
          (is (= reanalyzed present))
          (is (empty? fixed)))))))

(deftest baseline-snapshot-test
  (let [{:keys [identities]} (run-analysis)
        expected (edn/read-string (slurp "test-projects/baseline-app/expected-baseline.edn"))]
    (with-baseline-path
      (fn [path]
        (baseline/write-baseline path identities)
        (let [data (edn/read-string (memory-fs/read-text path))]
          (is (= baseline/baseline-version (:version data)))
          (is (string? (:generated-at data)))
          (is (= expected (:issues data))
              "baseline issues match snapshot"))))))
