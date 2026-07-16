(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private filter-groups #'cljs-patrol.core/filter-groups)
(def ^:private filter-run-results #'cljs-patrol.core/filter-run-results)
(def ^:private assemble-groups #'cljs-patrol.core/assemble-groups)
(def ^:private default-groups (assemble-groups {}))

(deftest filter-run-results-test
  (let [item-a {:kw :a/sub
                :file "src/a.cljs"
                :row 1}
        item-b {:kw :b/sub
                :file "src/b.cljs"
                :row 1}
        run-results [{:source-dir "src"
                      :group-results [{:unused-subs [item-a item-b]
                                       :unused-events []}]}]]
    (testing "filters items to requested files only"
      (let [result (filter-run-results run-results [(fs/absolute-path "src/a.cljs")])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (= 1 (count subs)))
        (is (= "src/a.cljs" (:file (first subs))))))

    (testing "empty result when no files match"
      (let [result (filter-run-results run-results [(fs/absolute-path "src/other.cljs")])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (empty? subs))))))

(deftest filter-groups-test
  (testing "no filters returns all groups"
    (is (= 6 (count (filter-groups default-groups {})))))

  (testing "--only selects specific group"
    (let [groups (filter-groups default-groups {:only #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups default-groups {:disable #{:spade}})]
      (is (= 5 (count groups)))
      (is (= #{:re-frame :reagent :typography :a11y :docstrings} (set (map group/group-id groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups default-groups {:only #{:re-frame}
                                                :disable #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups)))))))

(def ^:private fixture-dir "test/projects/re-frame-spade-app/src/webapp")

(deftest baseline-write-integration-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run fixture-dir enabled-groups)]
        identities (baseline/collect-identities run-results)
        dir (fs/join-path (fs/tmp-dir) (str "cljs-patrol-bw-" (fs/nano-time)))
        path (fs/join-path dir "baseline.edn")]
    (try
      (baseline/write-baseline path identities)
      (is (fs/file-exists? path)
          "baseline file created")
      (let [{:keys [ok]} (baseline/read-baseline path)]
        (is (set? ok)
            "reads back as a set")
        (is (pos? (count ok))
            "contains issues from fixture project")
        (is (every? :rule ok)
            "every identity has a :rule"))
      (finally
        (fs/delete-tree! dir)))))

(deftest baseline-compare-integration-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run fixture-dir enabled-groups)]
        found (baseline/collect-identities run-results)]

    (testing "all issues in baseline — nothing new"
      (let [{:keys [new present fixed]} (baseline/diff-baseline found found)]
        (is (empty? new))
        (is (= found present))
        (is (empty? fixed))))

    (testing "empty baseline — everything is new"
      (let [{:keys [new present fixed]} (baseline/diff-baseline #{} found)]
        (is (= found new))
        (is (empty? present))
        (is (empty? fixed))))

    (testing "extra baseline issue — shows as fixed"
      (let [extra {:rule :unused-subs
                   :key :app/gone}
            baseline-with-extra (conj found extra)
            {:keys [new present fixed]} (baseline/diff-baseline baseline-with-extra found)]
        (is (empty? new))
        (is (= found present))
        (is (= #{extra} fixed))))))

(def ^:private blocking-issue
  {:kw :app/a
   :file "a.cljs"
   :row 1
   :tier :bugs})

(def ^:private non-blocking-issue
  {:kw :app/b
   :file "b.cljs"
   :row 2
   :tier :cleanup})

(deftest standalone-failed?-test
  (testing "without --fail-on, falls back to group/failed?"
    (let [run-results [{:source-dir "src"
                        :group-results [{:unused-subs [blocking-issue]}]}]]
      (is (true? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                           :run-results run-results}))
          ":unused-subs causes group/failed? to return truthy"))
    (let [run-results [{:source-dir "src"
                        :group-results [{:unused-subs []
                                         :phantom-subs [non-blocking-issue]}]}]]
      (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                            :run-results run-results}))
          "phantom-subs alone does not trigger group/failed?")))

  (testing "with --fail-on rule set, only listed rules cause failure"
    (let [run-results [{:source-dir "src"
                        :group-results [{:unused-subs [non-blocking-issue]
                                         :phantom-subs []}]}]]
      (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                            :run-results run-results
                                            :fail-on-rules #{:phantom-subs}}))
          "unused-subs issues don't fail when only phantom-subs is selected"))
    (let [run-results [{:source-dir "src"
                        :group-results [{:unused-subs []
                                         :phantom-subs [non-blocking-issue]}]}]]
      (is (true? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                           :run-results run-results
                                           :fail-on-rules #{:phantom-subs}}))
          "phantom-subs issues fail when phantom-subs is in fail-on set")))

  (testing "empty run-results never fails"
    (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                          :run-results []})))
    (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                          :run-results []
                                          :fail-on-rules #{:phantom-subs}})))))

(def ^:private new-bug
  {:rule :duplicate-subs
   :key :app/dup})

(def ^:private new-cleanup
  {:rule :unused-subs
   :key :app/unused})

(def ^:private fixed-issue
  {:rule :unused-subs
   :key :app/gone})

(deftest baseline-failed?-test
  (testing "without --fail-on (nil rules)"
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{}}))
        "no new, no fixed -> pass")
    (is (true? (core/baseline-failed? {:new-issues #{new-cleanup}
                                       :fixed-issues #{}}))
        "any new issue fails when fail-on is unset")
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{fixed-issue}}))
        "fixed without strict -> pass")
    (is (true? (core/baseline-failed? {:new-issues #{}
                                       :fixed-issues #{fixed-issue}
                                       :strict-baseline true}))
        "fixed with strict -> fail"))

  (testing "with --fail-on bugs"
    (let [fail-on #{:duplicate-subs :duplicate-events}]
      (is (true? (core/baseline-failed? {:new-issues #{new-bug}
                                         :fixed-issues #{}
                                         :fail-on-rules fail-on}))
          "new bug fails")
      (is (false? (core/baseline-failed? {:new-issues #{new-cleanup}
                                          :fixed-issues #{}
                                          :fail-on-rules fail-on}))
          "new cleanup does not fail when only bugs selected")
      (is (true? (core/baseline-failed? {:new-issues #{new-bug new-cleanup}
                                         :fixed-issues #{}
                                         :fail-on-rules fail-on}))
          "mixed new issues: bug still fails")))

  (testing "baseline issues never fail (that's the point)"
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{}
                                        :fail-on-rules #{:duplicate-subs}}))
        "no new issues, even with fail-on set"))

  (testing "fixed issues with strict ignore fail-on tier"
    (is (true? (core/baseline-failed? {:new-issues #{}
                                       :fixed-issues #{fixed-issue}
                                       :fail-on-rules #{:duplicate-subs}
                                       :strict-baseline true}))
        "strict fails on fixed regardless of tier"))

  (testing "combined: new bug + fixed cleanup, with strict and fail-on bugs"
    (is (true? (core/baseline-failed? {:new-issues #{new-bug}
                                       :fixed-issues #{fixed-issue}
                                       :fail-on-rules #{:duplicate-subs}
                                       :strict-baseline true})))))

(deftest baseline-with-files-filter-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run fixture-dir enabled-groups)]
        all-ids (baseline/collect-identities run-results)
        filtered-results (#'cljs-patrol.core/filter-run-results
                          run-results
                          [(fs/absolute-path (str fixture-dir "/subs.cljs"))])
        filtered-ids (baseline/collect-identities filtered-results)]
    (is (< (count filtered-ids) (count all-ids))
        "filtering reduces issue count")
    (is (every? #(contains? all-ids %) filtered-ids)
        "filtered ids are a subset of all ids")))
