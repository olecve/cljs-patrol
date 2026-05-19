(ns cljs-patrol.integration.fail-on-test
  "End-to-end tests for --fail-on against the baseline-app fixture project.
  Exercises the full pipeline: parser -> analyze -> tier annotation ->
  exit-code decision, with and without --baseline."
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.severity :as severity]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test/projects/baseline-app/src/baseline_app")
(def ^:private enabled-groups [re-frame/group spade/group reagent/group])
(def ^:private rule->tier (severity/collect-rule->tier enabled-groups))
(def ^:private bugs (severity/tier->rules rule->tier :bugs))
(def ^:private deprecations (severity/tier->rules rule->tier :deprecations))
(def ^:private cleanup (severity/tier->rules rule->tier :cleanup))

(defn- run-fixture []
  [(core/run fixture-dir enabled-groups)])

(deftest standalone-fail-on-bugs-test
  (let [run-results (run-fixture)]
    (is (true? (core/standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules bugs}))
        "fixture has :reg-event-fx-empty (::no-op) -> exit 1")))

(deftest standalone-fail-on-deprecations-test
  (let [run-results (run-fixture)]
    (is (true? (core/standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules deprecations}))
        "fixture has :deprecated-effects and :defclass-as-sole-attr -> exit 1")))

(deftest standalone-fail-on-cleanup-test
  (let [run-results (run-fixture)]
    (is (true? (core/standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules cleanup}))
        "fixture has unused/phantom issues -> exit 1")))

(deftest standalone-fail-on-all-test
  (let [run-results (run-fixture)
        all-rules (set (keys rule->tier))]
    (is (true? (core/standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules all-rules}))
        "any tiered issue -> exit 1")))

(deftest baseline-snapshot-fail-on-bugs-passes-test
  (let [run-results (run-fixture)
        identities (baseline/collect-identities run-results)
        {:keys [new fixed]} (baseline/diff-baseline identities identities)]
    (is (false? (core/baseline-failed? {:new-issues new
                                        :fixed-issues fixed
                                        :fail-on-rules bugs}))
        "all issues baselined, none new -> exit 0 even with --fail-on bugs")))

(deftest baseline-empty-fail-on-bugs-fails-test
  (let [run-results (run-fixture)
        identities (baseline/collect-identities run-results)
        {:keys [new fixed]} (baseline/diff-baseline #{} identities)]
    (is (true? (core/baseline-failed? {:new-issues new
                                       :fixed-issues fixed
                                       :fail-on-rules bugs}))
        "fixture has bug-tier issues (::no-op empty handler) -> exit 1")))

(deftest baseline-empty-fail-on-cleanup-fails-test
  (let [run-results (run-fixture)
        identities (baseline/collect-identities run-results)
        {:keys [new fixed]} (baseline/diff-baseline #{} identities)]
    (is (true? (core/baseline-failed? {:new-issues new
                                       :fixed-issues fixed
                                       :fail-on-rules cleanup}))
        "all fixture issues are new, cleanup-tier ones present -> exit 1")))

(deftest baseline-new-cleanup-fail-on-bugs-passes-test
  (let [run-results (run-fixture)
        identities (baseline/collect-identities run-results)
        baseline-without-cleanup (into #{}
                                       (remove #(contains? cleanup (:rule %)))
                                       identities)
        {:keys [new fixed]} (baseline/diff-baseline baseline-without-cleanup identities)]
    (is (seq new)
        "cleanup-tier issues are new")
    (is (false? (core/baseline-failed? {:new-issues new
                                        :fixed-issues fixed
                                        :fail-on-rules bugs}))
        "new cleanup issues don't fail when only bugs are blocking")))

(deftest baseline-strict-fixed-cleanup-still-fails-test
  (let [run-results (run-fixture)
        identities (baseline/collect-identities run-results)
        extra-fixed {:rule :unused-subs
                     :key :baseline-app.subs/gone}
        baseline-with-extra (conj identities extra-fixed)
        {:keys [new fixed]} (baseline/diff-baseline baseline-with-extra identities)]
    (is (true? (core/baseline-failed? {:new-issues new
                                       :fixed-issues fixed
                                       :fail-on-rules bugs
                                       :strict-baseline true}))
        "strict mode fails on fixed even if it's a non-blocking tier")))
