(ns cljs-patrol.integration.tests-naming-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.tests :as tests]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/tests-app/src/webapp")

(defn- findings []
  (-> (core/run fixture-dir [tests/group]) :group-results first :deftest-leading-article))

(deftest deftest-leading-article-fixture-test
  (let [found (findings)
        by-name (into {} (map (juxt (comp str :kw) identity)) found)]

    (testing "flags a name opening with an article"
      (is (contains? by-name "the-status-label-is-rendered"))
      (is (contains? by-name "a-viewer-cannot-change-relevance"))
      (is (contains? by-name "an-empty-list-renders-a-hint")))

    (testing "reads the name through metadata on the var"
      (is (contains? by-name "the-panel-loads-lazily")
          "^:async sits between deftest and the name"))

    (testing "an article inside the sentence is grammar, not noise"
      (is (not (contains? by-name "escape-closes-the-modal")))
      (is (not (contains? by-name "without-on-select-the-row-is-not-a-button"))))

    (testing "a word that merely starts with those letters is not an article"
      (is (not (contains? by-name "another-claim-holds"))
          "\"another\" opens with an, and the hyphen is what tells them apart")
      (is (not (contains? by-name "theme-switches-to-dark"))
          "\"theme\" opens with the"))

    (testing "leaves a name carrying no article alone"
      (is (not (contains? by-name "status-label-is-rendered")))
      (is (not (contains? by-name "divide-test"))))

    (testing "names the article and what the name becomes without it"
      (is (= "Drop the leading \"the-\": status-label-is-rendered"
             (:hint (by-name "the-status-label-is-rendered")))))

    (testing "flags exactly the bad names"
      (is (= 4 (count found))
          "four names open with an article in the fixture, no more"))))

(deftest baseline-identity-test
  (testing "a test var is identified by its name and file, not its row"
    (let [issue (first (findings))
          identity-of (fn [row] (baseline/issue->identity :deftest-leading-article
                                                          (assoc issue :row row)
                                                          fixture-dir))]
      (is (= (identity-of 26) (identity-of 94))
          "the row moves whenever anything above the var does")
      (is (= #{:rule :name :file} (set (keys (identity-of 26))))))))
