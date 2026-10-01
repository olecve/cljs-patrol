(ns cljs-patrol.integration.tests-naming-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.tests :as tests]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/tests-app/src/webapp")

(defn- result []
  (-> (core/run fixture-dir [tests/group]) :group-results first))

(defn- findings []
  (:deftest-leading-article (result)))

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

(deftest assertion-message-inline-fixture-test
  (let [found (:assertion-message-inline (result))
        by-row (into {} (map (juxt :row identity)) found)]

    (testing "flags a message sharing the expression's line"
      (is (contains? by-row 41)
          "a single-line expression with the message bolted on the end"))

    (testing "flags a message on the closing line of a multi-line expression"
      (is (contains? by-row 49)
          "the expression spans lines, but the message still shares its last one"))

    (testing "leaves a message on its own line alone"
      (is (not (contains? by-row 39)))
      (is (not (contains? by-row 46))
          "a multi-line expression whose message follows it on a new line"))

    (testing "says nothing about an assertion carrying no message"
      (is (not (contains? by-row 42))))

    (testing "only a string written out is the shape this is about"
      (is (not (contains? by-row 44))
          "a message named by a symbol is left alone"))

    (testing "names what to do"
      (is (= "Move the message to its own line, under the expression."
             (:hint (by-row 41)))))

    (testing "flags exactly the inline messages"
      (is (= 2 (count found))
          "two of the six assertions in the fixture share a line"))))

(deftest conditional-assertion-fixture-test
  (let [found (:conditional-assertion (result))
        by-row (into {} (map (juxt :row identity)) found)]

    (testing "flags an expected value computed by a conditional"
      (is (contains? by-row 58))
      (is (= "The expected value is computed here. State it instead, one case per row of a table."
             (:hint (by-row 58)))))

    (testing "flags a conditional choosing between assertions"
      (is (contains? by-row 60))
      (is (= "The test chooses between assertions. Pair each input with its expected value instead."
             (:hint (by-row 60)))))

    (testing "flags a guard that can skip the assertion"
      (is (contains? by-row 63))
      (is (= "This guard can skip the assertion, and the test then passes having checked nothing."
             (:hint (by-row 63)))))

    (testing "a conditional inside a function literal is a predicate"
      (is (not (contains? by-row 53))
          "#(when (= 1 (:id %)) %) handed to some branches per element, not per assertion"))

    (testing "leaves an assertion stating its expectation alone"
      (is (not (contains? by-row 56))))

    (testing "reports the row in the file, not in the form"
      (is (every? #(> (:row %) 1) found)
          "a z/subzip walk would name line 1 of the assertion instead"))

    (testing "flags exactly the three shapes"
      (is (= 3 (count found))))))

(deftest assertion-identity-separates-same-message-test
  (testing "two assertions sharing a message but not an expression stay apart in a baseline"
    (let [finding (fn [expression]
                    {:kw (symbol "is")
                     :form (str "(is " expression " \"the row is shown\")")
                     :file "src/views_test.cljs"
                     :row 10})
          identity-of #(baseline/issue->identity :assertion-message-inline (finding %))]
      (is (not= (identity-of "(.-checked checkbox-1)")
                (identity-of "(.-checked checkbox-3)"))
          "the identity keys on the whole assertion, not the message alone")
      (is (= (identity-of "(.-checked checkbox-1)")
             (identity-of "(.-checked checkbox-1)"))
          "the same assertion twice in one file is one identity, which no stable key can split"))))

(deftest assertion-identity-survives-a-reformat-test
  (testing "re-wrapping the expression across lines does not change the identity"
    (let [identity-of (fn [form row]
                        (baseline/issue->identity :assertion-message-inline
                                                  {:kw (symbol "is")
                                                   :form form
                                                   :file "src/views_test.cljs"
                                                   :row row}))]
      (is (= (identity-of "(is (= 2 (block-count surface)) \"the document reached the editor\")" 10)
             (identity-of "(is (= 2\n         (block-count surface))\n      \"the document reached the editor\")" 10))
          "whitespace is collapsed when the identity is built, not only when the finding is")
      (is (= (identity-of "(is (nil? (palette)) \"gone\")" 10)
             (identity-of "(is (nil? (palette)) \"gone\")" 94))
          "the row is not part of it, so a line moving above the finding changes nothing")
      (is (not-any? #{:row :line} (keys (identity-of "(is x \"m\")" 10)))
          "no row-shaped key reaches the stored identity"))))
