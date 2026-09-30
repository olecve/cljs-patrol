(ns webapp.naming-test
  (:require
   [cljs.test :refer [deftest is]]))

(deftest status-label-is-rendered
  (is true))

(deftest escape-closes-the-modal
  ;; an article inside the sentence is grammar, not noise
  (is true))

(deftest without-on-select-the-row-is-not-a-button
  (is true))

(deftest another-claim-holds
  ;; "another" only starts with "an", it is not the article
  (is true))

(deftest theme-switches-to-dark
  ;; "theme" only starts with "the"
  (is true))

(deftest divide-test
  (is true))

(deftest the-status-label-is-rendered
  (is true))

(deftest a-viewer-cannot-change-relevance
  (is true))

(deftest an-empty-list-renders-a-hint
  (is true))

(deftest ^:async the-panel-loads-lazily
  (is true))

(deftest assertion-message-placement
  (is (= 4 (+ 2 2))
      "the message on its own line reads as its own thought")
  (is (= 4 (+ 2 2)) "this one shares the line with the expression")
  (is (= 4 (+ 2 2)))
  (is (= 4 (+ 2 2))
      message-from-a-symbol)
  (is (= 4
         (+ 2 2))
      "a multi-line expression already puts the message on its own line")
  (is (= 4
         (+ 2 2)) "but the message can still share the closing line"))

(deftest conditional-assertions
  (let [owner? true
        found (some #(when (= 1 (:id %)) %) [{:id 1}])]
    (is (= found {:id 1})
        "a when inside a function literal is a predicate, not the test choosing")
    (is (= 4 (+ 2 2))
        "a plain assertion states its expectation")
    (is (= owner? (if owner? true false))
        "the expected value is computed by a conditional")
    (if owner?
      (is (= 1 1))
      (is (= 2 2)))
    (when owner?
      (is (= 3 3)))))
