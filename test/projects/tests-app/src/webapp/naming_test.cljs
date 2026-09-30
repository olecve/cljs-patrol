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
