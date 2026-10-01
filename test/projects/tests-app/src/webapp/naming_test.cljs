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

(defn reads-a-var-in-a-helper []
  ;; a helper beside the tests, where most real reads sit
  @#'other.ns/seen-ids)

(deftest var-deref-spellings
  (is (= 1 @#'other.ns/seen-ids)
      "the reader spelling")
  (is (= 1 @(var other.ns/seen-ids))
      "the var written out")
  (is (= 1 (deref #'other.ns/seen-ids))
      "the deref written out")
  (is (= 1 @^:tag #'other.ns/seen-ids)
      "metadata between the two"))

(deftest var-quote-without-a-deref
  (is (= 1 (with-redefs-fn {#'other.ns/handler (constantly 1)}
             (fn [] 1)))
      "a var quote handed to with-redefs-fn is how a test replaces a dependency")
  (is (= 1 @some-atom)
      "a deref of an atom is not a var deref")
  (is (= 1 (helpers/deref #'other.ns/seen-ids))
      "another function whose name merely ends in deref")
  (is (= 1 (count (quote [@#'other.ns/seen-ids])))
      "a written-out quote renders nothing"))
