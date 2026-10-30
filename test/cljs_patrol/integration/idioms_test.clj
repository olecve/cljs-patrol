(ns cljs-patrol.integration.idioms-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.idioms :as idioms]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test-projects/idioms-app/src")

(defn- found
  ([] (found :nested-call-chain))
  ([rule] (-> (core/run fixture-dir [idioms/group]) :group-results first rule)))

(deftest nested-call-chain-fixture-test
  (testing "a function defined in another file counts, referred or through an alias"
    (is (= ["referred" "aliased"] (map :test (found)))))

  (testing "carries the cleanup tier"
    (is (every? #(= :cleanup (:tier %)) (found)))))

(deftest cond-else-nil-fixture-test
  (is (= #{"plain" "reader-def-other" "aliased-cond" "as-alias-cond"} (set (map :test (found :cond-else-nil))))
      "a cond the ns form may take from elsewhere is left alone, a reader conditional included"))

(deftest negated-test-fixture-test
  (is (= ["negated-plain"] (map :test (found :negated-test)))
      "a when-not the ns form excludes, or the namespace defines in another file, is no fold to ask for"))

(deftest plus-minus-one-fixture-test
  (is (= ["plus-one-plain"] (map :test (found :plus-minus-one)))
      "an inc or dec the namespace defines, declares or excludes, anywhere in it, is no step to ask for"))

(deftest negated-equality-fixture-test
  (is (= ["negated-equality-plain"] (map :test (found :negated-equality)))
      "a not= the namespace defines in another file, loads or interns is no fix to ask for"))

(deftest unconditional-cond-threading-fixture-test
  (is (= ["true-test-plain"] (map :test (found :unconditional-cond-threading)))
      "a cond-> the ns form excludes, or a cond->> or -> the namespace defines in another file, is no thread to ask for"))
