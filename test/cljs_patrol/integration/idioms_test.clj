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
  (is (= ["plain"] (map :test (found :cond-else-nil)))
      "a cond the ns form may take from elsewhere is left alone, a reader conditional included"))
