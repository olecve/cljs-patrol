(ns cljs-patrol.integration.idioms-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.idioms :as idioms]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test-projects/idioms-app/src")

(defn- found []
  (-> (core/run fixture-dir [idioms/group]) :group-results first :nested-call-chain))

(deftest nested-call-chain-fixture-test
  (testing "a function defined in another file counts, referred or through an alias"
    (is (= ["referred" "aliased"] (map :test (found)))))

  (testing "carries the cleanup tier"
    (is (every? #(= :cleanup (:tier %)) (found)))))
