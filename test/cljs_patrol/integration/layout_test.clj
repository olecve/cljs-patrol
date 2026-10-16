(ns cljs-patrol.integration.layout-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.layout :as layout]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test-projects/layout-app/src/webapp")

(defn- result
  ([] (result layout/group))
  ([rule-group] (-> (core/run fixture-dir [rule-group]) :group-results first)))

(defn- rows [findings]
  (sort (map :row findings)))

(deftest form-after-split-map-fixture-test
  (let [found (:form-after-split-map (result))]
    (testing "flags each map of a staircase, a call argument and a Hiccup child"
      (is (= [5 6 11 16] (rows found))))

    (testing "reports the file it read"
      (is (every? #(re-find #"views\.cljs$" (:file %)) found)))

    (testing "carries the cleanup tier"
      (is (every? #(= :cleanup (:tier %)) found)))))

(deftest split-map-after-sibling-fixture-test
  (testing "flags a split map after a map sibling, not after a symbol or a path"
    (is (= [23 25] (rows (:split-map-after-sibling (result))))))

  (testing "with the symbol variant configured, a symbol in a parameter list counts too"
    (is (= [23 25 28]
           (-> (layout/make-group {:split-map-after-symbol true}) result :split-map-after-sibling rows)))))
