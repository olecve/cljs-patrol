(ns sharedids.other-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(def ^:private position "first")

(deftest finds-first-and-last-by-role
  (render! [:div
            [:button {:type :button}
             "First"]
            [:button {:type :button}
             "Last"]])
  (is (some? (s/get-by-role "button" {:name "Last"})) position))
