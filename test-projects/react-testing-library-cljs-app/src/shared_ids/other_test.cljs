(ns shared-ids.other-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest finds-buttons-by-role
  (render! [:div
            [:button {:type :button}
             "Last"]
            [:button {:type :button}
             "OK"]
            [:button {:type :button}
             "Yes"]
            [:button {:type :button}
             "Go"]])
  (is (some? (s/get-by-role "button" {:name "Last"}))
      "first"))
