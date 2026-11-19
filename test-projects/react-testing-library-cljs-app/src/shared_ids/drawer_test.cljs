(ns shared-ids.drawer-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest finds-first-and-last-by-test-id
  (render! [:div
            [:button {:data-testid "first"
                      :type :button}
             "First"]
            [:button {:data-testid "last"
                      :type :button}
             "Last"]])
  (is (some? (s/get-by-test-id "first")))
  (is (some? (s/get-by-test-id "last"))))

(deftest renders-a-first-button-without-an-id
  (render! [:button {:type :button}
            "First"])
  (is (some? (s/get-by-role "button" {:name "First"}))))
