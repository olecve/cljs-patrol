(ns shared-ids.own-body-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest submits-from-the-second-button
  (render! [:div
            [:button {:type :button}
             "Submit"]
            [:button {:data-testid "submit"
                      :type :button}
             "Submit"]])
  (is (some? (s/get-by-test-id "submit"))))
