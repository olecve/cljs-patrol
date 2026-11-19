(ns shared-ids.conditional-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

#?@(:cljs
    [(defn keep-dialog []
       [:div
        [:button {:type :button}
         "Keep"]])

     (deftest agrees-in-cljs
       (render! [:button {:data-testid "yes"
                          :type :button}
                 "Yes"])
       (is (some? (s/get-by-test-id "yes"))))])
