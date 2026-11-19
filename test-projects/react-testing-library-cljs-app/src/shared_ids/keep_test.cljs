(ns shared-ids.keep-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [shared-ids.conditional-test :refer [keep-dialog]]))

(deftest keeps-from-the-second-button
  (render! [:<>
            [keep-dialog]
            [:button {:data-testid "keep"
                      :type :button}
             "Keep"]])
  (is (some? (s/get-by-test-id "keep"))))
