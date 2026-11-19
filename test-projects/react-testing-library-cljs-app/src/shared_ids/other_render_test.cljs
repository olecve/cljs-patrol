(ns shared-ids.other-render-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.screen :as s]
   [reagent.dom :as rdom]))

(deftest draws-without-cleanup
  (rdom/render [:button {:data-testid "draw"
                         :type :button}
                "Draw"]
               (js/document.getElementById "app"))
  (is (some? (s/get-by-test-id "draw"))))
