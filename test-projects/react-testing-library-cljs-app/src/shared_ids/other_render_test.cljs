(ns shared-ids.other-render-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [reagent.dom :as rdom]))

(deftest draws-without-cleanup
  (rdom/render [:button {:data-testid "draw"
                         :type :button}
                "Draw"]
               (js/document.getElementById "app"))
  (is (some? (s/get-by-test-id "draw"))))

(deftest leaves-a-paint-button
  (rdom/render [:button {:type :button}
                "Paint"]
               (js/document.getElementById "app")))

(deftest paints-after-render
  (render! [:button {:data-testid "paint"
                     :type :button}
            "Paint"])
  (is (some? (s/get-by-test-id "paint"))))

(deftest looks-before-render
  (is (nil? (s/query-by-test-id "early")))
  (render! [:button {:data-testid "early"
                     :type :button}
            "Early"]))

(deftest renders-only-in-a-discard
  #_(render! [:div])
  (rdom/render [:button {:data-testid "late"
                         :type :button}
                "Late"]
               (js/document.getElementById "app"))
  (is (some? (s/get-by-test-id "late"))))

(deftest renders-only-in-a-comment
  (comment (render! [:div]))
  (rdom/render [:button {:data-testid "noted"
                         :type :button}
                "Noted"]
               (js/document.getElementById "app"))
  (is (some? (s/get-by-test-id "noted"))))
