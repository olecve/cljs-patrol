(ns opaque.opaque-test
  (:require
   [cljs.test :refer [deftest]]
   [react-testing-library-cljs.screen :as s]))

(defn view [build-id kind]
  [:div
   [:div {:data-testid (build-id kind)}]
   [:button {:aria-label "Send" :data-testid "send-button"}]])

(deftest finds-send-button
  (.click (s/get-by-test-id "send-button")))
