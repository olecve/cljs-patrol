(ns sharedids.panel-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [sharedids.panel :refer [panel]]))

(deftest finds-save-through-panel
  (render! [panel])
  (is (some? (s/get-by-test-id "save"))))
