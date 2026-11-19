(ns sharedids.edit-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest edits-by-test-id
  (render! [:button {:data-testid "edit"
                     :type :button}
            "Edit"])
  (is (some? (s/get-by-test-id "edit"))))
