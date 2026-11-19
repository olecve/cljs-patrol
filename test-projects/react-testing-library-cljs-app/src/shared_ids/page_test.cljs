(ns shared-ids.page-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [shared-ids.helpers :refer [get-open-button]]
   [shared-ids.open-row :refer [open-row]]))

(deftest opens-from-the-row
  (render! [:<>
            [:button {:type :button}
             "Open"]
            [open-row]])
  (is (some? (get-open-button))))
