(ns shared-ids.panel-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [shared-ids.button-row :refer [button-row]]))

(deftest finds-save-in-a-component
  (render! [button-row])
  (is (some? (s/get-by-test-id "save"))))
