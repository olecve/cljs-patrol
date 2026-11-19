(ns sharedids.page-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [sharedids.helpers :refer [get-open-button]]
   [sharedids.open-row :refer [open-row]]
   [sharedids.page :refer [page]]))

(deftest opens-from-the-row
  (render! [:<> [page] [open-row]])
  (is (some? (get-open-button))))
