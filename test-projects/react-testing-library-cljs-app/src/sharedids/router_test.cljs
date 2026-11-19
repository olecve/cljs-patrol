(ns sharedids.router-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [sharedids.router :as router]
   [sharedids.toolbar :refer [toolbar]]))

(deftest closes-from-the-toolbar
  (render! [:<> [toolbar] [router/page {:id :settings}]])
  (is (some? (s/get-by-test-id "close"))))
