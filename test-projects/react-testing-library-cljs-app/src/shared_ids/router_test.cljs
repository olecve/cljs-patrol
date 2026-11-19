(ns shared-ids.router-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]
   [shared-ids.router :as router]
   [shared-ids.toolbar :refer [toolbar]]))

(deftest closes-from-the-toolbar
  (render! [:<> [toolbar] [router/page {:id :fake}]])
  (is (some? (s/get-by-test-id "close"))))
