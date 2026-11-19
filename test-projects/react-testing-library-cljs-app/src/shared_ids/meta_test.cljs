(ns shared-ids.meta-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

^:integration
(deftest goes-with-metadata
  (render! [:button {:data-testid "go"
                     :type :button}
            "Go"])
  (is (some? (s/get-by-test-id "go"))))
