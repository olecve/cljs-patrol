(ns shared-ids.nested-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(defn make-suite []
  (deftest nested-check
    (render! [:button {:data-testid "nest"
                       :type :button}
              "Nest"])
    (is (some? (s/get-by-test-id "nest")))))
