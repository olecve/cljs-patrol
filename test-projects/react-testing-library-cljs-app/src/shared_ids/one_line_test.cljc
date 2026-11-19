(ns shared-ids.one-line-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

#?(:clj (deftest checks (render! [:button {:type :button} "Check"])) :cljs (deftest checks (render! [:button {:data-testid "check" :type :button} "Check"]) (is (some? (s/get-by-test-id "check")))))
