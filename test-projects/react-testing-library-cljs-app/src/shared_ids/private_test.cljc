(ns shared-ids.private-test
  (:require
   [clojure.test :refer [deftest- is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest- confirms-privately
  (render! [:button {:data-testid "ok"
                     :type :button}
            "OK"])
  (is (some? (s/get-by-test-id "ok"))))
