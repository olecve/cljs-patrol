(ns shared-ids.built-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest renders-a-built-id
  (render! [:span {:data-testid (str "bui" "lt")}]))

(deftest finds-the-literal-id
  (render! [:button {:data-testid "built"
                     :type :button}
            "Built"])
  (is (some? (s/get-by-test-id "built"))))

(deftest finds-an-id-it-may-build
  (render! [:div
            [:span {:data-testid (str "ma" "de")}]
            [:button {:data-testid "made"
                      :type :button}
             "Made"]])
  (is (some? (s/get-by-test-id "made"))))
