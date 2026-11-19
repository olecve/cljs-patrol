(ns shared-ids.same-name-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

#?(:clj
   (deftest stores
     (render! [:button {:type :button}
               "Store"]))
   :cljs
   (deftest stores
     (render! [:button {:data-testid "store"
                        :type :button}
               "Store"])
     (is (some? (s/get-by-test-id "store")))))
