(ns datadefbuilt.views-test
  (:require
   [cljs.test :refer [deftest]]
   [datadefbuilt.views]
   [react-testing-library-cljs.screen :as s]))

(deftest bad-save-button-by-test-id
  (.click (s/get-by-test-id "save-button")))
