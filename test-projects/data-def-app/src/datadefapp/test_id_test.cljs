(ns datadefapp.test-id-test
  (:require
   [cljs.test :refer [deftest]]
   [datadefapp.test-id-views]
   [react-testing-library-cljs.screen :as s]))

(deftest bad-save-button-by-test-id
  (.click (s/get-by-test-id "save-button")))

(deftest ok-open-button-by-test-id
  (.click (s/get-by-test-id "open-button")))
