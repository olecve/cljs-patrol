(ns datadefmixed.views-test
  (:require
   [cljs.test :refer [deftest]]
   [react-testing-library-cljs.screen :as s]))

(deftest bad-save-button-by-test-id
  (.click (s/get-by-test-id "save-button")))

(deftest bad-open-button-by-test-id
  (.click (s/get-by-test-id "open-button")))
