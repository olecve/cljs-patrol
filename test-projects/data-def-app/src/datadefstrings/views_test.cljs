(ns datadefstrings.views-test
  (:require
   [cljs.test :refer [deftest]]
   [react-testing-library-cljs.screen :as s]))

(deftest ok-save-button-by-test-id
  (.click (s/get-by-test-id "save-button")))

(deftest ok-open-button-by-test-id
  (.click (s/get-by-test-id "open-button")))

(deftest ok-close-button-by-test-id
  (.click (s/get-by-test-id "close-button")))

(deftest ok-edit-button-by-test-id
  (.click (s/get-by-test-id "edit-button")))
