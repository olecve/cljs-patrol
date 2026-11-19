(ns sharedids.helpers
  (:require
   [react-testing-library-cljs.screen :as s]
   [sharedids.open-row]))

(defn get-open-button [] (s/get-by-test-id "open"))
