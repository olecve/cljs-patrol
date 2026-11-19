(ns shared-ids.helpers
  (:require
   [react-testing-library-cljs.screen :as s]))

(defn get-open-button [] (s/get-by-test-id "open"))
