(ns app.true-test-excluded
  (:refer-clojure :exclude [cond->])
  (:require [app.util :refer [cond->]]))

(defn true-test-excluded [db]
  (cond-> db true (assoc :loading? true)))
