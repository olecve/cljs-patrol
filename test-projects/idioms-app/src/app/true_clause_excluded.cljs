(ns app.true-clause-excluded
  (:refer-clojure :exclude [cond->])
  (:require [app.util :refer [cond->]]))

(defn true-clause-excluded [db]
  (cond-> db true (assoc :loading? true)))
