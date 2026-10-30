(ns app.true-clause-thread-defined)

(defn true-clause-thread-defined [db]
  (cond-> db true (assoc :loading? true)))
