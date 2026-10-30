(ns app.true-test-thread-defined)

(defn true-test-thread-defined [db]
  (cond-> db true (assoc :loading? true)))
