(ns app.true-test-plain)

(defn true-test-plain [db reset?]
  (cond-> db
    true (assoc :loading? true)
    reset? (dissoc :cache)))
