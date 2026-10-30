(ns app.true-clause-plain)

(defn true-clause-plain [db reset?]
  (cond-> db
    true (assoc :loading? true)
    reset? (dissoc :cache)))
