(ns app.true-clause-defined)

(defn true-clause-defined [xs]
  (cond->> xs true (map inc)))
