(ns app.true-test-defined)

(defn true-test-defined [xs]
  (cond->> xs true (map inc)))
