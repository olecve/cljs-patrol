(ns app.second-ns-cond)

(defn second-ns [n]
  (cond (pos? n) :up :else nil))

(ns app.second-ns-cond-b
  (:refer-clojure :exclude [cond]))
