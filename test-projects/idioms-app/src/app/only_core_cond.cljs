(ns app.only-core-cond
  (:refer-clojure :only [when]))

(defn only-core [n]
  (cond (pos? n) :up :else nil))
