(ns app.top-require-cond)

(require '[app.macros :refer [cond]])

(defn top-require [n]
  (cond (pos? n) :up :else nil))
