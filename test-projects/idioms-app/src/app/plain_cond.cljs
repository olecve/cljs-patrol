(ns app.plain-cond)

(defn plain [n]
  (cond (pos? n) :up :else nil))
