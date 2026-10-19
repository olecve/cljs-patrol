(ns app.reader-def-other)

#?(:clj (defn helper [x] x))

(defn reader-def-other [n]
  (cond (pos? n) :up :else nil))
