(ns app.reader-def-cond)

#?(:clj (defmacro cond [& clauses] `(clojure.core/cond ~@clauses)))

(defn reader-def [n]
  (cond (pos? n) :up :else nil))
