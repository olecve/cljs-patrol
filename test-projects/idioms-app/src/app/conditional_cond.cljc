(ns app.conditional-cond
  #?(:clj (:refer-clojure :exclude [cond])))

#?(:clj (defmacro cond [& clauses] `(clojure.core/cond ~@clauses)))

(defn conditional [n]
  (cond (pos? n) :up :else nil))
