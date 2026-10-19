(ns app.negated-defined)

(defn negated-defined [x]
  (when (not x) :a))
