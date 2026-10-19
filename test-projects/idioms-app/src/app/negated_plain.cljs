(ns app.negated-plain)

(defn negated-plain [x]
  (when (not x) :a))
