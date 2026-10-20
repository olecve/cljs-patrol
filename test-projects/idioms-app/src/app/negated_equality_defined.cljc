(ns app.negated-equality-defined)

(defn not= [a b]
  (identical? a b))
