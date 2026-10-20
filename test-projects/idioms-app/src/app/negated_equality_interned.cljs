(ns app.negated-equality-interned)

(intern *ns* 'not= (fn [a b] (identical? a b)))

(defn negated-equality-interned [a b]
  (not (= a b)))
