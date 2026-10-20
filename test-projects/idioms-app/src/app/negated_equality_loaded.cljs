(ns app.negated-equality-loaded)

(load "negated_equality_impl")

(defn negated-equality-loaded [a b]
  (not (= a b)))
