(ns app.plus-one-nested)

(do
  (defn ^:private inc [n]
    (+ n 2)))

(defn plus-one-nested [n]
  (+ n 1))
