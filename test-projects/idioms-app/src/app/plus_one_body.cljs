(ns app.plus-one-body)

(defn setup! []
  (def inc identity))

(defn plus-one-body [n]
  (+ n 1))
