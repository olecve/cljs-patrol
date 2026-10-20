(ns app.plus-one-protocol)

(defprotocol Stepper
  (dec [this]))

(defn plus-one-protocol [n]
  (- n 1))
