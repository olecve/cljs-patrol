(ns app.plus-one-excluded
  (:refer-clojure :exclude [dec])
  (:require [app.util :refer [dec]]))

(defn plus-one-excluded [n]
  (- n 1))
