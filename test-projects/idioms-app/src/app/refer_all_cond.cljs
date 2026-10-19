(ns app.refer-all-cond
  (:require [app.macros :refer :all]))

(defn refer-all [n]
  (cond (pos? n) :up :else nil))
