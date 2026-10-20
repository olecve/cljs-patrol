(ns app.as-and-refer-cond
  (:require [app.macros :as m :refer-macros [cond]]))

(defn as-and-refer [n]
  (m/check n)
  (cond (pos? n) :up :else nil))
