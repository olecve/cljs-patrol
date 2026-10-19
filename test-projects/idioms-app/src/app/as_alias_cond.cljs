(ns app.as-alias-cond
  (:require [app.macros :as-alias cond]))

(defn as-alias-cond [n]
  (cond (pos? n) :up :else nil))
