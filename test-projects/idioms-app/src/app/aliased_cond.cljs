(ns app.aliased-cond
  (:require [app.macros :as cond]))

(defn aliased-cond [n]
  (cond (pos? n) :up :else nil))
