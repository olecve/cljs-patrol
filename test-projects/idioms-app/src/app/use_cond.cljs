(ns app.use-cond
  (:use [app.macros]))

(defn use-cond [n]
  (cond (pos? n) :up :else nil))
