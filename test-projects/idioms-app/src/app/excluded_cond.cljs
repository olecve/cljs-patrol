(ns app.excluded-cond
  (:refer-clojure :exclude [cond])
  (:require-macros [app.macros :refer [cond]]))

(defn excluded [n]
  (cond (pos? n) :up :else nil))
