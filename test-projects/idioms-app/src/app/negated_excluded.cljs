(ns app.negated-excluded
  (:refer-clojure :exclude [when-not])
  (:require [app.macros :refer [when-not]]))

(defn negated-excluded [x]
  (when (not x) :a))
