(ns app.vec-of-map-filter-excluded
  (:refer-clojure :exclude [filterv])
  (:require [app.util :refer [filterv]]))

(defn vec-of-map-filter-excluded [rows]
  (vec (filter :active? rows)))
