(ns cljs-patrol.groups.css-order.recess
  "Outside-to-inside property ranks, read from the embedded Recess order table."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(def ^:private property-groups
  (edn/read-string (slurp (io/resource "cljs_patrol/recess_order.edn"))))

(def ^:private property->rank
  (into {} (map-indexed (fn [index property] [property index])) (apply concat property-groups)))

(def unknown-rank
  "Rank for a property the Recess table does not name, custom properties included.
  It sorts after every known property, so a known property following an unknown one
  reads as out of order while two unknowns in a row do not."
  Long/MAX_VALUE)

(defn rank [property]
  (get property->rank property unknown-rank))

(def property-count (count property->rank))
