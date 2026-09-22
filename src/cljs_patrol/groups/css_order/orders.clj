(ns cljs-patrol.groups.css-order.orders
  "Property-order tables, each the property list of one stylelint property-order config.
  Every table is embedded verbatim from its npm package. `stylelint-order` applies the same
  lists to CSS, so a project already running one of these configs gets the matching order here."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def sources
  "Order name -> where its property list came from, in the order the docs list them."
  (array-map
   :recess {:resource "cljs_patrol/recess_order.edn"
            :package "stylelint-config-recess-order"
            :url "https://github.com/stormwarning/stylelint-config-recess-order"}
   :clean {:resource "cljs_patrol/clean_order.edn"
           :package "stylelint-config-clean-order"
           :url "https://github.com/kutsan/stylelint-config-clean-order"}
   :concentric {:resource "cljs_patrol/concentric_order.edn"
                :package "stylelint-config-concentric-order"
                :url "https://github.com/ream88/stylelint-config-concentric-order"}
   :smacss {:resource "cljs_patrol/smacss_order.edn"
            :package "stylelint-config-property-sort-order-smacss"
            :url "https://github.com/cahamilton/stylelint-config-property-sort-order-smacss"}
   :idiomatic {:resource "cljs_patrol/idiomatic_order.edn"
               :package "stylelint-config-idiomatic-order"
               :url "https://github.com/ream88/stylelint-config-idiomatic-order"}))

(def default-order :recess)

(def names (vec (keys sources)))

(defn- load-ranks [resource]
  (into {}
        (map-indexed (fn [index property] [property index]))
        (edn/read-string (slurp (io/resource resource)))))

(def ^:private order->ranks
  (delay (into {} (map (fn [[order {:keys [resource]}]] [order (load-ranks resource)])) sources)))

(def unknown-rank
  "Rank for a property the chosen table does not name, custom properties included.
  It sorts after every property the table does name, so a ranked property following an
  unranked one reads as out of order while two unranked ones in a row do not."
  Long/MAX_VALUE)

(defn ranks
  "Property -> rank map for `order`, or nil when no table goes by that name."
  [order]
  (get @order->ranks order))

(defn rank [order property]
  (get (ranks order) property unknown-rank))

(defn resolve-order
  "Return `order` when a table goes by that name, else the default.
  An unrecognized name warns rather than throwing: a typo in config should not stop a CI run,
  but silently ordering by something else would be worse than saying so."
  [order]
  (cond
    (nil? order) default-order
    (contains? sources order) order
    :else (binding [*out* *err*]
            (println (str "WARN: unknown css-order table " order "; using " default-order
                          ". Available: " (str/join ", " (map name names))))
            default-order)))
