(ns app.vec-of-map-filter-plain)

(defn vec-of-map-filter-plain [rows]
  (vec (map :id rows)))
