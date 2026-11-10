(ns app.vec-of-map-filter-defined)

(defn vec-of-map-filter-defined [rows]
  (->> rows (map :id) vec))
