(ns cljs-patrol.reporters.edn
  "EDN output for cljs-patrol analysis results, suitable for programmatic and AI-assisted use."
  (:require
   [cljs-patrol.group :as group]))

(defn- absolutize-item [item]
  (if (:file item)
    (update item :file #(.getAbsolutePath (java.io.File. %)))
    item))

(defn- absolutize-result [result]
  (into {} (map (fn [[k v]]
                  [k (if (sequential? v) (mapv absolutize-item v) v)])
                result)))

(defn- merge-results [results]
  (apply merge-with (fn [a b] (if (sequential? a) (into a b) b)) results))

(defn print-report
  "Print analysis results as EDN to stdout.
  File paths are absolute for direct use with editor/tooling integrations."
  [enabled-groups dirs run-results]
  (let [merged (into {}
                     (map-indexed (fn [g-idx g]
                                    [(group/group-id g) (absolutize-result
                                                         (merge-results
                                                          (map #(nth (:group-results %) g-idx) run-results)))])
                                  enabled-groups))
        suggestions (into {} (map (fn [g] [(group/group-id g) (group/suggestions g)]) enabled-groups))]
    (println (pr-str {:source-dirs (mapv #(.getAbsolutePath (java.io.File. %)) dirs)
                      :results merged
                      :suggestions suggestions}))))
