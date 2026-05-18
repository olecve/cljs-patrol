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

(defn- count-by-tier
  "Walk a merged results map and count issues that are blocking vs. warning
  per the given fail-on-rules. When fail-on-rules is empty, all counts go to 0."
  [merged fail-on-rules]
  (reduce-kv
   (fn [acc _group-id rule-map]
     (reduce-kv
      (fn [acc rule-key items]
        (if (sequential? items)
          (if (contains? fail-on-rules rule-key)
            (update acc :blocking-count + (count items))
            (update acc :warning-count + (count items)))
          acc))
      acc
      rule-map))
   {:blocking-count 0
    :warning-count 0}
   merged))

(defn print-report
  "Print analysis results as EDN to stdout.
  File paths are absolute for direct use with editor/tooling integrations.
  When fail-on-rules is provided (non-empty), the output includes
  :blocking-count and :warning-count."
  ([enabled-groups dirs run-results]
   (print-report enabled-groups dirs run-results nil))
  ([enabled-groups dirs run-results fail-on-rules]
   (let [merged (into {}
                      (map-indexed (fn [g-idx g]
                                     [(group/group-id g) (absolutize-result
                                                          (merge-results
                                                           (map #(nth (:group-results %) g-idx) run-results)))])
                                   enabled-groups))
         suggestions (into {} (map (fn [g] [(group/group-id g) (group/suggestions g)]) enabled-groups))
         output {:source-dirs (mapv #(.getAbsolutePath (java.io.File. %)) dirs)
                 :results merged
                 :suggestions suggestions}
         output (if (seq fail-on-rules)
                  (merge output (count-by-tier merged fail-on-rules))
                  output)]
     (println (pr-str output)))))

(defn- with-tier
  "Augment identity maps with :tier looked up from rule->tier."
  [identities rule->tier]
  (mapv #(assoc % :tier (get rule->tier (:rule %))) identities))

(defn print-baseline-report
  "Print baseline-aware analysis results as EDN to stdout.
  Includes :new-issues, :baseline-issues, :fixed-issues, and :exit-code.
  When fail-on-rules and rule->tier are provided, issues are annotated with
  :tier and the top level includes :blocking-count and :warning-count for
  the new-issues partition."
  ([dirs new-issues baseline-issues fixed-issues exit-code]
   (print-baseline-report dirs new-issues baseline-issues fixed-issues exit-code nil nil))
  ([dirs new-issues baseline-issues fixed-issues exit-code fail-on-rules rule->tier]
   (let [abs-dirs (mapv #(.getAbsolutePath (java.io.File. %)) dirs)
         new-sorted (vec (sort-by str new-issues))
         baseline-sorted (vec (sort-by str baseline-issues))
         fixed-sorted (vec (sort-by str fixed-issues))
         tier? (boolean rule->tier)
         output (cond-> {:source-dirs abs-dirs
                         :new-issues (if tier? (with-tier new-sorted rule->tier) new-sorted)
                         :baseline-issues (if tier? (with-tier baseline-sorted rule->tier) baseline-sorted)
                         :fixed-issues (if tier? (with-tier fixed-sorted rule->tier) fixed-sorted)
                         :exit-code exit-code}
                  (seq fail-on-rules)
                  (merge {:blocking-count (count (filter #(contains? fail-on-rules (:rule %)) new-sorted))
                          :warning-count (count (remove #(contains? fail-on-rules (:rule %)) new-sorted))}))]
     (println (pr-str output)))))
