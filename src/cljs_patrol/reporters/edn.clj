(ns cljs-patrol.reporters.edn
  "EDN output for cljs-patrol analysis results, suitable for programmatic and AI-assisted use."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.severity :as severity]
   [clojure.set :as set]))

(defn- with-absolute-file [item]
  (if (:file item)
    (update item :file fs/absolute-path)
    item))

(defn- with-absolute-files [result]
  (into {} (map (fn [[k v]]
                  [k (if (sequential? v) (mapv with-absolute-file v) v)])
                result)))

(defn- merge-results [results]
  (apply merge-with (fn [a b] (if (sequential? a) (into a b) b)) results))

(defn- count-by-tier
  "Wraps severity/count-by-fail-on, renaming keys to the EDN schema."
  [merged fail-on-rules]
  (set/rename-keys
   (severity/count-by-fail-on (vals merged) fail-on-rules)
   {:blocking :blocking-count
    :warning :warning-count}))

(defn print-report
  "Print analysis results as EDN to stdout.
  File paths are absolute for direct use with editor/tooling integrations.
  The output always includes :blocking-count and :warning-count. When
  fail-on-rules is empty, all issues are counted as blocking."
  ([enabled-groups dirs run-results]
   (print-report enabled-groups dirs run-results nil))
  ([enabled-groups dirs run-results fail-on-rules]
   (let [merged (into {}
                      (map-indexed (fn [g-idx g]
                                     [(group/group-id g) (with-absolute-files
                                                           (merge-results
                                                            (map #(nth (:group-results %) g-idx) run-results)))])
                                   enabled-groups))
         suggestions (into {} (map (fn [g] [(group/group-id g) (group/suggestions g)]) enabled-groups))
         output (merge {:source-dirs (mapv fs/absolute-path dirs)
                        :results merged
                        :suggestions suggestions}
                       (count-by-tier merged fail-on-rules))]
     (println (pr-str output)))))

(defn- with-tier
  "Augment identity maps with :tier looked up from rule->tier."
  [identities rule->tier]
  (mapv #(assoc % :tier (get rule->tier (:rule %))) identities))

(defn print-baseline-report
  "Print baseline-aware analysis results as EDN to stdout.
  Always includes :new-issues, :baseline-issues, :fixed-issues, :exit-code,
  :blocking-count, and :warning-count (counts apply to the new-issues
  partition; when fail-on-rules is empty, all new issues count as blocking).
  When rule->tier is provided, issues are annotated with :tier."
  ([dirs new-issues baseline-issues fixed-issues exit-code]
   (print-baseline-report dirs new-issues baseline-issues fixed-issues exit-code nil nil))
  ([dirs new-issues baseline-issues fixed-issues exit-code fail-on-rules rule->tier]
   (let [abs-dirs (mapv fs/absolute-path dirs)
         new-sorted (vec (sort-by str new-issues))
         baseline-sorted (vec (sort-by str baseline-issues))
         fixed-sorted (vec (sort-by str fixed-issues))
         tier? (boolean rule->tier)
         blocking-pred (if (seq fail-on-rules)
                         #(contains? fail-on-rules (:rule %))
                         (constantly true))
         output {:source-dirs abs-dirs
                 :new-issues (if tier? (with-tier new-sorted rule->tier) new-sorted)
                 :baseline-issues (if tier? (with-tier baseline-sorted rule->tier) baseline-sorted)
                 :fixed-issues (if tier? (with-tier fixed-sorted rule->tier) fixed-sorted)
                 :exit-code exit-code
                 :blocking-count (count (filter blocking-pred new-sorted))
                 :warning-count (count (remove blocking-pred new-sorted))}]
     (println (pr-str output)))))
