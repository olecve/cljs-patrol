(ns cljs-patrol.reporters.console
  "Console output formatting for cljs-patrol analysis results."
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.string :as str]))

(defn format-entry [{:keys [file kw row]}]
  (format "  %-60s %s:%d" (str kw) file row))

(defn print-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [item (sort-by (comp str :kw) items)]
      (println (format-entry item)))))

(defn print-dynamic-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [{:keys [file form row]} (sort-by :file items)]
      (println (format "  %s:%d  %s" file row (str/trim form))))))

(defn- key->title [k]
  (-> (name k)
      (str/replace #"-" " ")
      str/capitalize))

(defn report [result]
  (doseq [[k items] result
          :when (sequential? items)]
    (if (and (seq items) (:form (first items)))
      (print-dynamic-section (key->title k) items)
      (print-section (key->title k) items))))

(defn- format-tagged-entry [{:keys [file kw row]} tag]
  (format "  %-7s %-60s %s:%d" tag (str kw) file row))

(defn- format-tagged-dynamic [{:keys [file form row]} tag]
  (format "  %-7s %s:%d  %s" tag file row (str/trim form)))

(defn report-with-baseline
  "Print analysis results with [NEW] / [BASE] tags.
  `new-identities` is a set of identity maps for new issues.
  When `quiet?` is true, only new issues are printed."
  ([result new-identities] (report-with-baseline result new-identities false))
  ([result new-identities quiet?]
   (doseq [[rule-key items] result
           :when (and (sequential? items) (seq items))]
     (let [dynamic? (:form (first items))
           items-to-show (if quiet?
                           (filter #(contains? new-identities
                                               (baseline/issue->identity rule-key %))
                                   items)
                           items)]
       (when (seq items-to-show)
         (println (str "\n=== " (key->title rule-key) " (" (count items-to-show) ") ==="))
         (doseq [item (sort-by (if dynamic? :file (comp str :kw)) items-to-show)]
           (let [id (baseline/issue->identity rule-key item)
                 tag (if (contains? new-identities id) "[NEW]" "[BASE]")]
             (println (if dynamic?
                        (format-tagged-dynamic item tag)
                        (format-tagged-entry item tag))))))))))
