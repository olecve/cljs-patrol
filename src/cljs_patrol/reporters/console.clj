(ns cljs-patrol.reporters.console
  "Console output formatting for cljs-patrol analysis results."
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.string :as str]))

(defn format-entry
  ([item] (format-entry item nil))
  ([{:keys [file kw row]} tag]
   (let [prefix (if tag (str tag " ") "")]
     (format "  %s%-60s %s:%d" prefix (str kw) file row))))

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
  `new-identities` is a set of identity maps for new issues."
  [result new-identities]
  (doseq [[rule-key items] result
          :when (and (sequential? items) (seq items))]
    (let [dynamic? (:form (first items))]
      (println (str "\n=== " (key->title rule-key) " (" (count items) ") ==="))
      (doseq [item (sort-by (if dynamic? :file (comp str :kw)) items)]
        (let [id (baseline/issue->identity rule-key item)
              tag (if (contains? new-identities id) "[NEW]" "[BASE]")]
          (println (if dynamic?
                     (format-tagged-dynamic item tag)
                     (format-tagged-entry item tag))))))))
