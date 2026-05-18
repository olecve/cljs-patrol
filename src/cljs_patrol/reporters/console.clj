(ns cljs-patrol.reporters.console
  "Console output formatting for cljs-patrol analysis results."
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.string :as str]))

(defn format-entry [{:keys [file kw row]}]
  (format "  %-60s %s:%d" (str kw) file row))

(defn- section-header [title items blocking?]
  (str "\n=== " title " (" (count items) ")"
       (when blocking? " [BLOCKING]")
       " ==="))

(defn print-section
  ([title items] (print-section title items false))
  ([title items blocking?]
   (println (section-header title items blocking?))
   (if (empty? items)
     (println "  (none)")
     (doseq [item (sort-by (comp str :kw) items)]
       (println (format-entry item))))))

(defn print-dynamic-section
  ([title items] (print-dynamic-section title items false))
  ([title items blocking?]
   (println (section-header title items blocking?))
   (if (empty? items)
     (println "  (none)")
     (doseq [{:keys [file form row]} (sort-by :file items)]
       (println (format "  %s:%d  %s" file row (str/trim form)))))))

(defn- key->title [k]
  (-> (name k)
      (str/replace #"-" " ")
      str/capitalize))

(defn- blocking-rule? [fail-on-rules rule-key]
  (and (seq fail-on-rules) (contains? fail-on-rules rule-key)))

(defn report
  ([result] (report result nil))
  ([result fail-on-rules]
   (doseq [[rule-key items] result
           :when (sequential? items)]
     (let [blocking? (blocking-rule? fail-on-rules rule-key)]
       (if (and (seq items) (:form (first items)))
         (print-dynamic-section (key->title rule-key) items blocking?)
         (print-section (key->title rule-key) items blocking?))))))

(defn- format-tagged-entry [{:keys [file kw row]} tag]
  (format "  %-7s %-60s %s:%d" tag (str kw) file row))

(defn- format-tagged-dynamic [{:keys [file form row]} tag]
  (format "  %-7s %s:%d  %s" tag file row (str/trim form)))

(defn report-with-baseline
  "Print analysis results with [NEW] / [BASE] tags.
  `new-identities` is a set of identity maps for new issues.
  Options:
    :quiet? - if true, only print new issues
    :source-dir - used to relativize file paths for identity matching
    :fail-on-rules - if non-empty, section headers for matching rules get [BLOCKING]"
  ([result new-identities]
   (report-with-baseline result new-identities {}))
  ([result new-identities {:keys [quiet? source-dir fail-on-rules]}]
   (doseq [[rule-key items] result
           :when (and (sequential? items) (seq items))]
     (let [dynamic? (:form (first items))
           blocking? (blocking-rule? fail-on-rules rule-key)
           items-to-show (if quiet?
                           (filter #(contains? new-identities
                                               (baseline/issue->identity rule-key % source-dir))
                                   items)
                           items)]
       (when (seq items-to-show)
         (println (section-header (key->title rule-key) items-to-show blocking?))
         (doseq [item (sort-by (if dynamic? :file (comp str :kw)) items-to-show)]
           (let [id (baseline/issue->identity rule-key item source-dir)
                 tag (if (contains? new-identities id) "[NEW]" "[BASE]")]
             (println (if dynamic?
                        (format-tagged-dynamic item tag)
                        (format-tagged-entry item tag))))))))))
