(ns cljs-patrol.reporters.console
  "Console output formatting for cljs-patrol analysis results."
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.string :as str]))

(defn format-entry [{:keys [file kw row]}]
  (format "  %-60s %s:%d" (str kw) file row))

(defn- hint-line
  "Return an indented follow-up line naming the fix, or nil when the rule attached none."
  [{:keys [hint]}]
  (when hint (str "      \u2192 " hint)))

(def ^:private suggestion-width 92)

(defn- wrap-words [width text]
  (reduce (fn [lines word]
            (let [current (peek lines)]
              (if (and current (<= (+ (count current) 1 (count word)) width))
                (conj (pop lines) (str current " " word))
                (conj lines word))))
          []
          (str/split (str/trim text) #"\s+")))

(defn- print-suggestion
  "Print a rule's suggestion under its header, wrapped and indented.
  Skipped for empty sections, where there is nothing to explain."
  [suggestion items]
  (when (and (seq suggestion) (seq items))
    (doseq [line (wrap-words suggestion-width suggestion)]
      (println (str "  " line)))
    (println)))

(defn- section-header [title items blocking?]
  (str "\n=== " title " (" (count items) ")"
       (when blocking? " [BLOCKING]")
       " ==="))

(defn print-section
  ([title items] (print-section title items false nil))
  ([title items blocking?] (print-section title items blocking? nil))
  ([title items blocking? suggestion]
   (println (section-header title items blocking?))
   (print-suggestion suggestion items)
   (if (empty? items)
     (println "  (none)")
     (doseq [item (sort-by (comp str :kw) items)]
       (println (format-entry item))
       (some-> (hint-line item) println)))))

(defn print-dynamic-section
  ([title items] (print-dynamic-section title items false nil))
  ([title items blocking?] (print-dynamic-section title items blocking? nil))
  ([title items blocking? suggestion]
   (println (section-header title items blocking?))
   (print-suggestion suggestion items)
   (if (empty? items)
     (println "  (none)")
     (doseq [{:keys [file form row]
              :as item} (sort-by :file items)]
       (println (format "  %s:%d  %s" file row (str/trim form)))
       (some-> (hint-line item) println)))))

(defn- key->title [k]
  (-> (name k)
      (str/replace #"-" " ")
      str/capitalize))

(defn- blocking-rule? [fail-on-rules rule-key]
  (and (seq fail-on-rules) (contains? fail-on-rules rule-key)))

(defn report
  ([result] (report result nil nil))
  ([result fail-on-rules] (report result fail-on-rules nil))
  ([result fail-on-rules suggestions]
   (doseq [[rule-key items] result
           :when (sequential? items)]
     (let [blocking? (blocking-rule? fail-on-rules rule-key)
           suggestion (get suggestions rule-key)]
       (if (and (seq items) (:form (first items)))
         (print-dynamic-section (key->title rule-key) items blocking? suggestion)
         (print-section (key->title rule-key) items blocking? suggestion))))))

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
    :fail-on-rules - if non-empty, section headers for matching rules get [BLOCKING]
    :suggestions - rule-key -> explanation, printed under a non-empty section"
  ([result new-identities]
   (report-with-baseline result new-identities {}))
  ([result new-identities {:keys [quiet? source-dir fail-on-rules suggestions]}]
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
         (print-suggestion (get suggestions rule-key) items-to-show)
         (doseq [item (sort-by (if dynamic? :file (comp str :kw)) items-to-show)]
           (let [id (baseline/issue->identity rule-key item source-dir)
                 tag (if (contains? new-identities id) "[NEW]" "[BASE]")]
             (println (if dynamic?
                        (format-tagged-dynamic item tag)
                        (format-tagged-entry item tag)))
             (some-> (hint-line item) println))))))))
