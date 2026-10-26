(ns cljs-patrol.reporters.console
  "Console output laid out like ESLint's stylish format.
  Findings are grouped by file, one line each, then come the totals and the docs of the rules that fired."
  (:require
   [clojure.string :as str]))

(def ^:private subject-width 80)

(defn- subject [{:keys [kw form]}]
  (let [text (if form
               (str/replace (str/trim form) #"\s+" " ")
               (str kw))]
    (if (> (count text) subject-width)
      (str (subs text 0 (dec subject-width)) "\u2026")
      text)))

(defn- message
  "Name a finding by its hint when it has one. A form's line already shows the form, a name's line does not."
  [{:keys [form hint]
    :as item}]
  (cond
    (and hint form) hint
    hint (str (subject item) ": " hint)
    :else (subject item)))

(defn finding [rule-key severity item]
  {:col (:col item)
   :file (str (:file item))
   :message (message item)
   :row (:row item)
   :rule rule-key
   :severity severity})

(defn- position [{:keys [row col]}]
  (if col (str row ":" col) (str row)))

(defn- widest [f rows]
  (apply max (map (comp count f) rows)))

(defn- pad-right [text width]
  (format (str "%-" width "s") text))

(defn- file-lines [file findings]
  (let [rows (sort-by (juxt :row #(or (:col %) 0)) findings)
        position-width (widest position rows)
        severity-width (widest :severity rows)
        message-width (widest :message rows)]
    (cons file
          (map (fn [row]
                 (str "  " (format (str "%" position-width "s") (position row))
                      "  " (pad-right (:severity row) severity-width)
                      "  " (pad-right (:message row) message-width)
                      "  " (name (:rule row))))
               rows))))

(defn- counted [n word]
  (str n " " word (when (not= 1 n) "s")))

(defn- totals-line [findings baseline]
  (let [by-severity (frequencies (map :severity findings))
        problems (- (count findings) (get by-severity "baseline" 0))
        baseline-part (when baseline
                        (str " \u00b7 " (:present baseline) " in baseline, " (:fixed baseline) " fixed"))]
    (if (zero? problems)
      (str "\u2714 No " (when baseline "new ") "problems" baseline-part)
      (str "\u2716 " (counted problems "problem")
           " (" (counted (get by-severity "error" 0) "error")
           ", " (counted (get by-severity "warning" 0) "warning")
           (when-let [info (get by-severity "info")]
             (str ", " info " info"))
           ")" baseline-part))))

(defn- docs-lines [findings docs]
  (let [fired (->> findings (map :rule) distinct (filter #(get docs %)) (sort-by name))]
    (when (seq fired)
      (let [rule-width (widest name fired)]
        (cons "Rule docs:"
              (map #(str "  " (pad-right (name %) rule-width) "  " (docs %)) fired))))))

(defn print-report
  "Print `findings` grouped by file, then the totals and a docs link per rule that fired.
  Options:
    :docs - rule-key -> docs URL
    :baseline - {:present n :fixed n}, for a run compared against a baseline"
  [findings {:keys [docs baseline]}]
  (doseq [[file file-findings] (sort-by key (group-by :file findings))]
    (println)
    (run! println (file-lines file file-findings)))
  (println)
  (println (totals-line findings baseline))
  (when (pos? (:fixed baseline 0))
    (println (str (:fixed baseline) " baseline issues no longer present - consider running --baseline-write"
                  " to refresh.")))
  (when-let [lines (docs-lines findings docs)]
    (println)
    (run! println lines)))
