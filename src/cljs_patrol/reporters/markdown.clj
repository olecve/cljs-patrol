(ns cljs-patrol.reporters.markdown
  "Markdown output for cljs-patrol analysis results, optimized for AI-assisted remediation."
  (:require
   [cljs-patrol.group :as group]
   [clojure.string :as str]))

(defn- absolutize [path]
  (.getAbsolutePath (java.io.File. path)))

(defn- format-entry [{:keys [file kw row]}]
  (str "- `" kw "` — `" (absolutize file) ":" row "`"))

(defn- format-dynamic-entry [{:keys [file form row]}]
  (str "- `" (absolutize file) ":" row "` — `" (str/trim form) "`"))

(defn- section [title suggestion items]
  (when (seq items)
    (str "## " title " (" (count items) ")\n\n"
         (if (:form (first items))
           (str/join "\n" (map format-dynamic-entry (sort-by :file items)))
           (str/join "\n" (map format-entry (sort-by (comp str :kw) items))))
         "\n\n> " suggestion "\n")))

(defn- merge-results [results]
  (apply merge-with (fn [a b] (if (sequential? a) (into a b) b)) results))

(defn print-report
  "Print analysis results as Markdown to stdout."
  [enabled-groups _dirs run-results]
  (let [sections (for [group-idx (range (count enabled-groups))
                       :let [group (nth enabled-groups group-idx)
                             merged (merge-results (map #(nth (:group-results %) group-idx) run-results))
                             suggestions (group/suggestions group)]
                       [issue-key items] (sort-by key merged)
                       :when (and (sequential? items) (seq items))
                       :let [title (-> (name issue-key) (str/replace #"-" " ") str/capitalize)
                             suggestion (get suggestions issue-key "")]]
                   (section title suggestion items))
        summary (for [group-idx (range (count enabled-groups))
                      :let [group (nth enabled-groups group-idx)
                            merged (merge-results (map #(nth (:group-results %) group-idx) run-results))]
                      [label count] (group/summary-lines group merged)]
                  (str "| " label " | " count " |"))
        output (str "# cljs-patrol report\n\n"
                    (str/join "\n" (remove nil? sections))
                    "\n## Summary\n\n"
                    "| Category | Count |\n| --- | --- |\n"
                    (str/join "\n" summary) "\n")]
    (print output)
    (flush)))
