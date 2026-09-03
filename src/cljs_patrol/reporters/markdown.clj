(ns cljs-patrol.reporters.markdown
  "Markdown output for cljs-patrol analysis results, optimized for AI-assisted remediation."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [clojure.string :as str]))

(defn- hint-suffix
  "Return an indented follow-up line naming the fix, or \"\" when the rule attached none."
  [{:keys [hint]}]
  (if hint (str "\n  - " hint) ""))

(defn- format-entry [{:keys [file kw row]
                      :as item}]
  (str "- `" kw "` — `" (fs/absolute-path file) ":" row "`" (hint-suffix item)))

(defn- format-dynamic-entry [{:keys [file form row]
                              :as item}]
  (str "- `" (fs/absolute-path file) ":" row "` — `" (str/trim form) "`" (hint-suffix item)))

(defn- section [title suggestion items]
  (when (seq items)
    (str "## " title " (" (count items) ")\n\n"
         (if (:form (first items))
           (str/join "\n" (map format-dynamic-entry (sort-by :file items)))
           (str/join "\n" (map format-entry (sort-by (comp str :kw) items))))
         "\n\n> " suggestion "\n")))

(defn- merge-results [results]
  (apply merge-with (fn [a b] (if (sequential? a) (into a b) b)) results))

(defn print-report [enabled-groups _dirs run-results]
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
