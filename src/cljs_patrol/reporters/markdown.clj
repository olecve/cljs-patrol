(ns cljs-patrol.reporters.markdown
  "Markdown output for cljs-patrol analysis results, optimized for AI-assisted remediation."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [clojure.string :as str])
  (:import
   (java.nio.file
    Path)))

(defn- hint-suffix
  "Return an indented follow-up line naming the fix, or \"\" when the rule attached none."
  [{:keys [hint]}]
  (if hint (str "\n  - " hint) ""))

(defn- format-entry [file-system {:keys [file kw row]
                                  :as item}]
  (str "- `" kw "` — `" (fs/absolute-text file-system file) ":" row "`" (hint-suffix item)))

(defn- format-dynamic-entry [file-system {:keys [file form row]
                                          :as item}]
  (str "- `" (fs/absolute-text file-system file) ":" row "` — `" (str/trim form) "`" (hint-suffix item)))

(defn- section [file-system title {:keys [text docs]} items]
  (when (seq items)
    (str "## " title " (" (count items) ")\n\n"
         (if (:form (first items))
           (->> items (sort-by :file) (map #(format-dynamic-entry file-system %)) (str/join "\n"))
           (->> items (sort-by (comp str :kw)) (map #(format-entry file-system %)) (str/join "\n")))
         "\n\n> " text (when docs (str " [Docs](" docs ")")) "\n")))

(defn- merge-results [results]
  (apply merge-with (fn [a b] (if (sequential? a) (into a b) b)) results))

(defn print-report
  "Print run-results as Markdown, each file path absolute on the file system of the first of `sources`."
  [enabled-groups sources run-results]
  (let [file-system (.getFileSystem ^Path (:path (first sources)))
        sections (for [group-idx (range (count enabled-groups))
                       :let [group (nth enabled-groups group-idx)
                             merged (merge-results (map #(nth (:group-results %) group-idx) run-results))
                             guides (group/guides group)]
                       [issue-key items] (sort-by key merged)
                       :when (and (sequential? items) (seq items))
                       :let [title (-> (name issue-key) (str/replace #"-" " ") str/capitalize)
                             guide (get guides issue-key)]]
                   (section file-system title guide items))
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
