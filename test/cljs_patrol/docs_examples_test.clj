(ns cljs-patrol.docs-examples-test
  "Run every example in docs/rules/ through the rule it illustrates.

  A rule's section opens with its key, then a block whose `;; flagged` part holds code the
  rule reports and whose `;; fix` part holds the same code rewritten, and a `**Left alone:**`
  block of code it does not report. Top-level forms with no blank line between them make
  one chunk, analyzed as a file of its own, so an example cannot lean on another, and the
  docs cannot drift from the tool."
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]))

(def ^:private assemble-groups #'core/assemble-groups)

(def ^:private docs-config
  "Config the examples run under, for a rule that does nothing until it is configured.
  An example calls `(field-label)`, which resolves to a var of the example's own namespace."
  {:a11y {:label-styles #{'example.core/field-label}}
   :focus-ring {:approved-shadows #{'app.styles.focus-rings/primary-shadow}
                :allow-stacking-under #{":focus-visible"}
                :required-companion {:outline "2px solid transparent"}
                :forbidden-tokens {'app.theme.button/shadow-brand-focused 'app.styles.focus-rings/primary-shadow}
                :forbidden-color-literals #{"#516bff"}}})

(def ^:private default-requires
  "Aliases an example may use without writing its own ns form."
  (str "(:require [re-frame.core :as rf]"
       " [spade.core :refer [defclass defattrs]]"
       " [clojure.string :as str]"
       " [clojure.test :refer [deftest is testing]]"
       " [react-testing-library-cljs.screen :as s]"
       " [app.styles.focus-rings :as focus-rings]"
       " [app.theme.button :as button-theme])"))

(defn- chunks
  "Split code into chunks of top-level forms, a blank line between two forms ending a chunk.

  Read with the parser rather than split on blank lines, so a blank line inside a form, a
  docstring's paragraph break, stays in its chunk. Code that does not parse throws."
  [code]
  (let [lines (vec (str/split-lines code))
        spans (->> (p/parse-string-all code)
                   n/children
                   (remove n/whitespace-or-comment?)
                   (map (comp (juxt :row :end-row) meta)))
        groups (reduce (fn [groups [row end-row]]
                         (let [previous-end (-> groups peek peek second)]
                           (if (and previous-end (<= row (inc previous-end)))
                             (conj (pop groups) (conj (peek groups) [row end-row]))
                             (conj groups [[row end-row]]))))
                       []
                       spans)]
    (for [group groups
          :let [start (first (first group))
                end (second (peek group))]]
      (->> (subvec lines (dec start) end)
           (str/join "\n")))))

(defn- code-blocks [text]
  (map second (re-seq #"(?s)```clojure\n(.*?)```" text)))

(defn- heading-anchor
  "Return the anchor GitHub gives a heading."
  [heading]
  (-> heading
      str/lower-case
      (str/replace #"[^\p{L}\p{N}_\- ]" "")
      (str/replace " " "-")))

(defn- rule-sections
  "Return {:rule :anchor :flagged :fix :left-alone :links} for every rule section of a docs page.

  A section is the text under one heading; it is a rule's when it holds a key line."
  [text]
  (for [section (rest (str/split text #"(?m)^#+ "))
        :let [rule (some->> section (re-find #"(?m)^`([^`]+)` ·") second keyword)
              example (first (code-blocks section))
              left-alone (some->> (str/split section #"(?m)^\*\*Left alone:\*\*$" 2) second code-blocks first)
              [_ flagged fix] (when example (re-find #"(?s);; flagged\n(.*?)(?:;; fix\n(.*))?$" example))]
        :when rule]
    {:rule rule
     :anchor (-> section str/split-lines first heading-anchor)
     :flagged (some-> flagged chunks)
     :fix (some-> fix chunks)
     :left-alone (some-> left-alone chunks)
     :links (->> (str/split section #"\n```" 2) first (re-seq #"\]\(#([^)]+)\)") (map second))}))

(defn- anchors
  "Return the anchors GitHub gives the headings of a page."
  [text]
  (->> (re-seq #"(?m)^#+ (.*)$" text)
       (map (fn [[_ heading]] (heading-anchor heading)))
       set))

(defn- findings
  "Analyze code as the only file of a project, and return the rule's finding count and what the run warned."
  [rule-group rule code]
  (let [test-ns? (= :tests (group/group-id rule-group))
        ns-name (if test-ns? "example.core-test" "example.core")
        source (if (str/starts-with? code "(ns ")
                 code
                 (str "(ns " ns-name " " default-requires ")\n\n" code "\n"))
        {:keys [results errors]} (temp-project/run-group rule-group
                                                         {(if test-ns? "core_test.cljs" "core.cljs") source})]
    {:count (count (get results rule))
     :warnings (str/replace errors #"(?m)^Analyzing .*\n?" "")}))

(defn- declared-rules
  "Return every rule a group declares, however it declares it."
  [rule-group]
  (into (set (keys (group/suggestions rule-group)))
        (keys (group/rule->tier rule-group))))

(deftest rule-examples-match-the-tool
  (let [groups (into {} (map (juxt group/group-id identity)) (assemble-groups docs-config false))
        pages (->> (io/file "docs/rules") .listFiles (filter #(str/ends-with? (.getName ^java.io.File %) ".md")))]
    (is (seq pages))
    (doseq [^java.io.File page pages
            :let [group-id (-> page .getName (str/replace #"\.md$" "") keyword)
                  rule-group (get groups group-id)
                  text (slurp page)
                  sections (rule-sections text)]]
      (testing (.getName page)
        (is (some? rule-group)
            "every page is named after a group")
        (is (= (declared-rules rule-group) (set (map :rule sections)))
            "every rule the group declares has a section with its key line, and no section names another")
        (doseq [{:keys [rule flagged fix left-alone links]} sections]
          (testing (name rule)
            (is (seq flagged)
                "every rule shows code it flags")
            (doseq [anchor links]
              (is (contains? (anchors text) anchor)
                  (str "a link in the key line points at no heading: #" anchor)))
            (doseq [code flagged
                    :let [{:keys [count warnings]} (findings rule-group rule code)]]
              (is (str/blank? warnings)
                  (str "flagged example does not parse:\n" code "\n" warnings))
              (is (pos? count)
                  (str "flagged example is not flagged:\n" code)))
            (doseq [code (concat fix left-alone)
                    :let [{:keys [count warnings]} (findings rule-group rule code)]]
              (is (str/blank? warnings)
                  (str "fix or left-alone example does not parse:\n" code "\n" warnings))
              (is (zero? count)
                  (str "fix or left-alone example is flagged:\n" code)))))))))

(def ^:private suggestion-limit
  "Characters a suggestion may spend: a sentence or two naming the fix, the docs carrying the rest."
  280)

(deftest suggestions-are-short-and-anchor-their-docs
  (doseq [rule-group (assemble-groups {} false)
          :let [page (name (group/group-id rule-group))
                text (slurp (io/file "docs/rules" (str page ".md")))
                rule->anchor (->> (rule-sections text) (map (juxt :rule :anchor)) (into {}))
                anchors (group/docs-anchors rule-group)]]
    (testing page
      (is (= (set (keys (group/suggestions rule-group))) (set (keys anchors)))
          "every rule with a suggestion has a docs anchor, and no other rule does")
      (doseq [[rule suggestion] (group/suggestions rule-group)]
        (testing (name rule)
          (is (<= (count suggestion) suggestion-limit)
              (str "suggestion runs past " suggestion-limit " characters: " (count suggestion)))
          (is (= (rule->anchor rule) (get anchors rule))
              (str "anchor #" (get anchors rule) " is not rule's section #" (rule->anchor rule))))))))

(deftest docs-headings-have-distinct-anchors
  (doseq [^java.io.File page (.listFiles (io/file "docs/rules"))
          :when (str/ends-with? (.getName page) ".md")
          :let [headings (map second (re-seq #"(?m)^#+ (.*)$" (slurp page)))
                duplicates (->> headings (map heading-anchor) frequencies (filter #(< 1 (val %))) (map key))]]
    (is (empty? duplicates)
        (str (.getName page) ": headings share anchors GitHub would suffix, so a link misses: " (vec duplicates)))))
