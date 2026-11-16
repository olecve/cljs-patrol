(ns cljs-patrol.docs-examples-test
  "Run every code block of the rule docs, so the docs cannot drift from the tool.

  A rule change that makes an example false fails the build until the docs agree, and a rule
  with no section fails it too. A block outside a rule's two example blocks runs as well: a
  config block must be a config the groups accept, and any other block is labeled examples. A rule's section opens with its key, then a block whose
  `;; flagged` part holds code the rule reports and whose `;; fix` part holds the same code
  rewritten, and a `**Left alone:**` block of code it does not report. Each example is one
  chunk, analyzed as a file of its own, so an example cannot lean on another. ADR 0004
  records why the examples are tests and how a block splits into examples."
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]))

(def ^:private assemble-groups #'core/assemble-groups)

(def ^:private docs-config
  "Config the examples run under, for a rule that does nothing until it is configured, or an example left alone by it.
  An example calls `(field-label)`, which resolves to a var of the example's own namespace, and one wraps its
  control in `app.ui.field/field`, a label component."
  {:a11y {:label-components {'app.ui.field/field :label}
          :label-styles #{'example.core/field-label}}
   :focus-ring {:approved-shadows {'app.styles.focus-rings/primary-shadow #{'app.theme.button/shadow-brand-focused
                                                                            "#516bff"}}}
   :forbid-props [{:prop :aria-disabled
                   :disallowed-for #{:button 'app.ui.button/button}
                   :message "Use :disabled."}]})

(def ^:private default-requires
  "Aliases an example may use without writing its own ns form."
  (str "(:require [re-frame.core :as rf]"
       " [spade.core :refer [defclass defattrs]]"
       " [clojure.string :as str]"
       " [clojure.test :refer [deftest is testing]]"
       " [react-testing-library-cljs.screen :as s]"
       " [app.styles.focus-rings :as focus-rings]"
       " [app.theme.button :as button-theme]"
       " [app.ui.button :as button]"
       " [my.ui :as ui]"
       " [my.ui.icons :as icons])"))

(defn- comment-item
  "Read a comment that opens a line: a `;; config {…}` line gives config, any other comment is a label."
  [node]
  (let [text (-> (n/string node) (str/replace #"^;+\s*" "") str/trim)]
    (if (str/starts-with? text "config {")
      {:config (edn/read-string (subs text 7))}
      {:label text})))

(defn- example-items
  "Return the top-level forms of code as `{:row :end-row}`, and each comment that opens a line as a comment item."
  [code]
  (->> (p/parse-string-all code)
       n/children
       (keep (fn [node]
               (let [{:keys [row col end-row]} (meta node)]
                 (cond
                   (= :comment (n/tag node)) (when (= 1 col) (comment-item node))
                   (n/whitespace-or-comment? node) nil
                   :else {:end-row end-row
                          :row row}))))))

(defn- start-example [{:keys [groups pending-label]
                       :as examples} {:keys [end-row row]}]
  (assoc examples
         :commented? (some? pending-label)
         :groups (conj groups {:end-row end-row
                               :label pending-label
                               :row row})
         :pending-label nil))

(defn- add-example-item
  "Add a comment or a form to the examples read so far.

  A comment marks the next form as the start of a labeled example. A form after a blank line
  starts a new example only when the current one has no label, since a label is what lets
  one example space out its forms."
  [{:keys [commented? groups pending-label]
    :as examples}
   {:keys [config end-row label row]
    :as item}]
  (let [previous-end (some-> groups peek :end-row)]
    (cond
      config (update examples :config #(merge-with merge % config))
      label (update examples :pending-label #(or % label))
      (or pending-label (nil? previous-end)) (start-example examples item)
      (and (not commented?) (< (inc previous-end) row)) (start-example examples item)
      :else (assoc-in examples [:groups (dec (count groups)) :end-row] end-row))))

(defn- labeled-examples
  "Split code into examples, and return `{:examples [{:code :label}] :config}`.

  `:label` is the comment that opens the example, or nil. `:config` merges the `;; config {…}`
  lines of the code, the config every example of the block runs under."
  [code]
  (let [lines (vec (str/split-lines code))
        {:keys [config groups]} (reduce add-example-item {:groups []} (example-items code))]
    {:config config
     :examples (for [{:keys [end-row label row]} groups]
                 {:code (->> (subvec lines (dec row) end-row)
                             (str/join "\n"))
                  :label label})}))

(defn- chunks
  "Split code into chunks of top-level forms, one chunk per example.

  A comment that opens a line starts a new example, which runs on until the next such
  comment, blank lines and all, so an example of two forms can space them out. An example
  that opens with no comment ends at a blank line. Read with the parser rather than split on
  lines, so a blank line inside a form, a docstring's paragraph break, stays in its chunk.
  Code that does not parse throws."
  [code]
  (map :code (:examples (labeled-examples code))))

(defn- symbol-names
  "Return the names of every symbol in node."
  [node]
  (->> (tree-seq n/inner? n/children node)
       (keep #(when (= :token (n/tag %))
                (let [value (n/sexpr %)]
                  (when (symbol? value) (name value)))))
       set))

(defn- defined-name
  "Return the name a `def`-like form defines, or nil."
  [node]
  (when (= :list (n/tag node))
    (let [[head defined] (remove n/whitespace-or-comment? (n/children node))]
      (when (and (some-> head n/sexpr symbol?)
                 (str/starts-with? (name (n/sexpr head)) "def")
                 (some-> defined n/sexpr symbol?))
        (name (n/sexpr defined))))))

(defn- unrelated-parts
  "Return the parts of an example, split at blank lines between top-level forms, that use no name an earlier part defines.

  An example that spans a blank line shows forms that lean on each other, such as a component and the
  button that renders it; a part that leans on nothing is another example missing its own comment."
  [code]
  (let [forms (->> (p/parse-string-all code)
                   n/children
                   (remove n/whitespace-or-comment?))
        parts (reduce (fn [parts form]
                        (let [previous-end (some-> parts peek peek meta :end-row)]
                          (if (and previous-end (< (inc previous-end) (:row (meta form))))
                            (conj parts [form])
                            (conj (pop parts) (conj (or (peek parts) []) form)))))
                      [[]]
                      forms)]
    (->> (map vector parts (reductions into #{} (map #(keep defined-name %) parts)))
         rest
         (remove (fn [[part defined]] (some defined (mapcat symbol-names part))))
         (map (fn [[part _]] (str/join "\n" (map n/string part)))))))

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
     :flagged (some->> flagged (str ";; flagged\n") chunks)
     :fix (some->> fix (str ";; fix\n") chunks)
     :left-alone (some-> left-alone chunks)
     :links (->> (str/split section #"\n```" 2) first (re-seq #"\]\(#([^)]+)\)") (map second))}))

(defn- anchors
  "Return the anchors GitHub gives the headings of a page."
  [text]
  (->> (re-seq #"(?m)^#+ (.*)$" text)
       (map (fn [[_ heading]] (heading-anchor heading)))
       set))

(defn- findings
  "Analyze code as the only file of a project, and return the finding count of `rules` and what the run warned."
  [rule-group rules code]
  (let [test-ns? (= :tests (group/group-id rule-group))
        ns-name (if test-ns? "example.core-test" "example.core")
        source (if (str/starts-with? code "(ns ")
                 code
                 (str "(ns " ns-name " " default-requires ")\n\n" code "\n"))
        {:keys [results errors]} (temp-project/run-group rule-group
                                                         {(if test-ns? "core_test.cljs" "core.cljs") source})]
    {:count (reduce + (map #(count (get results %)) rules))
     :warnings (str/replace errors #"(?m)^Analyzing .*\n?" "")}))

(def ^:private top-level-config-keys
  "Config keys that name no group."
  #{:baseline :experimental-clj :fail-on})

(def ^:private page-groups
  "The groups the labeled examples of a page run under, for a page not named after one group."
  {"rules.md" #{:a11y :forbid-props :reagent :react-testing-library-cljs}})

(defn- fenced-blocks
  "Return the fenced `clojure` and `edn` blocks of text as `{:code :lang}`."
  [text]
  (for [[_ lang code] (re-seq #"(?s)```(clojure|edn)\n(.*?)```" text)]
    {:code code
     :lang lang}))

(defn- other-blocks
  "Return the blocks of a page that are neither a rule's example block nor its Left alone block, with the section's rule."
  [text]
  (for [section (str/split text #"(?m)^#+ ")
        :let [rule (some->> section (re-find #"(?m)^`([^`]+)` ·") second keyword)
              rule-blocks (when rule
                            #{(first (code-blocks section))
                              (some->> (str/split section #"(?m)^\*\*Left alone:\*\*$" 2) second code-blocks first)})]
        block (fenced-blocks section)
        :when (not (and (= "clojure" (:lang block)) (contains? rule-blocks (:code block))))]
    (assoc block :rule rule)))

(defn- config-block?
  "True when a block is config: an `edn` block, or one map keyed by keywords."
  [{:keys [code lang]}]
  (or (= "edn" lang)
      (let [forms (->> (p/parse-string-all code) n/children (remove n/whitespace-or-comment?))]
        (and (= 1 (count forms))
             (= :map (n/tag (first forms)))
             (every? keyword? (keys (n/sexpr (first forms))))))))

(defn- expectations
  "Pair each example with `:flagged` or `:left-alone`, read from its label or, for a label that names neither, the one before."
  [examples]
  (rest (reductions (fn [[_ previous] {:keys [label]
                                       :as example}]
                      [example (cond
                                 (some->> label (re-find #"(?i)^flagged")) :flagged
                                 (some->> label (re-find #"(?i)^(left alone|fix)")) :left-alone
                                 :else previous)])
                    [nil nil]
                    examples)))

(defn- declared-rules
  "Return every rule a group declares, however it declares it."
  [rule-group]
  (into (set (keys (group/suggestions rule-group)))
        (keys (group/rule->tier rule-group))))

(deftest chunks-split-examples-at-comments
  (is (= ["(def a 1)" "(def b 2)"]
         (chunks "(def a 1)\n\n(def b 2)"))
      "a blank line ends an example that opens with no comment")
  (is (= ["(def a 1)\n\n(def b a)" "(def c 3)"]
         (chunks ";; a and b\n(def a 1)\n\n(def b a)\n;; c\n(def c 3)"))
      "commented example runs past a blank line to the next comment")
  (is (= ["(def a 1)" "(def b 2)"]
         (chunks "(def a 1)\n;; b\n(def b 2)"))
      "comment right after a form starts a new example")
  (is (= ["(def a 1) ;; note\n(def b a)"]
         (chunks "(def a 1) ;; note\n(def b a)"))
      "comment after code on the same line starts nothing"))

(deftest unrelated-parts-find-an-unlabeled-example
  (is (= ["(def b 2)"]
         (unrelated-parts "(def a 1)\n\n(def b 2)"))
      "part that uses nothing defined above is another example")
  (is (empty? (unrelated-parts "(defn- face [] [:span \"x\"])\n\n(defn button [] [:button [face]])"))
      "part that uses a name defined above belongs to the example"))

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
            (doseq [code (concat flagged fix left-alone)]
              (is (empty? (unrelated-parts code))
                  (str "example spans a blank line, but a part uses nothing defined above it; "
                       "give that part its own comment:\n" code)))
            (doseq [code flagged
                    :let [{:keys [count warnings]} (findings rule-group #{rule} code)]]
              (is (str/blank? warnings)
                  (str "flagged example does not parse:\n" code "\n" warnings))
              (is (pos? count)
                  (str "flagged example is not flagged:\n" code)))
            (doseq [code (concat fix left-alone)
                    :let [{:keys [count warnings]} (findings rule-group #{rule} code)]]
              (is (str/blank? warnings)
                  (str "fix or left-alone example does not parse:\n" code "\n" warnings))
              (is (zero? count)
                  (str "fix or left-alone example is flagged:\n" code)))))))))

(deftest every-code-block-is-checked
  (let [group-ids (set (map group/group-id (assemble-groups {} false)))
        config-keys (into top-level-config-keys group-ids)
        pages (->> (io/file "docs/rules")
                   .listFiles
                   (filter #(str/ends-with? (.getName ^java.io.File %) ".md"))
                   (concat [(io/file "docs/rules.md") (io/file "README.md")]))]
    (doseq [^java.io.File page pages
            :let [page-name (.getName page)
                  page-group-ids (get page-groups page-name #{(keyword (str/replace page-name #"\.md$" ""))})]
            {:keys [code rule]
             :as block} (other-blocks (slurp page))]
      (testing (str page-name " " (some-> rule name))
        (if (config-block? block)
          (let [config (edn/read-string code)]
            (is (every? config-keys (keys config))
                (str "config block names a key that is no group:\n" code))
            (is (seq (assemble-groups config false))
                (str "config block is not a config the groups take:\n" code)))
          (let [{:keys [config examples]} (labeled-examples code)
                groups (filter #(contains? page-group-ids (group/group-id %))
                               (assemble-groups (merge-with merge docs-config config) false))]
            (is (seq examples)
                (str "code block holds no example:\n" code))
            (doseq [[{:keys [code]} expectation] (expectations examples)
                    :let [runs (for [rule-group groups]
                                 (findings rule-group (if rule #{rule} (declared-rules rule-group)) code))
                          finding-count (reduce + (map :count runs))
                          warnings (str/join (map :warnings runs))]]
              (is (some? expectation)
                  (str "example has no label that starts with flagged or left alone:\n" code))
              (is (empty? (unrelated-parts code))
                  (str "example spans a blank line, but a part uses nothing defined above it:\n" code))
              (is (str/blank? warnings)
                  (str "example does not parse:\n" code "\n" warnings))
              (case expectation
                :flagged (is (pos? finding-count)
                             (str "flagged example is not flagged:\n" code))
                :left-alone (is (zero? finding-count)
                                (str "left-alone example is flagged:\n" code))
                nil))))))))

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
