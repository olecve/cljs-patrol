(ns cljs-patrol.groups.comments
  "Comments rule group: comments that are code rather than prose, or that only restate the code.

  `restating-comment` flags a one-line comment that says in words what the line after it
  does, `;; Create a project` above `(post-project …)`. `commented-out-code` flags the rest.

  A `;` comment block whose text reads back as Clojure forms is code switched off. It goes
  stale while the code around it moves, and version control already keeps what it held.
  The reading is strict, to keep prose out: every form in the block must be a list, a
  vector or a map, and the block must hold a call whose head reads as code, such as
  `(swap! x f)`, `(= a b)`, `(defn …)` or a call through an alias the ns form requires,
  or a `#(…)`, a `#\"regex\"` or a `::keyword`. A word only prose writes, a shape holding a
  placeholder such as `[:dispatch [::kw args...]]`, and an example a comment ending in
  `:` introduces are left alone."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn- raw-locs
  "Return every loc of the file `zloc` sits in, whitespace and comments included, in order."
  [zloc]
  (->> (or (z/up zloc) zloc)
       (iterate z/next*)
       (take-while #(and % (not (z/end? %))))))

(defn- own-line?
  "True when nothing but indentation comes before the comment at `loc` on its line."
  [loc]
  (loop [left (z/left* loc)]
    (case (some-> left z/tag)
      nil true
      :whitespace (recur (z/left* left))
      (:newline :comment) true
      false)))

(defn- inside-comment-form?
  "True when a `(comment …)` encloses loc: code kept for the REPL on purpose."
  [loc]
  (boolean (some #(and (= :list (z/tag %))
                       (= "comment" (some-> % z/down z/string)))
                 (take-while some? (rest (iterate z/up loc))))))

(defn- comment-blocks
  "Group the own-line comments of a file into blocks of consecutive lines at one column."
  [zloc]
  (->> (raw-locs zloc)
       (filter #(and (= :comment (z/tag %)) (own-line? %) (not (inside-comment-form? %))))
       (reduce (fn [blocks loc]
                 (let [[row col] (parser/position loc)
                       [last-row last-col] (some-> blocks peek peek parser/position)]
                   (if (and (= col last-col) (= row (inc last-row)))
                     (conj (pop blocks) (conj (peek blocks) loc))
                     (conj blocks [loc]))))
               [])))

(defn- comment-text
  "Return the text of a comment block with each line's `;` markers and one space after them removed."
  [block]
  (->> block
       (map #(-> % z/node n/string str/trim-newline (str/replace #"^;+ ?" "")))
       (str/join "\n")))

(def ^:private form-tags #{:list :map :vector})

(def ^:private code-heads
  "Heads that read as code and as no English word, so a call to one is code wherever it is written."
  #{"assoc" "assoc-in" "cond" "condp" "conj" "dec" "def" "defmacro" "defmethod" "defn" "defn-" "defonce"
    "dissoc" "doseq" "dotimes" "fn" "get-in" "if-let" "inc" "let" "letfn" "println" "prn" "recur" "str"
    "update-in" "when-let" "when-not"})

(defn- alias-head?
  "True when `text` names a var through an alias the ns form requires, or `js/`."
  [text {:keys [aliases]}]
  (let [[_ alias-part] (re-matches #"([^/:\"]+)/.+" text)]
    (boolean (and alias-part (or (= "js" alias-part) (contains? aliases alias-part))))))

(defn- code-call?
  "True when the node is a call whose head reads as code.

  An operator, a head of [[code-heads]], or a call through a required alias. A head
  ending in `!` or `?` counts only with arguments after it: alone in parentheses it is as
  likely the end of a sentence, `(works for now!)`."
  [node ns-info]
  (and (= :list (n/tag node))
       (let [[head & arguments] (remove n/whitespace-or-comment? (n/children node))
             text (some-> head n/string)]
         (and (= :token (some-> head n/tag))
              (or (some? (re-matches #"[-+*/<>=]+|->>?|not=" text))
                  (contains? code-heads text)
                  (alias-head? text ns-info)
                  (and (seq arguments) (some? (re-find #"[^!?][!?]$" text))))))))

(defn- code-token?
  "True when the node is a `#(…)`, a `#\"regex\"` or a `::keyword`, which prose never writes."
  [node]
  (or (contains? #{:fn :regex} (n/tag node))
      (and (= :token (n/tag node)) (str/starts-with? (n/string node) "::"))))

(defn- code-signal?
  "True when a form holds a call whose head reads as code, or a token prose never writes, at any depth."
  [form ns-info]
  (boolean (some #(or (code-call? % ns-info) (code-token? %))
                 (tree-seq n/inner? n/children form))))

(defn- descendants-of [forms]
  (mapcat #(tree-seq n/inner? n/children %) forms))

(defn- call? [node]
  (and (= :list (n/tag node))
       (let [head (first (remove n/whitespace-or-comment? (n/children node)))]
         (and (= :token (some-> head n/tag))
              (symbol? (try (n/sexpr head) (catch Exception _ nil)))))))

(defn- placeholder?
  "True when the token node stands for something left out: `...`, `…`, `args...`, or a lone capital such as `N`."
  [node]
  (and (= :token (n/tag node))
       (let [text (n/string node)]
         (or (str/ends-with? text "...")
             (str/includes? text "…")
             (re-matches #"[A-Z]" text)))))

(def ^:private prose-words
  "Words prose writes and no Clojure code names, read without a trailing dot, so `e.g` counts as `e.g.` does."
  #{"a" "an" "are" "as" "at" "be" "but" "by" "cf" "e.g" "etc" "from" "i.e" "in" "of" "on" "see"
    "than" "that" "the" "this" "to" "via" "vs" "was" "were" "which" "with" "you"})

(defn- prose-word?
  "True when the token node is a word of prose: one of `prose-words`, or a capitalized word such as `RFC`.
  A class name, `(catch Exception e …)`, reads as one too, which only costs a finding."
  [node]
  (and (= :token (n/tag node))
       (let [text (n/string node)]
         (or (contains? prose-words (str/lower-case (str/replace text #"\.$" "")))
             (some? (re-matches #"[A-Z][a-z]*[A-Z]*" text))))))

(defn- code-forms
  "Return the forms `text` reads back as when it is code, or nil.

  Each form is a list, a vector or a map; one is a call; one holds a code signal; and none
  holds a placeholder or a word of prose. A `=>` marks a REPL example showing a result,
  which documents code rather than switching it off, and `,,,` a placeholder the reader
  takes for whitespace."
  [text ns-info]
  (when-not (or (str/includes? text "=>") (str/includes? text ",,,"))
    (let [forms (try (remove n/whitespace-or-comment? (n/children (p/parse-string-all text)))
                     (catch Exception _ nil))
          nodes (descendants-of forms)]
      (when (and (seq forms)
                 (every? #(contains? form-tags (n/tag %)) forms)
                 (some #(code-signal? % ns-info) forms)
                 (some call? nodes)
                 (not-any? placeholder? nodes)
                 (not-any? prose-word? nodes))
        forms))))

(defn- introduced-example?
  "True when the comment just before `block`, a blank line apart at most, ends in `:`, as `;; Example:` does."
  [block previous]
  (let [last-comment (peek previous)]
    (boolean (and last-comment
                  (<= (- (parser/position-row (first block)) (parser/position-row last-comment)) 2)
                  (str/ends-with? (str/trim (n/string (z/node last-comment))) ":")))))

(def ^:private action-verbs
  "Verbs a step comment opens with, `;; Create a project`, in any of their forms."
  #{"add" "build" "call" "check" "compute" "convert" "create" "delete" "duplicate" "export" "fetch" "filter"
    "find" "get" "load" "make" "parse" "remove" "render" "return" "save" "send" "set" "sort" "update" "view"})

(def ^:private restate-max-words
  "The most words a comment may hold and still be read as only restating the line after it."
  6)

(def ^:private stopwords
  "Words a comment holds around what it names without naming anything."
  #{"a" "all" "an" "and" "by" "for" "from" "in" "into" "it" "its" "of" "on" "or" "the" "then" "this" "to" "with"})

(defn- stem
  "Fold a plural or a third person onto its base, so `documents` matches the `document` of a name."
  [word]
  (cond
    (str/ends-with? word "ies") (str (subs word 0 (- (count word) 3)) "y")
    (re-find #"(s|x|z|ch|sh)es$" word) (subs word 0 (- (count word) 2))
    (and (str/ends-with? word "s") (not (str/ends-with? word "ss"))) (subs word 0 (dec (count word)))
    :else word))

(defn- words [text]
  (re-seq #"(?U)[\p{L}\p{N}]+" (str/lower-case text)))

(defn- restates?
  "True when a one-line comment only says, in words, what the source line after it does.

  It opens with an action verb, holds `restate-max-words` words at most and no punctuation,
  and every word after the verb, stopwords aside, is a word of the next line: `;; Fetch
  tagged documents` above `(fetch-tagged documents)`. Only the line's symbols count, not
  its strings or keywords, which say what the code handles rather than what it does. A
  reason or a condition brings words the line does not hold, so a comment giving one is
  left alone."
  [text next-line]
  (let [[verb & others] (words text)
        named (remove stopwords others)
        code-words (->> (str/replace next-line #"\"(?:[^\"\\]|\\.)*\"|:{1,2}[^\s()\[\]{}\"]+" " ")
                        words
                        (map stem)
                        set)]
    (boolean (and verb
                  (contains? action-verbs (stem verb))
                  (<= (count (words text)) restate-max-words)
                  (not (re-find #"[()\[\]{}#=?:`]|https?://" text))
                  (seq named)
                  (every? #(contains? code-words (stem %)) named)))))

(defn- labelled-line?
  "True when `line` opens a definition or a data entry, which a comment above it names as a label.
  `;; Filter panel` above `(defn filter-panel …)`, `(rf/reg-event-fx ::save …)`, `[\"/export\" …]` or `:settings/x …`."
  [line]
  (boolean (re-find #"^(\[|:|\((?:[\w.-]+/)?(?:def|reg-))" line)))

(defn- restating-findings
  "Return a :restating-comment finding for each comment that restates the source line after it.
  A line of a longer block has another comment after it, which is no code, so only a one-line block counts."
  [blocks source-lines file]
  (keep (fn [block]
          (let [[row col] (parser/position (first block))
                text (comment-text block)
                next-line (str/trim (get source-lines row ""))]
            (when (and (not (str/starts-with? next-line ";"))
                       (not (labelled-line? next-line))
                       (restates? text next-line))
              {:type :restating-comment
               :kw 'comment
               :form (parser/normalize-form (str ";; " text " " next-line))
               :file file
               :row row
               :col col})))
        blocks))

(defn- handle-file
  "Emit the :commented-out-code and :restating-comment findings of a file.

  A comment block that reads back as code is commented out, and a one-line comment that
  says what the line after it does restates it.

  The finding points at the first form the block holds, and its `:form` is every form,
  so two blocks opening the same way stay two baseline entries."
  [zloc ns-info file]
  (let [blocks (comment-blocks zloc)
        findings (keep (fn [[previous block]]
                         (when-let [forms (and (not (introduced-example? block previous))
                                               (code-forms (comment-text block) ns-info))]
                           (let [[row col] (parser/position (first block))]
                             {:type :commented-out-code
                              :kw 'comment
                              :form (->> forms (map n/string) (str/join " ") parser/normalize-form)
                              :file file
                              :row (+ row (-> forms first meta :row dec))
                              :col col})))
                       (map vector (cons nil blocks) blocks))
        restating (restating-findings blocks (str/split-lines (z/root-string zloc)) file)
        findings (concat findings restating)]
    (when (seq findings)
      {:decls (vec findings)
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:commented-out-code (filterv #(= :commented-out-code (:type %)) declarations)
   :restating-comment (filterv #(= :restating-comment (:type %)) declarations)})

(defn- summary-lines* [{:keys [commented-out-code restating-comment]}]
  [["Commented-out code:" (count commented-out-code)]
   ["Restating comment:" (count restating-comment)]])

(defrecord CommentsGroup [extensions]
  group/RuleGroup
  (group-id [_] :comments)
  (group-name [_] "Comments")
  (parse-handlers [_] {:handle-file handle-file})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  ;; Comments have no runtime effect. A project that wants them enforced opts in with --fail-on.
  (failed? [_ _] false)
  (suggestions [_]
    {:commented-out-code (str "This comment is code switched off. Delete it: version control keeps it. "
                              "Keep code meant for the REPL in a (comment …) form instead.")
     :restating-comment (str "This comment says in words what the next line does. Delete it, or say why the "
                             "line is there.")})
  (docs-anchors [_] {:commented-out-code "commented-out-code"
                     :restating-comment "restating-comment"})
  (rule->tier [_] {:commented-out-code :cleanup
                   :restating-comment :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return a comments RuleGroup.

  The group has no config keys of its own, so `config` is accepted and ignored, the way
  the docstrings group does. `clj?` is the experimental `.clj` mode."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([_config clj?] (->CommentsGroup (group/extensions-for clj?))))

(def group (make-group))
