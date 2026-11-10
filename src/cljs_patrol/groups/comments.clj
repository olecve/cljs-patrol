(ns cljs-patrol.groups.comments
  "Comments rule group: comments that are code rather than prose.

  A `;` comment block whose text reads back as Clojure forms is code switched off. It goes
  stale while the code around it moves, and version control already keeps what it held.
  The reading is strict so that prose is never taken for code: every form in the block
  must be a list, a vector or a map, and the block must hold a call, a list headed by a
  symbol, along with a token English never writes, such as `@x`, `swap!`, `->` or a
  call through an alias the ns form requires. A word only prose writes, a shape holding a
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
                 (let [[row col] (z/position loc)
                       [last-row last-col] (some-> blocks peek peek z/position)]
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

(def ^:private reader-tags
  "Reader forms English never writes: `@x`, `#(…)`, `'x`, `` `x ``, `#\"re\"`, `#{…}` and `#?(…)`."
  #{:deref :fn :quote :reader-macro :regex :set :syntax-quote})

(defn- alias-call-head?
  "True when the token node heads a call through an alias the ns form requires, or `js/`."
  [node parent {:keys [aliases]}]
  (let [text (n/string node)
        [_ alias-part] (re-matches #"([^/:\"]+)/.+" text)]
    (boolean (and alias-part
                  (= :list (n/tag parent))
                  (identical? node (first (remove n/whitespace-or-comment? (n/children parent))))
                  (or (= "js" alias-part) (contains? aliases alias-part))))))

(defn- code-token?
  "True when the node is one English never writes.

  A reader form, a symbol ending in `!` or `?`, an operator, an auto-resolved `::keyword`,
  a head of [[code-heads]] in call position, or a call through a required alias."
  [node parent ns-info]
  (or (contains? reader-tags (n/tag node))
      (and (= :token (n/tag node))
           (let [text (n/string node)
                 head? (and (= :list (some-> parent n/tag))
                            (identical? node (first (remove n/whitespace-or-comment? (n/children parent)))))]
             (or (some? (re-find #"[^!?][!?]$" text))
                 (some? (re-matches #"[-+*/<>=]+|->>?|not=" text))
                 (str/starts-with? text "::")
                 (and head? (contains? code-heads text))
                 (alias-call-head? node parent ns-info))))))

(defn- code-signal?
  "True when a form holds a token English never writes, at any depth."
  [form ns-info]
  (letfn [(walk [node parent]
            (or (code-token? node parent ns-info)
                (and (n/inner? node) (some #(walk % node) (n/children node)))))]
    (boolean (walk form nil))))

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

  Each form is a list, a vector or a map; one is a call; one holds a token English never
  writes; and none holds a placeholder or a word of prose. A `=>` marks a REPL example
  showing a result, which documents code rather than switching it off."
  [text ns-info]
  (when-not (str/includes? text "=>")
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
                  (<= (- (first (z/position (first block))) (first (z/position last-comment))) 2)
                  (str/ends-with? (str/trim (n/string (z/node last-comment))) ":")))))

(defn- handle-file
  "Emit a :commented-out-code finding for each comment block that reads back as code.

  The finding points at the first form the block holds, and its `:form` is every form,
  so two blocks opening the same way stay two baseline entries."
  [zloc ns-info file]
  (let [blocks (comment-blocks zloc)
        findings (keep (fn [[previous block]]
                         (when-let [forms (and (not (introduced-example? block previous))
                                               (code-forms (comment-text block) ns-info))]
                           (let [[row col] (z/position (first block))]
                             {:type :commented-out-code
                              :kw 'comment
                              :form (->> forms (map n/string) (str/join " ") parser/normalize-form)
                              :file file
                              :row (+ row (-> forms first meta :row dec))
                              :col col})))
                       (map vector (cons nil blocks) blocks))]
    (when (seq findings)
      {:decls (vec findings)
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:commented-out-code (vec (filter #(= :commented-out-code (:type %)) declarations))})

(defn- summary-lines* [{:keys [commented-out-code]}]
  [["Commented-out code:" (count commented-out-code)]])

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
                              "Keep code meant for the REPL in a (comment …) form instead.")})
  (docs-anchors [_] {:commented-out-code "commented-out-code"})
  (rule->tier [_] {:commented-out-code :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return a comments RuleGroup.

  The group has no config keys of its own, so `config` is accepted and ignored, the way
  the docstrings group does. `clj?` is the experimental `.clj` mode."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([_config clj?] (->CommentsGroup (group/extensions-for clj?))))

(def group (make-group))
