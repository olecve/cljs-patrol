(ns cljs-patrol.groups.comments
  "Comments rule group: comments that are code rather than prose.

  A `;` comment block whose text reads back as Clojure forms is code switched off. It goes
  stale while the code around it moves, and version control already keeps what it held.
  The reading is strict so that prose is never taken for code: every form in the block
  must be a list, a vector or a map, and the block must hold a call, a list headed by a
  symbol, along with something prose does not write, a keyword, a string, a number, a
  qualified symbol or a form nested in another. A shape written to document data,
  `[:dispatch [::kw args...]]`, holds a placeholder and is left alone."
  (:require
   [cljs-patrol.group :as group]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(def ^:private snippet-max-length 80)

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

(defn- code-token?
  "True when the token node is one prose does not write: a keyword, a string, a number or a qualified symbol."
  [node]
  (let [text (n/string node)]
    (or (str/starts-with? text ":")
        (str/starts-with? text "\"")
        (re-matches #"[+-]?\d.*" text)
        (re-matches #"[^/]+/.+" text))))

(defn- code-signal?
  "True when a form holds something prose does not: a code token, or a form nested in it."
  [form]
  (boolean (some (fn [node]
                   (or (contains? #{:list :map :multi-line :set :vector} (n/tag node))
                       (and (= :token (n/tag node)) (code-token? node))))
                 (rest (tree-seq n/inner? n/children form)))))

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

(defn- code?
  "True when `text` reads back as Clojure forms, each a list, a vector or a map, with a call, a code signal and no placeholder.
  A `=>` marks a REPL example showing a result, which documents code rather than switching it off."
  [text]
  (and (not (str/includes? text "=>"))
       (let [forms (try (remove n/whitespace-or-comment? (n/children (p/parse-string-all text)))
                        (catch Exception _ nil))
             nodes (descendants-of forms)]
         (boolean (and (seq forms)
                       (every? #(contains? form-tags (n/tag %)) forms)
                       (some code-signal? forms)
                       (some call? nodes)
                       (not-any? placeholder? nodes))))))

(defn- snippet [text]
  (let [line (first (str/split-lines text))]
    (if (<= (count line) snippet-max-length)
      line
      (str (subs line 0 (- snippet-max-length 3)) "..."))))

(defn- handle-file
  "Emit a :commented-out-code finding for each comment block that reads back as code."
  [zloc _ns-info file]
  (let [findings (keep (fn [block]
                         (let [text (comment-text block)]
                           (when (code? text)
                             (let [[row col] (z/position (first block))]
                               {:type :commented-out-code
                                :kw 'comment
                                :form (snippet text)
                                :file file
                                :row row
                                :col col}))))
                       (comment-blocks zloc))]
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
