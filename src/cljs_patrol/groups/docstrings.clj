(ns cljs-patrol.groups.docstrings
  "Docstrings rule group: enforces bbatsov clojure-style-guide docstring conventions.
  Checks docstring-summary, docstring-indentation, docstring-leading-trailing-whitespace and
  docstring-repeats-name on every def-form regardless of privacy (defn, defn-, def, defmacro,
  defmulti, defprotocol).
  For defprotocol, both the outer protocol docstring and each method signature's
  trailing docstring are checked."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private def-fns
  #{"defn" "defn-" "def" "defmacro" "defmulti" "defprotocol"})

(def ^:private name-title-abbreviations
  "Name- and place-title abbreviations stripped before run-on detection.

  Without stripping, `Mr. Smith` would look like two sentences. Excludes
  `e.g.`, `i.e.`, `etc.` — those legitimately introduce a second sentence
  and should remain detected."
  #{"Mr." "Mrs." "Ms." "Dr." "Prof." "Jr." "Sr." "St." "Inc." "Ltd."})

(defn- strip-abbreviation-periods [s]
  (reduce (fn [acc abbr]
            (str/replace acc abbr (subs abbr 0 (dec (count abbr)))))
          s
          name-title-abbreviations))

(defn- string-node? [loc]
  (and loc
       (contains? #{:token :multi-line} (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))))

(defn- name-sym
  "Return the symbol at name-loc (unwrapping any metadata), or nil."
  [name-loc]
  (try
    (let [value (z/sexpr name-loc)]
      (when (symbol? value) value))
    (catch Exception _ nil)))

(defn- find-docstring-loc
  "Return the docstring zip loc for a def-form, or nil.
  A string is treated as a docstring only when followed by at least one more
  node (so `(def foo \"v\")` is recognized as a value, not a docstring)."
  [def-loc]
  (let [op-loc (z/down def-loc)
        name-loc (some-> op-loc z/right)
        candidate (some-> name-loc z/right)]
    (when (and (string-node? candidate)
               (z/right candidate))
      candidate)))

(defn- multi-line? [content]
  (str/includes? content "\n"))

(defn- summary-violation?
  "Detect a missing or non-self-contained summary line in a multi-line docstring.

  The first line must end with a sentence terminator (`.`, `!`, `?`, or `:` for
  summaries that introduce an indented list/example) and not begin a new
  sentence on the same line. The run-on check also tolerates a closing bracket
  between the terminator and the next sentence (`(helper.) Used by ...`).
  False positives from Clojure-style identifiers (`:dynamic?`, `string?`) are
  skipped by requiring an upper-case letter after the terminator. Name and
  place-title abbreviations (`Mr.`, `St.`, ...) are stripped first so they do
  not look like sentence breaks."
  [content]
  (when (multi-line? content)
    (let [first-line (first (str/split content #"\n" 2))
          cleaned (strip-abbreviation-periods first-line)]
      (boolean
       (or (re-find #"[.!?][)\]}]?\s+[A-Z]" cleaned)
           (not (re-find #"[.!?:]\s*$" first-line)))))))

(def ^:private tab-width 4)

(defn- indentation-violation?
  "Detect under-indented continuation lines in a multi-line docstring.

  Each non-blank continuation line must have leading whitespace at least
  equal to `(column-of-opening-quote - 1)`. Tabs are expanded to `tab-width`
  spaces before counting."
  [content col]
  (when (multi-line? content)
    (let [expected (dec col)
          continuation (rest (str/split content #"\n" -1))]
      (boolean
       (some (fn [line]
               (let [expanded (str/replace line "\t" (apply str (repeat tab-width " ")))
                     trimmed (str/triml expanded)
                     leading (- (count expanded) (count trimmed))]
                 (and (seq trimmed)
                      (< leading expected))))
             continuation)))))

(defn- leading-trailing-violation? [^String content]
  (and (seq content)
       (or (Character/isWhitespace (.charAt content 0))
           (Character/isWhitespace (.charAt content (dec (count content)))))))

(def ^:private repeat-max-words
  "The most words a docstring may hold and still be read as only repeating its name."
  6)

(def ^:private filler-words
  "Words a docstring wraps a name in without saying more: articles, linking words, and verbs any function could claim."
  #{"a" "an" "and" "are" "as" "at" "be" "build" "builds" "by" "check" "checks" "compute" "computes" "create"
    "creates" "false" "fetch" "fetches" "for" "from" "get" "gets" "given" "handle" "handles" "if" "in" "is"
    "it" "its" "make" "makes" "of" "on" "or" "return" "returns" "set" "sets" "that" "the" "this" "to" "true"
    "when" "whether" "with"})

(defn- stem
  "Fold a plural onto its singular, so `users` matches the `user` of a name."
  [word]
  (cond
    (str/ends-with? word "ies") (str (subs word 0 (- (count word) 3)) "y")
    (re-find #"(s|x|z|ch|sh)es$" word) (subs word 0 (- (count word) 2))
    (and (str/ends-with? word "s") (not (str/ends-with? word "ss"))) (subs word 0 (dec (count word)))
    :else word))

(defn- words [text]
  (re-seq #"[a-z0-9]+" (str/replace (str/lower-case text) #"'s\b" "")))

(defn- repeats-name?
  "True when a one-line docstring says nothing its var's name does not.

  It holds `repeat-max-words` words at most, and once filler words are dropped, every word
  left, at least one, is a word of the name: `(defn get-user-data \"Gets user data.\")`."
  [content var-name]
  (let [all-words (words content)
        content-words (remove filler-words all-words)
        name-words (->> var-name words (map stem) set)]
    (boolean (and (not (multi-line? content))
                  (<= (count all-words) repeat-max-words)
                  (seq content-words)
                  (every? #(contains? name-words (stem %)) content-words)))))

(defn- docstring-issues [content col var-name]
  (let [leading-trailing? (leading-trailing-violation? content)]
    (cond-> []
      (repeats-name? content var-name)
      (conj :docstring-repeats-name)

      (summary-violation? content)
      (conj :docstring-summary)

      (and (not leading-trailing?) (indentation-violation? content col))
      (conj :docstring-indentation)

      leading-trailing?
      (conj :docstring-leading-trailing-whitespace))))

(defn- rightmost-sibling [loc]
  (loop [cur loc last-loc cur]
    (if-let [nxt (z/right cur)]
      (recur nxt nxt)
      last-loc)))

(defn- find-method-docstrings
  "Return [method-kw doc-loc] for each defprotocol method sig with a docstring.
  Skips the outer protocol docstring and method sigs without a trailing string."
  [defprotocol-loc ns-name]
  (let [op-loc (z/down defprotocol-loc)
        first-after-name (some-> op-loc z/right z/right)]
    (loop [loc first-after-name acc []]
      (if (nil? loc)
        acc
        (recur (z/right loc)
               (if (= :list (z/tag loc))
                 (let [first-child (z/down loc)
                       method-name (parser/sym-name first-child)
                       last-child (when first-child (rightmost-sibling first-child))]
                   (if (and method-name (string-node? last-child))
                     (conj acc [(keyword ns-name method-name) last-child])
                     acc))
                 acc))))))

(defn- issues-for-doc [kw doc-loc file]
  (let [content (try (z/sexpr doc-loc) (catch Exception _ ""))
        [row col] (try (z/position doc-loc) (catch Exception _ [0 1]))]
    (mapv (fn [t]
            {:kw kw
             :type t
             :file file
             :row row})
          (docstring-issues content col (name kw)))))

(defn- collect-doc-pairs [loc operator ns-name name-symbol]
  (let [primary-kw (keyword ns-name (name name-symbol))
        primary (when-let [d (find-docstring-loc loc)] [[primary-kw d]])
        methods (when (= "defprotocol" operator) (find-method-docstrings loc ns-name))]
    (concat primary methods)))

(defn- handle-list [loc {:keys [ns-name]} file]
  (let [op-loc (z/down loc)
        operator (parser/sym-name op-loc)]
    (when (contains? def-fns operator)
      (let [name-loc (some-> op-loc z/right)
            sym (some-> name-loc name-sym)]
        (when sym
          (let [pairs (collect-doc-pairs loc operator ns-name sym)
                usages (vec (mapcat (fn [[kw doc-loc]] (issues-for-doc kw doc-loc file)) pairs))]
            (when (seq usages)
              {:decls []
               :usages usages
               :dynamics []})))))))

(defn- analyze* [{:keys [usages]}]
  (let [by-type (group-by :type usages)]
    {:docstring-summary
     (vec (by-type :docstring-summary))
     :docstring-indentation
     (vec (by-type :docstring-indentation))
     :docstring-leading-trailing-whitespace
     (vec (by-type :docstring-leading-trailing-whitespace))
     :docstring-repeats-name
     (vec (by-type :docstring-repeats-name))}))

(defn- summary-lines* [{:keys [docstring-summary docstring-indentation
                               docstring-leading-trailing-whitespace docstring-repeats-name]}]
  [["Docstring summary violations:" (count docstring-summary)]
   ["Docstring indentation violations:" (count docstring-indentation)]
   ["Docstring leading/trailing whitespace:" (count docstring-leading-trailing-whitespace)]
   ["Docstring repeats name:" (count docstring-repeats-name)]])

(defn- failed?*
  "Always return false; docstring rules are informational by design.

  Users opt into CI enforcement via `--fail-on cleanup` or per-rule
  (`--fail-on docstring-summary`). Other groups (re-frame, spade,
  typography) return truthy here because their issues are bugs or
  deprecations, not style noise."
  [_]
  false)

(defrecord DocstringsGroup [extensions]
  group/RuleGroup
  (group-id [_] :docstrings)
  (group-name [_] "Docstrings")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:docstring-summary
     (str "The first line of a multi-line docstring is not a summary sentence of its own. End it "
          "with '.', '!', '?' or ':' and put the rest of the prose on the next lines.")
     :docstring-indentation
     (str "A docstring line is indented less than the opening quote. Indent the continuation "
          "lines at least to the column of the opening quote.")
     :docstring-leading-trailing-whitespace
     "A docstring starts or ends with whitespace. Remove it."
     :docstring-repeats-name
     (str "This docstring only repeats the var's name. Say what the name cannot, such as when or why "
          "to call it, or delete the docstring.")})
  (docs-anchors [_]
    {:docstring-indentation "docstring-indentation"
     :docstring-leading-trailing-whitespace "docstring-leadingtrailing-whitespace"
     :docstring-summary "docstring-summary"
     :docstring-repeats-name "docstring-repeats-name"})
  (rule->tier [_]
    {:docstring-summary :cleanup
     :docstring-indentation :cleanup
     :docstring-leading-trailing-whitespace :cleanup
     :docstring-repeats-name :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return a docstrings RuleGroup.

  The group has no config keys of its own, so `config` is accepted and ignored: the
  argument is there so the call reads like every other group's, and so a key added later
  lands in the slot every sibling already uses. `clj?` is the experimental `.clj` mode."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([_config clj?] (->DocstringsGroup (group/extensions-for clj?))))

(def group (make-group))
