(ns cljs-patrol.groups.docstrings
  "Docstrings rule group: enforces bbatsov clojure-style-guide docstring conventions.
  Checks docstring-summary, docstring-indentation, and docstring-leading-trailing-whitespace
  on every def-form regardless of privacy (defn, defn-, def, defmacro, defmulti, defprotocol).
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
  "Title and place-name abbreviations whose trailing period is part of the
  abbreviation, not a sentence break. Stripped before run-on detection so that
  `Mr. Smith`, `St. Petersburg`, etc. are not misread as two sentences.
  Excludes `e.g.`, `i.e.`, `etc.` — those legitimately introduce a second
  sentence and should remain detected."
  #{"Mr." "Mrs." "Ms." "Dr." "Prof." "Jr." "Sr." "St." "Inc." "Ltd."})

(defn- strip-abbreviation-periods
  "Replace each whole-word abbreviation with the same word minus its trailing
  period, so the following capital letter is no longer preceded by a `.`."
  [s]
  (reduce (fn [acc abbr]
            (str/replace acc abbr (subs abbr 0 (dec (count abbr)))))
          s
          name-title-abbreviations))

(defn- string-node?
  [loc]
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

(defn- strip-quotes [raw]
  (if (and (>= (count raw) 2)
           (str/starts-with? raw "\"")
           (str/ends-with? raw "\""))
    (subs raw 1 (dec (count raw)))
    raw))

(defn- multi-line? [content]
  (str/includes? content "\n"))

(defn- summary-violation?
  "Detect a missing or non-self-contained summary line in a multi-line docstring.
  The first line must end with a sentence terminator and not begin a new
  sentence on the same line. False positives from Clojure-style identifiers
  (`:dynamic?`, `string?`) are skipped by requiring an upper-case letter after
  the terminator. Name and place-title abbreviations (`Mr.`, `St.`, ...) are
  stripped first so they do not look like sentence breaks."
  [content]
  (when (multi-line? content)
    (let [first-line (first (str/split content #"\n" 2))
          cleaned (strip-abbreviation-periods first-line)]
      (boolean
       (or (re-find #"[.!?]\s+[A-Z]" cleaned)
           (not (re-find #"[.!?]\s*$" first-line)))))))

(defn- indentation-violation?
  "Detect under-indented continuation lines in a multi-line docstring.
  Each non-blank continuation line must have leading whitespace at least
  equal to `(column-of-opening-quote - 1)`."
  [content col]
  (when (multi-line? content)
    (let [expected (dec col)
          continuation (rest (str/split content #"\n" -1))]
      (boolean
       (some (fn [line]
               (let [trimmed (str/triml line)
                     leading (- (count line) (count trimmed))]
                 (and (seq trimmed)
                      (< leading expected))))
             continuation)))))

(defn- leading-trailing-violation? [content]
  (and (seq content)
       (or (Character/isWhitespace (.charAt content 0))
           (Character/isWhitespace (.charAt content (dec (count content)))))))

(defn- docstring-issues [content col]
  (let [leading-trailing? (leading-trailing-violation? content)]
    (cond-> []
      (summary-violation? content)
      (conj :docstring-summary)

      (and (not leading-trailing?) (indentation-violation? content col))
      (conj :docstring-indentation)

      leading-trailing?
      (conj :docstring-leading-trailing-whitespace))))

(defn- rightmost-sibling
  "Walk to the rightmost sibling starting from loc."
  [loc]
  (loop [cur loc last-loc cur]
    (if-let [nxt (z/right cur)]
      (recur nxt nxt)
      last-loc)))

(defn- find-method-docstrings
  "For a defprotocol form, return a seq of [method-kw doc-loc] for each method
  signature whose last form is a string. Skips the outer docstring and method
  signatures that have no trailing docstring."
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

(defn- issues-for-doc
  "Run all three rule predicates against `doc-loc` and return one usage map per
  violation, keyed by `kw`."
  [kw doc-loc file]
  (let [content (strip-quotes (parser/raw doc-loc))
        [_ col] (try (z/position doc-loc) (catch Exception _ [0 1]))
        row (parser/position-row doc-loc)]
    (mapv (fn [t]
            {:kw kw
             :type t
             :file file
             :row row})
          (docstring-issues content col))))

(defn- collect-doc-pairs
  "Return a seq of [kw doc-loc] for every docstring to check in a def-form:
  the outer docstring (if present) plus, for defprotocol, each method signature
  with a trailing docstring."
  [loc operator ns-name name-symbol]
  (let [primary-kw (keyword ns-name (name name-symbol))
        primary (when-let [d (find-docstring-loc loc)] [[primary-kw d]])
        methods (when (= "defprotocol" operator) (find-method-docstrings loc ns-name))]
    (concat primary methods)))

(defn- handle-list
  [loc ns-name _aliases file]
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
  {:docstring-summary
   (vec (filter #(= :docstring-summary (:type %)) usages))
   :docstring-indentation
   (vec (filter #(= :docstring-indentation (:type %)) usages))
   :docstring-leading-trailing-whitespace
   (vec (filter #(= :docstring-leading-trailing-whitespace (:type %)) usages))})

(defn- summary-lines* [{:keys [docstring-summary docstring-indentation
                               docstring-leading-trailing-whitespace]}]
  [["Docstring summary violations:" (count docstring-summary)]
   ["Docstring indentation violations:" (count docstring-indentation)]
   ["Docstring leading/trailing whitespace:" (count docstring-leading-trailing-whitespace)]])

(defn- failed?* [_] false)

(defrecord DocstringsGroup []
  group/RuleGroup
  (group-id [_] :docstrings)
  (group-name [_] "Docstrings")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:docstring-summary
     (str "First line of a multi-line docstring must be a self-contained summary sentence "
          "ending in '.', '!' or '?'. Put any additional prose on the next line. "
          "See bbatsov clojure-style-guide#docstring-summary.")
     :docstring-indentation
     (str "Continuation lines of a multi-line docstring should be indented at least to the "
          "column of the opening quote. See bbatsov clojure-style-guide#docstring-indentation.")
     :docstring-leading-trailing-whitespace
     (str "Docstring must not start or end with whitespace. "
          "See bbatsov clojure-style-guide#docstring-leading-trailing-whitespace.")})
  (rule->tier [_]
    {:docstring-summary :cleanup
     :docstring-indentation :cleanup
     :docstring-leading-trailing-whitespace :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->DocstringsGroup))
