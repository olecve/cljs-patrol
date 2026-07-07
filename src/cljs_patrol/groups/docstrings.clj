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

(defn- docstring-issues [content col]
  (let [leading-trailing? (leading-trailing-violation? content)]
    (cond-> []
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
          (docstring-issues content col))))

(defn- collect-doc-pairs [loc operator ns-name name-symbol]
  (let [primary-kw (keyword ns-name (name name-symbol))
        primary (when-let [d (find-docstring-loc loc)] [[primary-kw d]])
        methods (when (= "defprotocol" operator) (find-method-docstrings loc ns-name))]
    (concat primary methods)))

(defn- handle-list [loc ns-name _aliases file]
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
     (vec (by-type :docstring-leading-trailing-whitespace))}))

(defn- summary-lines* [{:keys [docstring-summary docstring-indentation
                               docstring-leading-trailing-whitespace]}]
  [["Docstring summary violations:" (count docstring-summary)]
   ["Docstring indentation violations:" (count docstring-indentation)]
   ["Docstring leading/trailing whitespace:" (count docstring-leading-trailing-whitespace)]])

(defn- failed?*
  "Always return false; docstring rules are informational by design.
  Users opt into CI enforcement via `--fail-on cleanup` or per-rule
  (`--fail-on docstring-summary`). Other groups (re-frame, spade,
  typography) return truthy here because their issues are bugs or
  deprecations, not style noise."
  [_]
  false)

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
