(ns cljs-patrol.groups.docstrings
  "Docstrings rule group: enforces bbatsov clojure-style-guide docstring conventions.
  Checks docstring-summary, docstring-indentation, and docstring-leading-trailing-whitespace."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private public-def-fns
  #{"defn" "def" "defmacro" "defmulti" "defprotocol"})

(defn- string-node?
  [loc]
  (and loc
       (= :token (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))))

(defn- name-info
  "Return {:sym <symbol> :private? <bool>} for a defn-style name location, or nil."
  [name-loc]
  (try
    (let [value (z/sexpr name-loc)]
      (when (symbol? value)
        {:sym value
         :private? (boolean (:private (meta value)))}))
    (catch Exception _ nil)))

(defn- find-docstring-loc
  "Return the docstring zip loc for a def-form, or nil. A string is treated as
  a docstring only when followed by at least one more node (so `(def foo \"v\")`
  is recognized as a value, not a docstring)."
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
  "Multi-line docstring: first line must end with a sentence terminator and not
  contain prose after that terminator on the same line."
  [content]
  (when (multi-line? content)
    (let [first-line (first (str/split content #"\n" 2))]
      (boolean
       (or (re-find #"[.!?]\s+\S" first-line)
           (not (re-find #"[.!?]\s*$" first-line)))))))

(defn- indentation-violation?
  "Multi-line docstring: each non-blank continuation line must have leading
  whitespace at least equal to `(column-of-opening-quote - 1)`."
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
  (cond-> []
    (summary-violation? content)
    (conj :docstring-summary)

    (indentation-violation? content col)
    (conj :docstring-indentation)

    (leading-trailing-violation? content)
    (conj :docstring-leading-trailing-whitespace)))

(defn- handle-list
  [loc ns-name _aliases file]
  (let [op-loc (z/down loc)
        operator (parser/sym-name op-loc)]
    (when (contains? public-def-fns operator)
      (let [name-loc (some-> op-loc z/right)
            info (some-> name-loc name-info)]
        (when (and info (not (:private info)))
          (when-let [doc-loc (find-docstring-loc loc)]
            (let [kw (keyword ns-name (name (:sym info)))
                  content (strip-quotes (parser/raw doc-loc))
                  [_ col] (try (z/position doc-loc) (catch Exception _ [0 1]))
                  row (parser/position-row doc-loc)
                  issue-types (docstring-issues content col)
                  usages (mapv (fn [t]
                                 {:kw kw
                                  :type t
                                  :file file
                                  :row row})
                               issue-types)]
              (when (seq usages)
                {:decls []
                 :usages usages
                 :dynamics []}))))))))

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
     :docstring-leading-trailing-whitespace :cleanup}))

(def group (->DocstringsGroup))
