(ns cljs-patrol.groups.idioms
  "Rule group: nested calls a threading macro would read top to bottom.

  Reports only what can be rewritten without a judgement call. Every step of a chain
  takes exactly one argument, so `->` and `->>` would produce the same form and the
  rewrite cannot thread into the wrong slot. Every head is known to be a function, so
  the rewrite cannot thread into a macro, whose arguments are not evaluated in order."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(def ^:private core-fns
  "Functions of `clojure.core` / `cljs.core` that a one-argument call reads as one step."
  #{"first" "second" "last" "rest" "next" "butlast" "ffirst" "fnext" "nfirst" "nnext"
    "count" "seq" "vec" "set" "keys" "vals" "sort" "distinct" "dedupe" "reverse" "flatten"
    "frequencies" "shuffle" "peek" "pop" "key" "val" "meta" "doall"
    "name" "namespace" "keyword" "symbol" "str" "inc" "dec" "int" "long" "double" "abs"
    "not" "boolean" "some?" "nil?" "empty?" "not-empty" "true?" "false?"
    "zero?" "pos?" "neg?" "even?" "odd?" "deref" "realized?"
    "clj->js" "js->clj" "array-seq" "into-array" "to-array"
    "parse-long" "parse-double" "parse-boolean" "parse-uuid"})

(def ^:private string-fns
  "Functions of `clojure.string` that a one-argument call reads as one step."
  #{"lower-case" "upper-case" "capitalize" "trim" "triml" "trimr" "trim-newline" "blank?"
    "split-lines"})

(def ^:private core-namespaces #{"clojure.core" "cljs.core"})

(def ^:private wrappers
  "Calls that turn a value into a yes or no, and read worse at the end of a thread.

  `(not (str/blank? (:description m)))` threads to `(-> m :description str/blank? not)`.
  Left outermost, one of these is not counted toward the chain."
  #{"not" "boolean" "some?" "nil?" "seq" "empty?" "not-empty" "true?" "false?"})

(defn- elements
  "Return the child nodes of node other than whitespace.

  A comment or a discarded form counts as a child, so a step holding one has more than one
  argument and ends the chain: neither has a place in a thread."
  [node]
  (remove n/whitespace? (n/children node)))

(def ^:private threading-heads
  "Forms that pass each step after the first an argument the step does not write out."
  #{"->" "->>" "some->" "some->>" "cond->" "cond->>" "as->" "doto" ".."})

(defn- threaded-step?
  "True when list-loc is a step of a threading form, so it takes an argument it does not show.

  `(doto cell (.setCellValue (double x)))` calls `.setCellValue` with two arguments."
  [list-loc]
  (let [parent (z/up list-loc)]
    (and (= :list (some-> parent z/tag))
         (contains? threading-heads (some-> parent z/down parser/sym-name))
         (not (identical? (z/node list-loc) (some-> parent z/down z/right z/node))))))

(defn- unary-call?
  "True when list-loc is a list of a token head and exactly one argument, written out.

  The list a reader conditional holds is not a call: `#?(:clj x)` reads as `(:clj x)`."
  [list-loc]
  (and (= :list (z/tag list-loc))
       (let [children (elements (z/node list-loc))]
         (and (= 2 (count children))
              (= :token (n/tag (first children)))
              (not (contains? #{"quote" "comment"} (parser/sym-name (z/down list-loc))))))
       (not= :reader-macro (some-> list-loc z/up z/tag))
       (not (threaded-step? list-loc))))

(defn- head-loc [list-loc] (z/down list-loc))

(defn- argument-loc [list-loc] (z/right (z/down list-loc)))

(defn- class-name? [segment]
  (let [initial (first segment)]
    (boolean (and initial (Character/isUpperCase (char initial))))))

(defn- interop-or-host?
  "True when sym-str names a host call: `.method`, `.-field`, `Ctor.`, `js/f` or `Class/static`.

  A qualifier names a class when its last segment is capitalized, `java.util.UUID/randomUUID`
  as much as `ImageIO/read`; a lower-case one, `some.lib/f`, is a namespace."
  [sym-str]
  (let [slash (str/index-of sym-str "/")
        qualifier (when (and slash (pos? slash)) (subs sym-str 0 slash))]
    (boolean
     (or (and (str/starts-with? sym-str ".") (> (count sym-str) 1))
         (and (str/ends-with? sym-str ".") (> (count sym-str) 1) (nil? slash))
         (= "js" qualifier)
         (class-name? (some-> qualifier (str/split #"\.") last))))))

(defn- describe-head
  "Classify the head of a call by what the analysis needs to know about it.

  `:fn` is known to be a function wherever it is used. `:var` resolves to a namespace's
  var, and is a function only if the project defines it with `defn`. `:local-or-core` is
  unqualified and unreferred: the project's own definition in this namespace decides, and
  failing one, whether it is a core function. `:unknown` is everything else."
  [loc {:keys [ns-name aliases refers]}]
  (let [text (parser/raw loc)
        sym (parser/sym-name loc)]
    (cond
      (parser/kw-node? loc) {:kind :fn}
      (nil? sym) {:kind :unknown}
      (and (str/includes? text "/") (pos? (str/index-of text "/")))
      (let [[alias-part name-part] (str/split text #"/" 2)
            full-ns (get aliases alias-part alias-part)]
        (cond
          (and (core-namespaces full-ns) (core-fns name-part)) {:kind :fn}
          (and (= "clojure.string" full-ns) (string-fns name-part)) {:kind :fn}
          (contains? aliases alias-part) {:kind :var
                                          :var (keyword full-ns name-part)}
          (interop-or-host? text) {:kind :fn}
          :else {:kind :var
                 :var (keyword full-ns name-part)}))
      (interop-or-host? text) {:kind :fn}
      (contains? refers text) {:kind :var
                               :var (keyword (get refers text) text)}
      :else {:kind :local-or-core
             :var (keyword ns-name text)
             :core? (contains? core-fns text)})))

(defn- literal?
  "True when loc is written out as a value: a string, number, keyword, or collection literal."
  [loc]
  (case (z/tag loc)
    (:map :vector :set :multi-line :regex) true
    :token (not (symbol? (try (z/sexpr loc) (catch Exception _ nil))))
    false))

(defn- level [list-loc ns-info]
  {:head (describe-head (head-loc list-loc) ns-info)
   :literal-argument? (literal? (argument-loc list-loc))
   :head-text (parser/raw (head-loc list-loc))
   :head-name (parser/sym-name (head-loc list-loc))
   :argument-text (parser/normalize-form (parser/raw (argument-loc list-loc)))
   :form-text (parser/normalize-form (parser/raw list-loc))
   :position (try (z/position list-loc) (catch Exception _ [0 1]))})

(defn- inner-link?
  "True when list-loc is the argument of a one-argument call, so a chain starts above it."
  [list-loc]
  (let [parent (z/up list-loc)]
    (and parent
         (unary-call? parent)
         (identical? (z/node list-loc) (z/node (argument-loc parent))))))

(defn- unevaluated?
  "True when loc is quoted, discarded, or inside `(quote …)` or `(comment …)`."
  [loc]
  (or (hiccup/inside-unrendered-form? loc)
      (loop [current (z/up loc)]
        (cond
          (nil? current) false
          (and (= :list (z/tag current))
               (contains? #{"quote" "comment"} (some-> current z/down parser/sym-name))) true
          :else (recur (z/up current))))))

(defn- chain-candidate
  "Record the longest run of one-argument calls starting at list-loc, or nil.

  Whether each head is a function is decided once every file has been read, since a
  head may name a var defined in another one."
  [list-loc ns-info file]
  (when (and (unary-call? list-loc)
             (not (inner-link? list-loc))
             (not (unevaluated? list-loc)))
    (let [levels (loop [loc list-loc
                        found []]
                   (let [found (conj found (level loc ns-info))
                         argument (argument-loc loc)]
                     (if (unary-call? argument)
                       (recur argument found)
                       found)))]
      (when (>= (count levels) 2)
        {:type ::chain
         :levels levels
         :file file
         :test (parser/top-level-name list-loc)}))))

(def ^:private def-kinds
  {"defn" :fn
   "defn-" :fn
   "defmulti" :fn
   "defmacro" :macro})

(defn- definition
  "Record what a top-level `def`-like form defines, or nil."
  [list-loc {:keys [ns-name]} file]
  (let [head (some-> list-loc z/down parser/sym-name)]
    (when (and head
               (str/starts-with? head "def")
               (= :forms (some-> list-loc z/up z/tag)))
      (when-let [defined (some-> (parser/declared-name-loc list-loc) parser/sym-name)]
        {:type ::definition
         :var (keyword ns-name defined)
         :kind (get def-kinds head :other)
         :file file}))))

(defn- handle [loc ns-info file]
  (let [found (keep identity [(definition loc ns-info file) (chain-candidate loc ns-info file)])]
    (when (seq found)
      {:decls (vec found)
       :usages []
       :dynamics []})))

(defn- function? [{:keys [kind var core?]} definitions]
  (case kind
    :fn true
    :var (= :fn (get definitions var))
    :local-or-core (if-let [defined (get definitions var)] (= :fn defined) core?)
    false))

(defn- runs
  "Split levels into the maximal runs whose every head is a function."
  [levels definitions]
  (->> levels
       (partition-by #(function? (:head %) definitions))
       (filter #(function? (:head (first %)) definitions))))

(defn- drop-wrappers [run]
  (drop-while #(contains? wrappers (:head-name %)) run))

(defn- keep-literal-call
  "Leave a call on a literal as the thread's starting value rather than a step of it.

  `(:question (:params (request-to \"/qa\")))` threads to `(-> (request-to \"/qa\") :params
  :question)`: a path or a constant reads backwards at the head of a thread."
  [run]
  (if (:literal-argument? (last run)) (butlast run) run))

(def ^:private seed-max 40)

(defn- shorten [text limit]
  (if (<= (count text) limit) text (str (subs text 0 (- limit 3)) "...")))

(defn- rewrite [run seed-limit]
  (str "(-> " (shorten (:argument-text (last run)) seed-limit) " "
       (str/join " " (map :head-text (reverse run))) ")"))

(def ^:private form-max 60)

(defn- finding [{:keys [file test]} run]
  (let [[row col] (:position (first run))]
    (cond-> {:kw '->
             :type :nested-call-chain
             :form (shorten (:form-text (first run)) form-max)
             :file file
             :row row
             :col col
             :rewrite (rewrite run Integer/MAX_VALUE)
             :hint (str "Thread it: " (rewrite run seed-max))}
      test (assoc :test test))))

(defn- analyze* [min-calls {:keys [declarations]}]
  (let [definitions (->> declarations
                         (filter #(= ::definition (:type %)))
                         (map (juxt :var :kind))
                         (into {}))]
    {:nested-call-chain
     (->> declarations
          (filter #(= ::chain (:type %)))
          (mapcat (fn [candidate]
                    (->> (runs (:levels candidate) definitions)
                         (map (comp keep-literal-call drop-wrappers))
                         (filter #(>= (count %) min-calls))
                         (map #(finding candidate %)))))
          vec)}))

(defrecord IdiomsGroup [min-calls extensions]
  group/RuleGroup
  (group-id [_] :idioms)
  (group-name [_] "Idioms")
  (parse-handlers [_] {:handle-list handle})
  (analyze [_ data] (analyze* min-calls data))
  (summary-lines [_ {:keys [nested-call-chain]}]
    [["Nested call chain:" (count nested-call-chain)]])
  (failed? [_ _] false)
  (suggestions [_]
    {:nested-call-chain
     (str "Three or more calls nested one inside another, each taking the one before as its "
          "only argument. The reader has to start at the innermost form and work outwards; a "
          "thread reads top to bottom in the order the calls run. Rewrite it with ->, which "
          "the finding spells out. Only calls known to be functions count: keywords, interop, "
          "core and clojure.string functions, and the project's own defn. A macro, a call into "
          "a library the scan cannot read, and a call with more than one argument end the "
          "chain. A not, boolean, some?, nil?, seq or empty? wrapping the whole chain is not "
          "counted.")})
  (rule->tier [_] {:nested-call-chain :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return an idioms RuleGroup configured with the given map.

  Supported keys:
    :min-calls 3 — the shortest chain reported, counted after any wrapper is dropped.

  `clj?` is the experimental `.clj` mode."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([config clj?]
   (let [min-calls (when (map? config) (:min-calls config))]
     (->IdiomsGroup (if (and (int? min-calls) (>= min-calls 2)) min-calls 3)
                    (group/extensions-for clj?)))))

(def group (make-group))
