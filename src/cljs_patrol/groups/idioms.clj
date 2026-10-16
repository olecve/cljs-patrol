(ns cljs-patrol.groups.idioms
  "Rule group: nested calls a threading macro would read top to bottom.

  Reports only what can be rewritten without a judgement call. Every step of a chain
  takes exactly one argument, so `->` and `->>` produce the same form, and the rewrite
  expands back to exactly the code that was written. Every head is also known to be a
  function. Threading cannot change what a macro does, but it changes how it reads:
  `(-> xs sort vec delay)` looks like four steps run in order, while the `delay` defers
  the other three. A head the scan cannot vouch for, a library var or a local, ends the
  chain, which gives up some findings so that none of them is a judgement call."
  (:require
   [cljs-patrol.group :as group]
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
  "Calls that turn what they wrap into a test or an empty-or-nil, and read worse ending a thread.

  `(not (str/blank? (:description m)))` threads to `(-> m :description str/blank? not)`.
  At the top of what would be threaded, one of these is not counted toward the chain,
  whether or not something the chain stops at sits above it: `(is (nil? (:id (first xs))))`
  reads no better as `(is (-> xs first :id nil?))`."
  #{"not" "boolean" "some?" "nil?" "seq" "empty?" "not-empty" "true?" "false?"})

(defn- code-children
  "Return the child nodes of node other than whitespace, or nil when one is a comment or `#_`.

  Neither has a place in a thread, and either can stand where an argument would, so a call
  holding one is not read as a step at all."
  [node]
  (let [children (remove n/whitespace? (n/children node))]
    (when-not (some #(contains? #{:comment :uneval} (n/tag %)) children)
      children)))

(defn- index-in-parent
  "Return the position of loc among its parent's children, the head at 0, or nil."
  [loc]
  (when-let [parent (z/up loc)]
    (->> (n/children (z/node parent))
         (remove #(or (n/whitespace? %) (contains? #{:comment :uneval} (n/tag %))))
         (keep-indexed (fn [i child] (when (identical? child (z/node loc)) i)))
         first)))

(def ^:private threading-heads
  "Forms that pass every step after the first an argument the step does not write out."
  #{"->" "->>" "some->" "some->>" "doto" ".."})

(def ^:private conditional-threading-heads
  "Forms that pass that argument only to the step of each test-and-step pair."
  #{"cond->" "cond->>"})

(defn- threaded-step?
  "True when loc is a step of a threading form, so it takes an argument it does not show.

  `(doto cell (.setCellValue (double x)))` calls `.setCellValue` with two arguments. A
  `cond->` test takes none, and an `as->` step writes its argument out."
  [loc]
  (let [parent (z/up loc)
        head (when (= :list (some-> parent z/tag)) (some-> parent z/down parser/sym-name))
        index (index-in-parent loc)]
    (boolean
     (and index
          (or (and (contains? threading-heads head) (>= index 2))
              (and (contains? conditional-threading-heads head) (>= index 3) (odd? index)))))))

(defn- one-argument?
  "True when loc is a list or `#(…)` of a token head and exactly one argument, written out."
  [loc]
  (let [children (code-children (z/node loc))]
    (and (= 2 (count children))
         (= :token (n/tag (first children)))
         (not (contains? #{"quote" "comment"} (parser/sym-name (z/down loc)))))))

(defn- link?
  "True when loc is a one-argument call that can sit inside a chain.

  The list a reader conditional holds is not a call: `#?(:clj x)` reads as `(:clj x)`.
  A `#(…)` is a function value wherever it is an argument, so it can only start a chain."
  [loc]
  (and (= :list (z/tag loc))
       (one-argument? loc)
       (not= :reader-macro (some-> loc z/up z/tag))
       (not (threaded-step? loc))))

(defn- start?
  "True when loc can start a chain: a link, or a `#(…)` whose body is a one-argument call."
  [loc]
  (or (link? loc)
      (and (= :fn (z/tag loc)) (one-argument? loc))))

(defn- head-loc [loc] (z/down loc))

(defn- argument-loc [loc] (z/right (z/down loc)))

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

(defn- describe-var
  "Classify the var full-ns/var-name: a core or string function, or a var to look up."
  [full-ns var-name]
  (cond
    (and (core-namespaces full-ns) (core-fns var-name)) {:kind :fn}
    (and (= "clojure.string" full-ns) (string-fns var-name)) {:kind :fn}
    :else {:kind :var
           :var (keyword full-ns var-name)}))

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
      (let [[alias-part name-part] (str/split text #"/" 2)]
        (if (or (contains? aliases alias-part) (not (interop-or-host? text)))
          (describe-var (get aliases alias-part alias-part) name-part)
          {:kind :fn}))
      (interop-or-host? text) {:kind :fn}
      (contains? refers text) (describe-var (get refers text) text)
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

(defn- level [loc ns-info]
  {:head (describe-head (head-loc loc) ns-info)
   :fn-literal? (= :fn (z/tag loc))
   :literal-argument? (literal? (argument-loc loc))
   :head-text (parser/raw (head-loc loc))
   :head-name (parser/sym-name (head-loc loc))
   :argument-text (z/string (argument-loc loc))
   :form-text (z/string loc)
   :position (try (z/position loc) (catch Exception _ [0 1]))})

(defn- inner-link?
  "True when loc is a link a chain above it walks into, so no chain starts at loc."
  [loc]
  (let [parent (z/up loc)]
    (boolean
     (and parent
          (link? loc)
          (start? parent)
          (identical? (z/node loc) (z/node (argument-loc parent)))))))

(def ^:private unevaluated-tags #{:quote :syntax-quote :unquote :unquote-splicing :uneval})

(defn- unevaluated?
  "True when loc is quoted, discarded, or inside `(quote …)` or `(comment …)`."
  [loc]
  (loop [current (z/up loc)]
    (cond
      (nil? current) false
      (contains? unevaluated-tags (z/tag current)) true
      (and (= :list (z/tag current))
           (contains? #{"quote" "comment"} (some-> current z/down parser/sym-name))) true
      :else (recur (z/up current)))))

(defn- chain-candidate
  "Record the longest run of one-argument calls starting at loc, or nil.

  Whether each head is a function is decided once every file has been read, since a
  head may name a var defined in another one. A run shorter than min-calls is dropped
  here: setting a head aside later only shortens it."
  [min-calls loc ns-info file]
  (when (and (start? loc) (not (inner-link? loc)))
    (let [levels (loop [current loc
                        found []]
                   (let [found (conj found (level current ns-info))
                         argument (argument-loc current)]
                     (if (link? argument)
                       (recur argument found)
                       found)))]
      (when (and (>= (count levels) min-calls) (not (unevaluated? loc)))
        {:type ::chain
         :levels levels
         :file file
         :test (parser/top-level-name loc)}))))

(def ^:private def-kinds
  {"defn" :fn
   "defn-" :fn
   "defmulti" :fn
   "defmacro" :macro})

(defn- definition
  "Record what a top-level `def`-like form defines, or nil.

  A `defmethod` adds a method to a var that exists already, and defines nothing."
  [list-loc {:keys [ns-name]} file]
  (let [head (some-> list-loc z/down parser/sym-name)]
    (when (and head
               (str/starts-with? head "def")
               (not= "defmethod" head)
               (= :forms (some-> list-loc z/up z/tag)))
      (when-let [defined (some-> (parser/declared-name-loc list-loc) parser/sym-name)]
        {:type ::definition
         :var (keyword ns-name defined)
         :kind (get def-kinds head :other)
         :file file}))))

(defn- handle [min-calls loc ns-info file]
  (let [found (keep identity [(when (= :list (z/tag loc)) (definition loc ns-info file))
                              (chain-candidate min-calls loc ns-info file)])]
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

(defn- rewrite
  "Spell out the thread for run, the starting value in full or, given a limit, shortened."
  [run seed-limit]
  (let [seed (:argument-text (last run))
        shown (if (and seed-limit
                       (or (> (count seed) seed-limit) (str/includes? seed "\n")))
                "..."
                seed)
        thread (str "-> " shown " " (str/join " " (map :head-text (reverse run))))]
    (if (:fn-literal? (first run))
      (str "#(" thread ")")
      (str "(" thread ")"))))

(defn- finding [{:keys [file test]} run]
  (let [[row col] (:position (first run))]
    (cond-> {:kw '->
             :type :nested-call-chain
             :form (parser/normalize-form (:form-text (first run)))
             :file file
             :row row
             :col col
             :rewrite (rewrite run nil)
             :hint (str "Thread it: " (rewrite run seed-max))}
      test (assoc :test test))))

(defn- function-definitions
  "Map each var the project defines to :fn, or to :other when any definition of it is not a fn.

  The same var can be defined twice, in a `.cljs` and a `.clj` file read together, and
  only one that is a function in both counts."
  [declarations]
  (->> declarations
       (filter #(= ::definition (:type %)))
       (group-by :var)
       (map (fn [[var defined]]
              [var (if (every? #(= :fn (:kind %)) defined) :fn :other)]))
       (into {})))

(defn- analyze* [min-calls {:keys [declarations]}]
  (let [definitions (function-definitions declarations)]
    {:nested-call-chain
     (->> declarations
          (filter #(= ::chain (:type %)))
          (mapcat (fn [candidate]
                    (->> (runs (:levels candidate) definitions)
                         (map (comp keep-literal-call drop-wrappers))
                         (filter #(>= (count %) min-calls))
                         (map #(finding candidate %)))))
          vec)}))

(defn- suggestion [min-calls]
  (str min-calls " or more calls nested one inside another, each taking the one before as its "
       "only argument. The reader has to start at the innermost form and work outwards; a "
       "thread reads top to bottom in the order the calls run. Rewrite it with ->, which "
       "the finding spells out. Only calls known to be functions count: keywords, interop, "
       "core and clojure.string functions, and the project's own defn and defmulti. A macro, "
       "a call into a library the scan cannot read, and a call with more than one argument "
       "end the chain. A " (str/join ", " (sort wrappers)) " at the top of the thread is not "
       "counted, and a call on a literal stays the thread's starting value."))

(defrecord IdiomsGroup [min-calls extensions]
  group/RuleGroup
  (group-id [_] :idioms)
  (group-name [_] "Idioms")
  (parse-handlers [_] {:handle-list (fn [loc ns-info file] (handle min-calls loc ns-info file))})
  (analyze [_ data] (analyze* min-calls data))
  (summary-lines [_ {:keys [nested-call-chain]}]
    [["Nested call chain:" (count nested-call-chain)]])
  (failed? [_ _] false)
  (suggestions [_] {:nested-call-chain (suggestion min-calls)})
  (rule->tier [_] {:nested-call-chain :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return an idioms RuleGroup configured with the given map.

  Supported keys:
    :min-calls 3 — the shortest chain reported, counted after a wrapper and a call on a
      literal are set aside.

  `clj?` is the experimental `.clj` mode."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([config clj?]
   (let [min-calls (when (map? config) (:min-calls config))]
     (->IdiomsGroup (if (and (int? min-calls) (>= min-calls 2)) min-calls 3)
                    (group/extensions-for clj?)))))

(def group (make-group))
