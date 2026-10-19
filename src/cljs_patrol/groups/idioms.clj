(ns cljs-patrol.groups.idioms
  "Rule group: nested calls a threading macro would read top to bottom.

  Reports only what can be rewritten without a judgement call. Every step passes the call
  below it in the slot a thread fills, the first argument for `->` or the last for `->>`,
  so the rewrite expands back to exactly the code that was written. That slot also has to
  be where the function takes its data: `(get-in db (path))` nests a call, but the call is
  the path, and threading the path reads backwards. So a step with more than one argument
  counts only for a function whose data slot is known, core, `clojure.string` and interop.

  Every head is also known to be a function. Threading cannot change what a macro does,
  but it changes how it reads: `(-> xs sort vec delay)` looks like four steps run in order,
  while the `delay` defers the other three. A head the scan cannot vouch for, a library var
  or a local, ends the chain, which gives up some findings so that none of them is a
  judgement call."
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

(defn- arity [& counts] (set counts))

(defn- at-least [minimum] #(>= % minimum))

(def ^:private data-slots
  "Where a function of more than one argument takes its data, and the argument counts it does so at.

  Keyed by library and name. A count outside the set has no data slot a thread can fill:
  `(map f)` is a transducer, and `(map f xs ys)` maps over two collections."
  {[:core "map"] [:last (arity 2)]
   [:core "mapv"] [:last (arity 2)]
   [:core "mapcat"] [:last (arity 2)]
   [:core "map-indexed"] [:last (arity 2)]
   [:core "filter"] [:last (arity 2)]
   [:core "filterv"] [:last (arity 2)]
   [:core "remove"] [:last (arity 2)]
   [:core "keep"] [:last (arity 2)]
   [:core "keep-indexed"] [:last (arity 2)]
   [:core "take"] [:last (arity 2)]
   [:core "drop"] [:last (arity 2)]
   [:core "take-while"] [:last (arity 2)]
   [:core "drop-while"] [:last (arity 2)]
   [:core "take-last"] [:last (arity 2)]
   [:core "drop-last"] [:last (arity 2)]
   [:core "sort"] [:last (arity 2)]
   [:core "sort-by"] [:last (arity 2 3)]
   [:core "group-by"] [:last (arity 2)]
   [:core "partition"] [:last (arity 2 3 4)]
   [:core "partition-all"] [:last (arity 2 3)]
   [:core "partition-by"] [:last (arity 2)]
   [:core "interpose"] [:last (arity 2)]
   [:core "some"] [:last (arity 2)]
   [:core "every?"] [:last (arity 2)]
   [:core "not-any?"] [:last (arity 2)]
   [:core "reduce"] [:last (arity 2 3)]
   [:core "into"] [:last (arity 2 3)]
   [:core "cons"] [:last (arity 2)]
   [:string "join"] [:last (arity 2)]
   [:core "get"] [:first (arity 2 3)]
   [:core "get-in"] [:first (arity 2 3)]
   [:core "assoc"] [:first #(and (>= % 3) (odd? %))]
   [:core "assoc-in"] [:first (arity 3)]
   [:core "update"] [:first (at-least 3)]
   [:core "update-in"] [:first (at-least 3)]
   [:core "dissoc"] [:first (at-least 2)]
   [:core "select-keys"] [:first (arity 2)]
   [:core "merge"] [:first (at-least 2)]
   [:core "conj"] [:first (at-least 2)]
   [:core "disj"] [:first (at-least 2)]
   [:core "contains?"] [:first (arity 2)]
   [:core "find"] [:first (arity 2)]
   [:core "nth"] [:first (arity 2 3)]
   [:core "subs"] [:first (arity 2 3)]
   [:core "update-vals"] [:first (arity 2)]
   [:core "update-keys"] [:first (arity 2)]
   [:string "split"] [:first (arity 2 3)]
   [:string "replace"] [:first (arity 3)]
   [:string "replace-first"] [:first (arity 3)]
   [:string "starts-with?"] [:first (arity 2)]
   [:string "ends-with?"] [:first (arity 2)]
   [:string "includes?"] [:first (arity 2)]
   [:string "index-of"] [:first (arity 2 3)]})

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
  "Classify the var full-ns/var-name: core or `clojure.string`, or a var to look up."
  [full-ns var-name]
  (cond
    (core-namespaces full-ns) {:kind :core
                               :library :core
                               :name var-name}
    (= "clojure.string" full-ns) {:kind :core
                                  :library :string
                                  :name var-name}
    :else {:kind :var
           :var (keyword full-ns var-name)}))

(defn- describe-head
  "Classify the head of a call by what the analysis needs to know about it.

  `:keyword` and `:host` are functions wherever they are used, and `:core` names a function
  of `clojure.core` or `clojure.string`. `:var` resolves to a namespace's var, a function
  only if the project defines it with `defn`. `:local` is unqualified and unreferred: the
  project's own definition in this namespace decides, and failing one it is read as core.
  `:other` is everything else."
  [loc {:keys [ns-name aliases refers]}]
  (let [text (parser/raw loc)
        sym (parser/sym-name loc)]
    (cond
      (parser/kw-node? loc) {:kind :keyword}
      (nil? sym) {:kind :other}
      (and (str/includes? text "/") (pos? (str/index-of text "/")))
      (let [[alias-part name-part] (str/split text #"/" 2)]
        (if (or (contains? aliases alias-part) (not (interop-or-host? text)))
          (describe-var (get aliases alias-part alias-part) name-part)
          {:kind :host}))
      (interop-or-host? text) {:kind :host
                               :method? (str/starts-with? text ".")}
      (contains? refers text) (describe-var (get refers text) text)
      :else {:kind :local
             :var (keyword ns-name text)
             :name text})))

(defn- call-arguments
  "Return the argument locs of a list or `#(…)` with a token head, or nil."
  [loc]
  (let [children (code-children (z/node loc))]
    (when (and (seq children)
               (= :token (n/tag (first children)))
               (not (contains? #{"quote" "comment"} (parser/sym-name (z/down loc)))))
      (->> (iterate z/right (z/right (z/down loc)))
           (take (dec (count children)))
           vec))))

(defn- data-slot
  "Return [direction argument-loc] for the argument loc threads, or nil when it threads none.

  One argument threads either way, `:either`. With more, the head's data slot decides:
  `:last` for `(map f xs)`, `:first` for `(get m k)` and a method call. Which function the
  head names, and whether it takes this many arguments, is settled once every file is
  read; this reads it as core unless the namespace says otherwise, and a head that turns
  out to be something else ends the chain."
  [loc ns-info]
  (when-let [arguments (seq (call-arguments loc))]
    (if (= 1 (count arguments))
      [:either (first arguments)]
      (let [{:keys [kind library name method?]} (describe-head (z/down loc) ns-info)
            [direction] (when (contains? #{:core :local} kind)
                          (get data-slots [(or library :core) name]))]
        (cond
          direction
          [direction (if (= :last direction) (last arguments) (first arguments))]

          (and (= :host kind) method?)
          [:first (first arguments)])))))

(defn- link?
  "True when loc is a call that can sit inside a chain.

  The list a reader conditional holds is not a call: `#?(:clj x)` reads as `(:clj x)`.
  A `#(…)` is a function value wherever it is an argument, so it can only start a chain."
  [loc ns-info]
  (and (= :list (z/tag loc))
       (some? (data-slot loc ns-info))
       (not= :reader-macro (some-> loc z/up z/tag))
       (not (threaded-step? loc))))

(defn- start?
  "True when loc can start a chain: a link, or a `#(…)` whose body is a call."
  [loc ns-info]
  (or (link? loc ns-info)
      (and (= :fn (z/tag loc)) (some? (data-slot loc ns-info)))))

(defn- inner-link?
  "True when loc is a link a chain above it walks into, so no chain starts at loc."
  [loc ns-info]
  (let [parent (z/up loc)]
    (boolean
     (and parent
          (link? loc ns-info)
          (start? parent ns-info)
          (identical? (z/node loc) (some-> (data-slot parent ns-info) second z/node))))))

(defn- literal?
  "True when loc is written out as a value: a string, number, keyword, or collection literal."
  [loc]
  (case (z/tag loc)
    (:map :vector :set :multi-line :regex) true
    :token (not (symbol? (try (z/sexpr loc) (catch Exception _ nil))))
    false))

(defn- level [loc ns-info]
  (let [[direction argument] (data-slot loc ns-info)
        arguments (call-arguments loc)]
    {:head (describe-head (z/down loc) ns-info)
     :direction direction
     :arity (count arguments)
     :fn-literal? (= :fn (z/tag loc))
     :literal-argument? (literal? argument)
     :head-text (parser/raw (z/down loc))
     :head-name (parser/sym-name (z/down loc))
     :other-arguments (->> arguments
                           (remove #(identical? (z/node %) (z/node argument)))
                           (mapv z/string))
     :argument-text (z/string argument)
     :form-text (z/string loc)
     :position (try (z/position loc) (catch Exception _ [0 1]))}))

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
  "Record the longest run of calls each threading the one below it, starting at loc, or nil.

  Whether each head is a function is decided once every file has been read, since a
  head may name a var defined in another one. A run shorter than min-calls is dropped
  here: setting a head aside later only shortens it."
  [min-calls loc ns-info file]
  (when (and (start? loc ns-info) (not (inner-link? loc ns-info)))
    (let [levels (loop [current loc
                        found []]
                   (let [found (conj found (level current ns-info))
                         argument (second (data-slot current ns-info))]
                     (if (link? argument ns-info)
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

(defn- settle-head
  "Settle what a head names now that every definition in the project is known."
  [{:keys [kind var name]
    :as head} definitions]
  (case kind
    :local (if-let [defined (get definitions var)]
             {:kind (if (= :fn defined) :project :other)}
             {:kind :core
              :library :core
              :name name})
    :var {:kind (if (= :fn (get definitions var)) :project :other)}
    head))

(defn- step?
  "True when a level is a step the scan can vouch for.

  A one-argument call needs a head known to be a function. A call of more is a step only
  for a head whose data slot is known, core or `clojure.string` by name, or a method call."
  [{:keys [direction arity]
    :as level} definitions]
  (let [{:keys [kind library name method?]} (settle-head (:head level) definitions)]
    (if (= :either direction)
      (case kind
        (:keyword :host :project) true
        :core (contains? (if (= :string library) string-fns core-fns) name)
        false)
      (case kind
        :core (let [[slot arity?] (get data-slots [library name])]
                (boolean (and slot (arity? arity))))
        :host (boolean (and method? (= :first direction)))
        false))))

(defn- split-by-direction
  "Split run where its steps stop agreeing on which slot they thread into.

  One-argument steps agree with either; `(get (map f xs) k)` reads as neither thread."
  [run]
  (:runs (reduce (fn [{:keys [runs direction]} {step-direction :direction
                                                :as step}]
                   (if (or (= :either step-direction) (nil? direction) (= direction step-direction))
                     {:runs (conj (pop runs) (conj (peek runs) step))
                      :direction (if (= :either step-direction) direction step-direction)}
                     {:runs (conj runs [step])
                      :direction step-direction}))
                 {:runs [[]]
                  :direction nil}
                 run)))

(defn- runs
  "Split levels into the maximal runs whose every level is a step, threading one way."
  [levels definitions]
  (->> levels
       (partition-by #(step? % definitions))
       (filter #(step? (first %) definitions))
       (mapcat split-by-direction)))

(defn- drop-wrappers [run]
  (drop-while #(contains? wrappers (:head-name %)) run))

(defn- keep-literal-call
  "Leave a call on a literal as the thread's starting value rather than a step of it.

  `(:question (:params (request-to \"/qa\")))` threads to `(-> (request-to \"/qa\") :params
  :question)`: a path or a constant reads backwards at the head of a thread."
  [run]
  (if (:literal-argument? (last run)) (butlast run) run))

(def ^:private text-max 40)

(defn- shown
  "Return text as a rewrite shows it: whole, or, given a limit, `...` once longer or spanning lines."
  [text limit]
  (if (and limit (or (> (count text) limit) (str/includes? text "\n")))
    "..."
    text))

(defn- rewrite
  "Spell out the thread for run, every part whole or, given a limit, shortened."
  [run limit]
  (let [operator (if (some #(= :last (:direction %)) run) "->>" "->")
        step (fn [{:keys [head-text other-arguments]}]
               (if (seq other-arguments)
                 (str "(" head-text " " (str/join " " (map #(shown % limit) other-arguments)) ")")
                 head-text))
        thread (str operator " " (shown (:argument-text (last run)) limit) " "
                    (str/join " " (map step (reverse run))))]
    (if (:fn-literal? (first run))
      (str "#(" thread ")")
      (str "(" thread ")"))))

(defn- finding [{:keys [file test]} run]
  (let [[row col] (:position (first run))]
    (cond-> {:kw '->
             :type :nested-call-chain
             :form (-> run first :form-text parser/normalize-form)
             :file file
             :row row
             :col col
             :rewrite (rewrite run nil)
             :hint (str "Thread it: " (rewrite run text-max))}
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
  (str min-calls " or more calls nested one inside another, each passing the one below it "
       "where a thread would: the first argument, read with ->, or the last, read with ->>. "
       "The reader has to start at the innermost form and work outwards; a thread reads top "
       "to bottom in the order the calls run. Rewrite it as the finding spells out. A "
       "one-argument call counts when its head is known to be a function: a keyword, "
       "interop, core and clojure.string functions, and the project's own defn and "
       "defmulti. A call of more arguments counts only where the nested call is the "
       "function's data, the collection of (map f xs) or the map of (get m k), for core, "
       "clojure.string and method calls. A " (str/join ", " (sort wrappers)) " at the top of "
       "the thread is not counted, and a call on a literal stays the thread's starting value."))

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
