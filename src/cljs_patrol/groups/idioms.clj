(ns cljs-patrol.groups.idioms
  "Rule group: code a core idiom says more plainly.

  Nested calls a threading macro would read top to bottom, a `cond` closing on an
  `:else nil` it returns anyway, an `if` or `when` testing a `not`, and an `into` from a
  collection into an empty vector or set. Each rule reports only what it can rewrite
  without a judgement call.

  For a thread, every step passes the call below it in the slot a thread fills, the first
  argument for `->` or the last for `->>`, so the rewrite expands back to exactly the code
  that was written. That slot also has to be where the function takes its data:
  `(get-in db (path))` nests a call, but the call is the path, and threading the path
  reads backwards. So a step with more than one argument counts only for a function whose
  data slot is known: a core or `clojure.string` function, or a `.method` call.

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

(def ^:private data-slots
  "Where a function of more than one argument takes its data, and the argument counts it does so at.

  Keyed by library and name. A count outside the set has no data slot a thread can fill:
  `(map f)` is a transducer, and `(map f xs ys)` maps over two collections."
  {[:core "map"] [:last #{2}]
   [:core "mapv"] [:last #{2}]
   [:core "mapcat"] [:last #{2}]
   [:core "map-indexed"] [:last #{2}]
   [:core "filter"] [:last #{2}]
   [:core "filterv"] [:last #{2}]
   [:core "remove"] [:last #{2}]
   [:core "keep"] [:last #{2}]
   [:core "keep-indexed"] [:last #{2}]
   [:core "take"] [:last #{2}]
   [:core "drop"] [:last #{2}]
   [:core "take-while"] [:last #{2}]
   [:core "drop-while"] [:last #{2}]
   [:core "take-last"] [:last #{2}]
   [:core "drop-last"] [:last #{2}]
   [:core "sort"] [:last #{2}]
   [:core "sort-by"] [:last #{2 3}]
   [:core "group-by"] [:last #{2}]
   [:core "partition"] [:last #{2 3 4}]
   [:core "partition-all"] [:last #{2 3}]
   [:core "partition-by"] [:last #{2}]
   [:core "interpose"] [:last #{2}]
   [:core "some"] [:last #{2}]
   [:core "every?"] [:last #{2}]
   [:core "not-any?"] [:last #{2}]
   [:core "reduce"] [:last #{2 3}]
   [:core "into"] [:last #{2 3}]
   [:core "cons"] [:last #{2}]
   [:string "join"] [:last #{2}]
   [:core "get"] [:first #{2 3}]
   [:core "get-in"] [:first #{2 3}]
   [:core "assoc"] [:first #(and (>= % 3) (odd? %))]
   [:core "assoc-in"] [:first #{3}]
   [:core "update"] [:first #(>= % 3)]
   [:core "update-in"] [:first #(>= % 3)]
   [:core "dissoc"] [:first #(>= % 2)]
   [:core "select-keys"] [:first #{2}]
   [:core "merge"] [:first #(>= % 2)]
   [:core "conj"] [:first #(>= % 2)]
   [:core "disj"] [:first #(>= % 2)]
   [:core "contains?"] [:first #{2}]
   [:core "find"] [:first #{2}]
   [:core "nth"] [:first #{2 3}]
   [:core "subs"] [:first #{2 3}]
   [:core "update-vals"] [:first #{2}]
   [:core "update-keys"] [:first #{2}]
   [:string "split"] [:first #{2 3}]
   [:string "replace"] [:first #{3}]
   [:string "replace-first"] [:first #{3}]
   [:string "starts-with?"] [:first #{2}]
   [:string "ends-with?"] [:first #{2}]
   [:string "includes?"] [:first #{2}]
   [:string "index-of"] [:first #{2 3}]})

(def ^:private core-namespaces #{"clojure.core" "cljs.core"})

(defn- wrapper?
  "True when a call named head-name turns what it wraps into a test or an empty-or-nil.

  Ending a thread, one reads worse than wrapped around it: `(not (str/blank? (:title m)))`
  threads to `(-> m :title str/blank? not)`, and `(contains? (set ids) id)` to
  `(-> ids set (contains? id))`. At the top of what would be threaded, one is not counted
  toward the chain, whether or not something the chain stops at sits above it:
  `(is (nil? (:id (first xs))))` reads no better as `(is (-> xs first :id nil?))`. A
  predicate is a name ending in `?`; `not`, `boolean`, `seq`, `not-empty` and `some` are the
  others."
  [head-name]
  (boolean
   (when head-name
     (or (str/ends-with? head-name "?")
         (contains? #{"not" "boolean" "seq" "not-empty" "some"} head-name)))))

(defn- code-children
  "Return the child nodes of node other than whitespace, or nil when one is a comment or `#_`.

  Neither has a place in a thread, and either can stand where an argument would, so a call
  holding one is not read as a step at all."
  [node]
  (let [children (remove n/whitespace? (n/children node))]
    (when-not (some #(contains? #{:comment :uneval} (n/tag %)) children)
      children)))

(defn- code-forms
  "Return the child nodes of node that are code: no whitespace, comment or `#_` form."
  [node]
  (remove #(or (n/whitespace-or-comment? %) (= :uneval (n/tag %))) (n/children node)))

(defn- index-in-parent
  "Return the position of loc among its parent's children, the head at 0, or nil."
  [loc]
  (when-let [parent (z/up loc)]
    (->> (code-forms (z/node parent))
         (keep-indexed (fn [i child] (when (identical? child (z/node loc)) i)))
         first)))

(def ^:private threading-heads
  "Forms that pass every step after the first an argument the step does not write out."
  #{"->" "->>" "some->" "some->>" "doto" ".."})

(def ^:private conditional-threading-heads
  "Forms that pass that argument only to the step of each test-and-step pair."
  #{"cond->" "cond->>"})

(defn- site
  "Return the outermost of loc and what wraps it without changing it: metadata, a reader conditional."
  [loc]
  (let [parent (z/up loc)]
    (cond
      (= :meta (some-> parent z/tag)) (recur parent)
      (= :reader-macro (some-> parent z/up z/tag)) (recur (z/up parent))
      :else loc)))

(defn- parent-head
  "Return the name of the symbol heading the list loc's site sits in, or nil."
  [loc]
  (let [parent (z/up (site loc))]
    (when (= :list (some-> parent z/tag))
      (some-> parent z/down parser/sym-name))))

(def ^:private first-slot-threading-heads
  "Threads that pass their value as the first argument, so each step takes one argument more than it writes."
  #{"->" "some->" "doto" ".." "cond->"})

(defn- spliced?
  "True when loc is an element of a `#?@` splice, which lands where the source does not show."
  [loc]
  (let [vector-loc (z/up loc)
        branch-loc (some-> vector-loc z/up)
        reader-loc (some-> branch-loc z/up)]
    (and (= :vector (some-> vector-loc z/tag))
         (= :list (some-> branch-loc z/tag))
         (= :reader-macro (some-> reader-loc z/tag))
         (str/starts-with? (z/string reader-loc) "#?@"))))

(defn- threaded-step?
  "True when loc is a step of a threading form, so it takes an argument it does not show.

  `(doto cell (.setCellValue (double x)))` calls `.setCellValue` with two arguments. A
  `cond->` test takes none, and an `as->` step writes its argument out. Metadata and a
  reader conditional around loc do not move it, and a form spliced in by `#?@` may land
  anywhere, so it counts as a step. A threading form that is itself a step of a first-slot
  thread takes one argument more than it writes, so its steps sit one further along: the
  `if` of `(-> m (cond-> flag (if a b)))` is a step."
  [loc]
  (let [at (site loc)
        head (parent-head loc)]
    (cond
      (spliced? at) true
      (not (or (contains? threading-heads head) (contains? conditional-threading-heads head))) false
      :else (let [thread-loc (z/up at)
                  index (+ (index-in-parent at)
                           (if (and (contains? first-slot-threading-heads (parent-head thread-loc))
                                    (threaded-step? thread-loc))
                             1
                             0))]
              (or (and (contains? threading-heads head) (>= index 2))
                  (and (contains? conditional-threading-heads head) (>= index 3) (odd? index)))))))

(defn- class-name? [segment]
  (let [initial (first segment)]
    (boolean (and initial (Character/isUpperCase (char initial))))))

(defn- interop-or-host?
  "True when sym-str names a host call: `.method`, `.-field`, `Ctor.`, `js/f` or `Class/static`.

  A qualifier names a class when its last segment is capitalized, `java.util.UUID/randomUUID`
  as much as `ImageIO/read`; a lower-case one, `some.lib/f`, is a namespace. `..` is the
  member-access threading macro, not a member."
  [sym-str]
  (let [slash (str/index-of sym-str "/")
        qualifier (when (and slash (pos? slash)) (subs sym-str 0 slash))]
    (boolean
     (and (not= ".." sym-str)
          (or (and (str/starts-with? sym-str ".") (> (count sym-str) 1))
              (and (str/ends-with? sym-str ".") (> (count sym-str) 1) (nil? slash))
              (= "js" qualifier)
              (class-name? (some-> qualifier (str/split #"\.") last)))))))

(def ^:private let-like-heads
  "Forms whose first argument is a vector of binding pairs."
  #{"let" "let*" "loop" "loop*" "when-let" "if-let" "when-some" "if-some" "when-first"
    "binding" "with-open" "with-redefs" "with-let" "with-local-vars" "doseq" "for" "dotimes"
    "go-loop"})

(def ^:private fn-like-heads #{"fn" "fn*" "defn" "defn-" "defmacro" "defmethod"})

(defn- token-value [node]
  (when (= :token (n/tag node))
    (try (n/sexpr node) (catch Exception _ nil))))

(defn- keys-entry?
  "True when node is the key of a destructuring map entry naming its locals: `:keys`, `:syms`, `:strs`."
  [node]
  (let [value (token-value node)]
    (and (keyword? value) (contains? #{"keys" "syms" "strs"} (name value)))))

(defn- symbol-names
  "Return the names node binds when it sits on the binding side, at any depth.

  An unqualified symbol, and every symbol or keyword in the vector of a `:keys`, `:syms`
  or `:strs` entry, `{:keys [:cond app/id]}` binding `cond` and `id`. A default under
  `:or` reads as bound too, which only gives up a finding."
  [node]
  (letfn [(keyed-names [vector-node]
            (keep #(let [value (token-value %)]
                     (when (or (symbol? value) (keyword? value)) (name value)))
                  (code-forms vector-node)))
          (names [node]
            (cond
              (= :token (n/tag node))
              (let [value (token-value node)]
                (when (and (symbol? value) (nil? (namespace value))) [(name value)]))

              (= :map (n/tag node))
              (->> (code-forms node)
                   (partition-all 2)
                   (mapcat (fn [[k v]]
                             (if (and (keys-entry? k) (= :vector (some-> v n/tag)))
                               (keyed-names v)
                               (concat (names k) (some-> v names))))))

              (n/inner? node)
              (mapcat names (n/children node))))]
    (set (names node))))

(defn- binding-sides
  "Return the binding-side nodes of a binding vector, `:let` modifiers included."
  [vector-node]
  (let [elements (vec (code-forms vector-node))]
    (->> (range 0 (count elements) 2)
         (mapcat (fn [i]
                   (let [binding-node (nth elements i)
                         value-node (get elements (inc i))]
                     (if (and (= :let (try (n/sexpr binding-node) (catch Exception _ nil)))
                              (= :vector (some-> value-node n/tag)))
                       (binding-sides value-node)
                       [binding-node])))))))

(defn- parameter-vectors
  "Return the parameter vectors of a fn-like form from its arguments.

  The first vector is the parameter list of a single arity, and the vector opening each
  arity list of a multi-arity form. A `defmethod` has its dispatch value first, which may be
  a vector too. A vector in a body, the return of a Hiccup view, is not a parameter list."
  [head-name arguments]
  (let [after-dispatch (if (= "defmethod" head-name) (drop 2 arguments) arguments)]
    (if-let [single (first (filter #(= :vector (n/tag %)) after-dispatch))]
      [single]
      (keep #(when (= :list (n/tag %))
               (let [opening (first (code-forms %))]
                 (when (= :vector (some-> opening n/tag)) opening)))
            after-dispatch))))

(defn- names-bound-by
  "Return the names list-node binds for its body.

  Let-like and `letfn` bindings, the parameters of a fn or any `def`-headed form, a
  `defui` or `defnc` as much as a `defn`, a named fn's own name, the fields of a `deftype`
  or `defrecord`, the name of a `catch` or `as->`, and the index and result of an `amap`
  or `areduce`. A comment or `#_` form among them changes nothing they bind."
  [list-node]
  (let [[head & arguments] (code-forms list-node)
        head-name (when (= :token (some-> head n/tag))
                    (some-> (try (n/sexpr head) (catch Exception _ nil)) name))]
    (cond
      (contains? let-like-heads head-name)
      (some->> arguments first binding-sides (mapcat symbol-names) set)

      (= "letfn" head-name)
      (some->> arguments first code-forms (keep #(some-> % code-forms first symbol-names)) (apply concat) set)

      (or (contains? fn-like-heads head-name) (some-> head-name (str/starts-with? "def")))
      (cond-> (->> (parameter-vectors head-name arguments)
                   (mapcat symbol-names)
                   set)
        (and (= "fn" head-name) (= :token (some-> arguments first n/tag)))
        (into (symbol-names (first arguments))))

      (contains? #{"catch" "as->"} head-name)
      (some-> (nth arguments 1 nil) symbol-names)

      (contains? #{"amap" "areduce"} head-name)
      (->> (take 2 (rest arguments))
           (mapcat symbol-names)
           set))))

(def ^:private method-defining-heads
  "Forms whose lists define methods: a list's head names the method, and its vector the parameters."
  #{"reify" "deftype" "defrecord" "extend-type" "extend-protocol" "proxy" "specify" "specify!"
    "definterface" "defprotocol"})

(defn- method-definition?
  "True when loc is a list defining a method, not a call: `(toString [_] …)` in a `reify`."
  [loc]
  (and (= :list (z/tag loc))
       (contains? method-defining-heads (parent-head loc))))

(defn- method-parameter-names
  "Return the names a method definition binds for its body, one arity or several."
  [loc]
  (->> (code-forms (z/node loc))
       rest
       (parameter-vectors "fn")
       (mapcat symbol-names)
       set))

(defn- locally-bound?
  "True when a form enclosing loc binds the unqualified name, so it is a local and not a var.

  Every binding form above loc is read, whatever part of it loc sits in: a local that shadows
  `filter` in one branch is reason enough to vouch for no `filter` there. Only a name core's
  data-slot table holds, or `cond`, is ever asked about, so the walk runs for few calls."
  [loc unqualified-name]
  (loop [current (z/up loc)]
    (cond
      (nil? current) false
      (and (contains? #{:list :fn} (z/tag current))
           (contains? (names-bound-by (z/node current)) unqualified-name)) true
      (and (method-definition? current)
           (contains? (method-parameter-names current) unqualified-name)) true
      :else (recur (z/up current)))))

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
      (->> loc z/down parser/right-siblings vec))))

(defn- data-slot
  "Return [direction argument-loc head] for the argument loc threads, or nil when it threads none.

  One argument threads either way, `:either`. With more, the head's data slot decides:
  `:last` for `(map f xs)`, `:first` for `(get m k)` and a method call. Which function the
  head names, and whether it takes this many arguments, is settled once every file is
  read; this reads it as core unless the namespace says otherwise, and a head that turns
  out to be something else ends the chain. An unqualified name a form above binds is a
  local, and core's data slot says nothing about it: `(filter g xs)` threads nothing once
  `filter` is destructured. A local called with one argument still threads, since a local
  is never a macro."
  [loc ns-info]
  (when-let [arguments (seq (call-arguments loc))]
    (let [{:keys [kind library name method?]
           :as head} (describe-head (z/down loc) ns-info)]
      (if (= 1 (count arguments))
        [:either (first arguments) head]
        (let [[direction] (when (and (contains? #{:core :local} kind)
                                     (not (and (= :local kind) (locally-bound? loc name))))
                            (get data-slots [(or library :core) name]))]
          (cond
            direction [direction (if (= :last direction) (last arguments) (first arguments)) head]
            (and (= :host kind) method?) [:first (first arguments) head]))))))

(defn- link-shaped?
  "True when loc has the shape of a link, whatever its head.

  A link is a list that is not a step of a threading form and not the list a reader
  conditional holds, `#?(:clj x)` reading as `(:clj x)`. A `#(…)` is a function value
  wherever it is an argument, so it can only start a chain."
  [loc]
  (and (= :list (z/tag loc))
       (not= :reader-macro (some-> loc z/up z/tag))
       (not (threaded-step? loc))))

(defn- inner-link?
  "True when loc, a call, is a link the chain above it walks into, so no chain starts at loc."
  [loc ns-info]
  (let [parent (z/up loc)]
    (boolean
     (and parent
          (link-shaped? loc)
          (or (link-shaped? parent) (= :fn (z/tag parent)))
          (identical? (z/node loc) (some-> (data-slot parent ns-info) second z/node))))))

(def ^:private literal-reader-tags #{"#js" "#inst" "#uuid"})

(defn- literal?
  "True when loc is written out as a value: a string, number, keyword, quoted form, or collection literal."
  [loc]
  (case (z/tag loc)
    (:map :vector :set :multi-line :regex :quote) true
    :token (not (symbol? (try (z/sexpr loc) (catch Exception _ nil))))
    :reader-macro (boolean (some #(str/starts-with? (z/string loc) %) literal-reader-tags))
    false))

(defn- level [loc [direction argument head]]
  (let [arguments (call-arguments loc)]
    {:head head
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
  here: setting a head aside later only shortens it. Each loc's data slot is read once."
  [min-calls loc ns-info file]
  (when (and (or (link-shaped? loc) (= :fn (z/tag loc)))
             (not (inner-link? loc ns-info)))
    (when-let [slot (data-slot loc ns-info)]
      (let [levels (loop [current loc
                          current-slot slot
                          found []]
                     (let [found (conj found (level current current-slot))
                           argument (second current-slot)
                           next-slot (when (link-shaped? argument) (data-slot argument ns-info))]
                       (if next-slot
                         (recur argument next-slot found)
                         found)))]
        (when (and (>= (count levels) min-calls) (not (unevaluated? loc)))
          {:type ::chain
           :levels levels
           :file file
           :test (parser/top-level-name loc)})))))

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

(defn- always-true?
  "True when node is a test no value can fail: a keyword or `true`."
  [node]
  (and (= :token (n/tag node))
       (let [value (n/sexpr node)]
         (or (keyword? value) (true? value)))))

(defn- written-nil? [node]
  (and (= :token (n/tag node))
       (nil? (n/sexpr node))))

(defn- case-constant?
  "True when loc is a test constant of the `case` it sits in, a list of constants and no call."
  [loc]
  (boolean
   (when (= "case" (parent-head loc))
     (let [index (index-in-parent (site loc))
           size (->> (site loc) z/up z/node code-forms count)]
       (and (even? index)
            (not (and (odd? size) (= index (dec size)))))))))

(defn- member-position?
  "True when loc names a member of `.` or `..`, `(. o (cond a b))` calling a method `cond`."
  [loc]
  (and (contains? #{"." ".."} (parent-head loc))
       (>= (index-in-parent (site loc)) 2)))

(defn- core-name-shadowed?
  "True when the ns form may take the unqualified name from somewhere other than core."
  [{:keys [shadowed-core]} unqualified-name]
  (or (= :all shadowed-core)
      (contains? shadowed-core unqualified-name)))

(defn- core-name-unshadowed?
  "True when no name the ns form takes and no local above loc hides core's unqualified-name."
  [loc ns-info unqualified-name]
  (not (or (core-name-shadowed? ns-info unqualified-name)
           (locally-bound? loc unqualified-name))))

(defn- core-call-head
  "Return the head of loc when it may call core's core-name, or nil.

  Whether the head is core's is settled once every file is read, since the namespace may
  define the name itself. An unqualified head is core's only when the ns form names it
  nowhere and nothing above binds it, and only a list in call position counts: a method a
  `reify` defines, a member of `..` and a `case` constant list are no calls."
  [loc ns-info core-name]
  (when (= core-name (some-> loc z/down parser/sym-name))
    (let [head (describe-head (z/down loc) ns-info)]
      (when (and (= core-name (:name head))
                 (or (not= :local (:kind head)) (core-name-unshadowed? loc ns-info core-name))
                 (not (method-definition? loc))
                 (not (member-position? loc))
                 (not (case-constant? loc))
                 (not (unevaluated? loc)))
        head))))

(defn- else-nil-candidate
  "Record a `cond` whose last clause pairs an always-true test with `nil`, or nil."
  [loc ns-info file]
  (when-let [head (core-call-head loc ns-info "cond")]
    (let [arguments (rest (code-forms (z/node loc)))
          [test-node value-node] (take-last 2 arguments)]
      (when (and (>= (count arguments) 4)
                 (even? (count arguments))
                 (always-true? test-node)
                 (written-nil? value-node))
        {:type ::else-nil
         :head head
         :head-text (z/string (z/down loc))
         :fn-literal? (= :fn (z/tag loc))
         :first-clause (map n/string (take 2 arguments))
         :clauses-between (- (quot (count arguments) 2) 2)
         :else-text (n/string test-node)
         :position (try (z/position loc) (catch Exception _ [0 1]))
         :file file
         :test (parser/top-level-name loc)}))))

(def ^:private negatable-forms
  "Forms a `not` around the test folds into, with the form that folds it and the argument counts they take."
  {"if" ["if-not" #{2 3}]
   "when" ["when-not" any?]})

(defn- describe-name
  "Classify an unqualified name the file does not write, as describe-head would if it did.

  A name the ns form refers is shadowed already, so this reads it as the namespace's own."
  [unqualified-name {:keys [ns-name]}]
  {:kind :local
   :var (keyword ns-name unqualified-name)
   :name unqualified-name})

(defn- negated-test-candidate
  "Record an `if` or `when` whose test is a call of `not`, or nil.

  The form the finding asks for, `if-not` or `when-not`, has to be core's too. A step of a
  thread takes its test from the thread, so its first argument is no test. A reader
  conditional the `not` holds may read as nothing on one platform, where `(not)` and
  `when-not` part ways."
  [loc ns-info file]
  (let [form-name (some-> loc z/down parser/sym-name)
        [folded-name arity?] (get negatable-forms form-name)]
    (when-let [head (when folded-name (core-call-head loc ns-info form-name))]
      (let [[test-node & body] (rest (code-forms (z/node loc)))]
        (when (and (arity? (inc (count body)))
                   (= :list (some-> test-node n/tag))
                   (not (threaded-step? loc))
                   (core-name-unshadowed? loc ns-info folded-name))
          (let [test-loc (->> loc z/down parser/right-siblings
                              (filter #(identical? test-node (z/node %)))
                              first)
                [negated & more] (rest (code-forms test-node))]
            (when-let [not-head (when (and negated
                                           (empty? more)
                                           (not= :reader-macro (n/tag negated)))
                                  (core-call-head test-loc ns-info "not"))]
              {:type ::negated-test
               :form-name form-name
               :folded-name folded-name
               :head head
               :not-head not-head
               :folded-head (describe-name folded-name ns-info)
               :head-text (z/string (z/down loc))
               :fn-literal? (= :fn (z/tag loc))
               :test-text (n/string test-node)
               :negated-text (n/string negated)
               :body (map n/string body)
               :position (try (z/position loc) (catch Exception _ [0 1]))
               :file file
               :test (parser/top-level-name loc)})))))))

(def ^:private into-empty-builders
  "Empty collection literals `into` can fill, with the core function that builds the same collection."
  {:vector "vec"
   :set "set"})

(def ^:private fresh-collection-fns
  "Core functions that return a seq or a new plain vector, list, hash set or hash map.

  Never an array, which `vec` aliases where `into` copies it; never a set, which `set`
  returns as it is where `into` builds a hash set, a sorted one included; and never a
  reducible `vec` and `set` cannot read. `conj` and `merge` are not among them: given one
  argument, they return it."
  #{"map" "mapcat" "map-indexed" "filter" "remove" "keep" "keep-indexed" "concat" "distinct"
    "dedupe" "keys" "vals" "range" "repeat" "take" "drop" "take-while" "drop-while" "take-last"
    "drop-last" "butlast" "rest" "next" "sort" "sort-by" "reverse" "partition" "partition-all"
    "partition-by" "interpose" "interleave" "flatten" "cons" "list" "vector" "hash-set"
    "tree-seq" "re-seq" "zipmap" "group-by" "frequencies" "select-keys"})

(defn- fresh-source
  "Return what vouches that node, the source of an `into`, reads the same to `vec` or `set`, or nil.

  A collection literal vouches for itself, as `:literal`. A call vouches through its head,
  which has to settle as one of the core functions in `fresh-collection-fns`."
  [node source-loc ns-info]
  (case (n/tag node)
    (:vector :map :set :quote) :literal
    :list (let [head-name (some-> source-loc z/down parser/sym-name)]
            (when (contains? fresh-collection-fns head-name)
              (core-call-head source-loc ns-info head-name)))
    nil))

(defn- into-empty-candidate
  "Record an `into` from one collection into an empty vector or set literal, or nil.

  `vec` aliases an array, and `set` returns a set as it is, a sorted one included, where
  `into` builds a new collection either way. So it counts only where the source is a
  literal or a call `fresh-source` vouches for. A step of a thread takes its source from
  the thread."
  [loc ns-info file]
  (when-let [head (core-call-head loc ns-info "into")]
    (let [[target source & more] (rest (code-forms (z/node loc)))
          builder (get into-empty-builders (some-> target n/tag))]
      (when (and builder
                 source
                 (empty? more)
                 (empty? (code-forms target))
                 (not (threaded-step? loc))
                 (core-name-unshadowed? loc ns-info builder))
        (let [source-loc (->> loc z/down parser/right-siblings
                              (filter #(identical? source (z/node %)))
                              first)
              vouch (fresh-source source source-loc ns-info)]
          (when vouch
            {:type ::into-empty
             :builder builder
             :head head
             :builder-head (describe-name builder ns-info)
             :source-head (when (map? vouch) vouch)
             :fn-literal? (= :fn (z/tag loc))
             :head-text (z/string (z/down loc))
             :target-text (n/string target)
             :source-text (n/string source)
             :position (try (z/position loc) (catch Exception _ [0 1]))
             :file file
             :test (parser/top-level-name loc)}))))))

(defn- handle [min-calls loc ns-info file]
  (let [found (keep identity [(when (= :list (z/tag loc)) (definition loc ns-info file))
                              (chain-candidate min-calls loc ns-info file)
                              (else-nil-candidate loc ns-info file)
                              (negated-test-candidate loc ns-info file)
                              (into-empty-candidate loc ns-info file)])]
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

  Read from the innermost step up, so a one-argument step between a `->>` part below and a
  `->` part above joins the part below: `(contains? (set (map :id xs)) id)` keeps
  `(->> xs (map :id) set)` whole."
  [run]
  (loop [steps (reverse run)
         current []
         direction nil
         done []]
    (if-let [{step-direction :direction
              :as step} (first steps)]
      (if (or (= :either step-direction) (nil? direction) (= direction step-direction))
        (recur (rest steps)
               (conj current step)
               (if (= :either step-direction) direction step-direction)
               done)
        (recur (rest steps) [step] step-direction (conj done current)))
      (->> (conj done current)
           (map (comp vec reverse))
           reverse))))

(defn- runs
  "Split levels into the maximal runs whose every level is a step, threading one way."
  [levels definitions]
  (->> levels
       (partition-by #(step? % definitions))
       (filter #(step? (first %) definitions))
       (mapcat split-by-direction)))

(defn- drop-wrappers [run]
  (drop-while #(wrapper? (:head-name %)) run))

(defn- keep-literal-call
  "Leave a call on a literal as the thread's starting value rather than a step of it.

  `(:question (:params (request-to \"/qa\")))` threads to `(-> (request-to \"/qa\") :params
  :question)`: a path or a constant reads backwards at the head of a thread. A collection is
  a sequence pipeline's natural start, so a `->>` step on one stays a step:
  `(->> [3 1 2] (map inc) sort vec)`."
  [run]
  (let [innermost (last run)]
    (if (and (:literal-argument? innermost) (not= :last (:direction innermost)))
      (butlast run)
      run)))

(def ^:private text-max 40)

(defn- shown
  "Return text as a rewrite shows it: whole, or, given a limit, `...` once longer or spanning lines."
  [text limit]
  (if (and limit (or (> (count text) limit) (str/includes? text "\n")))
    "..."
    text))

(defn- operator [run]
  (if (some #(= :last (:direction %)) run) "->>" "->"))

(defn- rewrite
  "Spell out the thread for run, every part whole or, given a limit, shortened."
  [run limit]
  (let [step (fn [{:keys [head-text other-arguments]}]
               (if (seq other-arguments)
                 (str "(" head-text " " (str/join " " (map #(shown % limit) other-arguments)) ")")
                 head-text))
        steps (->> run reverse (map step) (str/join " "))
        thread (str (operator run) " " (shown (:argument-text (last run)) limit) " " steps)]
    (if (:fn-literal? (first run))
      (str "#(" thread ")")
      (str "(" thread ")"))))

(defn- finding [{:keys [file test]} run]
  (let [[row col] (:position (first run))]
    (cond-> {:kw (symbol (operator run))
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

(defn- longest-run
  "Return the longest run of a chain that is long enough to report, or nil.

  Threading two runs of one chain would nest one thread inside the other, so a chain is
  reported once, as its longest run; the innermost wins a tie."
  [min-calls runs]
  (->> runs
       (map (comp keep-literal-call drop-wrappers))
       (filter #(>= (count %) min-calls))
       (reduce (fn [best run] (if (>= (count run) (count best)) run best)) nil)))

(defn- core-named?
  "True when head, settled against the project's definitions, is core's core-name."
  [head definitions core-name]
  (= {:kind :core
      :library :core
      :name core-name}
     (select-keys (settle-head head definitions) [:kind :library :name])))

(defn- else-nil-form
  "Spell out a cond by its first clause and its else clause, counting the clauses between.

  This is the finding's identity in a baseline, so it leaves out what churns: the clauses
  between, and a first value too long to show."
  [{:keys [head-text fn-literal? first-clause clauses-between else-text]}]
  (let [[first-test first-value] first-clause]
    (str (if fn-literal? "#(" "(") head-text " " first-test " " (shown first-value text-max)
         (when (pos? clauses-between) (str " ..." clauses-between " more..."))
         " " else-text " nil)")))

(defn- else-nil-finding [{:keys [else-text position file test]
                          :as candidate}]
  (let [[row col] position]
    (cond-> {:kw 'cond
             :type :cond-else-nil
             :form (parser/normalize-form (else-nil-form candidate))
             :file file
             :row row
             :col col
             :hint (str "Drop the " else-text " nil clause: a cond no clause matches returns nil")}
      test (assoc :test test))))

(defn- negated-test-form
  "Spell out an `if` or `when` by its test and its first two branches, counting the rest.

  This is the finding's identity in a baseline, so a branch too long to show is left out."
  [{:keys [head-text fn-literal? test-text body]}]
  (let [shown-body (map #(shown % text-max) (take 2 body))
        more (- (count body) 2)]
    (str (if fn-literal? "#(" "(") head-text " " test-text
         (apply str (map #(str " " %) shown-body))
         (when (pos? more) (str " ..." more " more..."))
         ")")))

(defn- negated-test-finding [{:keys [form-name folded-name negated-text position file test]
                              :as candidate}]
  (let [[row col] position]
    (cond-> {:kw (symbol form-name)
             :type :negated-test
             :form (parser/normalize-form (negated-test-form candidate))
             :file file
             :row row
             :col col
             :hint (str "Write (" folded-name " " (shown negated-text text-max) " ...)")}
      test (assoc :test test))))

(defn- negated-test? [{:keys [form-name folded-name head not-head folded-head]} definitions]
  (and (core-named? head definitions form-name)
       (core-named? not-head definitions "not")
       (core-named? folded-head definitions folded-name)))

(defn- into-empty? [{:keys [builder head builder-head source-head]} definitions]
  (and (core-named? head definitions "into")
       (core-named? builder-head definitions builder)
       (or (nil? source-head) (core-named? source-head definitions (:name source-head)))))

(defn- into-empty-finding [{:keys [builder fn-literal? head-text target-text source-text position file test]}]
  (let [[row col] position]
    (cond-> {:kw 'into
             :type :into-empty
             :form (parser/normalize-form (str (if fn-literal? "#(" "(") head-text " " target-text " "
                                               source-text ")"))
             :file file
             :row row
             :col col
             :hint (str "Write (" builder " " (shown source-text text-max) ")")}
      test (assoc :test test))))

(defn- analyze* [min-calls {:keys [declarations]}]
  (let [definitions (function-definitions declarations)]
    {:nested-call-chain
     (->> declarations
          (filter #(= ::chain (:type %)))
          (keep (fn [candidate]
                  (some->> (runs (:levels candidate) definitions)
                           (longest-run min-calls)
                           (finding candidate))))
          vec)
     :cond-else-nil
     (->> declarations
          (filter #(and (= ::else-nil (:type %)) (core-named? (:head %) definitions "cond")))
          (mapv else-nil-finding))
     :negated-test
     (->> declarations
          (filter #(and (= ::negated-test (:type %)) (negated-test? % definitions)))
          (mapv negated-test-finding))
     :into-empty
     (->> declarations
          (filter #(and (= ::into-empty (:type %)) (into-empty? % definitions)))
          (mapv into-empty-finding))}))

(defn- suggestion [min-calls]
  (str min-calls " or more calls nested one inside another, each passing the one below it "
       "where a thread would: the first argument, read with ->, or the last, read with ->>. "
       "The reader has to start at the innermost form and work outwards; a thread reads top "
       "to bottom in the order the calls run. Rewrite it as the finding spells out. A "
       "one-argument call counts when its head is known to be a function: a keyword, "
       "interop, core and clojure.string functions, and the project's own defn and "
       "defmulti. A call of more arguments counts only where the nested call is the "
       "function's data, the collection of (map f xs) or the map of (get m k), for core and "
       "clojure.string functions and method calls. A predicate, not, boolean, seq, not-empty "
       "or some at the top of the thread is not counted, and a call on a literal stays the "
       "thread's starting value unless it is the collection of a ->> step."))

(def ^:private else-nil-suggestion
  (str "A cond whose last clause is :else nil, or another keyword or true paired with nil. "
       "A cond no clause matches returns nil already, so the clause adds nothing, yet it reads "
       "as a default someone chose, and the reader has to check that it is not. Drop it."))

(def ^:private negated-test-suggestion
  (str "An if or when whose test is (not x). if-not and when-not carry the negation in their "
       "name, so the test reads as written, with one pair of parentheses less. Write "
       "(if-not x a b) or (when-not x ...)."))

(def ^:private into-empty-suggestion
  (str "(into [] xs) or (into #{} xs): pouring one collection into an empty vector or set. vec "
       "and set name the collection they build. Write (vec xs) or (set xs). It counts only "
       "where xs is a literal or a call of a core function returning a seq or a new plain "
       "collection: vec aliases an array, and set returns a set, a sorted one included, "
       "where into builds a new collection."))

(defrecord IdiomsGroup [min-calls extensions]
  group/RuleGroup
  (group-id [_] :idioms)
  (group-name [_] "Idioms")
  (parse-handlers [_] {:handle-list (fn [loc ns-info file] (handle min-calls loc ns-info file))})
  (analyze [_ data] (analyze* min-calls data))
  (summary-lines [_ {:keys [nested-call-chain cond-else-nil negated-test into-empty]}]
    [["Nested call chain:" (count nested-call-chain)]
     ["Cond else nil:" (count cond-else-nil)]
     ["Negated test:" (count negated-test)]
     ["Into empty:" (count into-empty)]])
  (failed? [_ _] false)
  (suggestions [_] {:nested-call-chain (suggestion min-calls)
                    :cond-else-nil else-nil-suggestion
                    :negated-test negated-test-suggestion
                    :into-empty into-empty-suggestion})
  (rule->tier [_] {:nested-call-chain :cleanup
                   :cond-else-nil :cleanup
                   :negated-test :cleanup
                   :into-empty :cleanup})
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
