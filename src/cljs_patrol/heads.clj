(ns cljs-patrol.heads
  "What the head of a form names: core's var, a library's, or a local.

  A rule matching a form by its head's bare name reads a look-alike as the real thing,
  Schema's `defn` with a `:- [schema]` return before its parameters as core's. Here a
  head resolves through the ns form's aliases and refers, and an unqualified name counts
  as core's only when neither the ns form nor a binding around it may shadow it."
  (:require
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn code-forms
  "Return the child nodes of node that are code: no whitespace, comment or `#_` form."
  [node]
  (remove #(or (n/whitespace-or-comment? %) (= :uneval (n/tag %))) (n/children node)))

(defn site
  "Return the outermost of loc and what wraps it without changing it: metadata, a reader conditional."
  [loc]
  (let [parent (z/up loc)]
    (cond
      (= :meta (some-> parent z/tag)) (recur parent)
      (= :reader-macro (some-> parent z/up z/tag)) (recur (z/up parent))
      :else loc)))

(defn parent-head
  "Return the name of the symbol heading the list or `#(…)` loc's site sits in, or nil."
  [loc]
  (let [parent (z/up (site loc))]
    (when (contains? #{:list :fn} (some-> parent z/tag))
      (some-> parent z/down parser/sym-name))))

(def ^:private let-like-heads
  "Forms whose first argument is a vector of binding pairs."
  #{"let" "let*" "loop" "loop*" "when-let" "if-let" "when-some" "if-some" "when-first"
    "binding" "with-open" "with-redefs" "with-let" "with-local-vars" "doseq" "for" "dotimes"
    "go-loop"})

(def ^:private fn-like-heads #{"fn" "fn*" "defn" "defn-" "defmacro" "defmethod"})

(defn token-value
  "Return the value a token node holds, or nil for any other node."
  [node]
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

  Let-like bindings, the names and parameters of a `letfn`'s functions, the parameters of a
  fn or any `def`-headed form, a `defui` or `defnc` as much as a `defn`, a named fn's own
  name, the fields of a `deftype` or `defrecord`, the name of a `catch`, `as->` or
  `this-as`, the template names of an `are`, and the index and result of an `amap` or
  `areduce`. An `as->` that is a step of a thread has its name first, so either of its first
  two symbols counts. A comment or `#_` form among them changes nothing they bind."
  [list-node]
  (let [[head & arguments] (code-forms list-node)
        head-name (when (= :token (some-> head n/tag))
                    (some-> (try (n/sexpr head) (catch Exception _ nil)) name))]
    (cond
      (contains? let-like-heads head-name)
      (some->> arguments first binding-sides (mapcat symbol-names) set)

      (= "letfn" head-name)
      (some->> arguments first code-forms
               (mapcat #(let [[fn-name & fn-arguments] (code-forms %)]
                          (concat (some-> fn-name symbol-names)
                                  (mapcat symbol-names (parameter-vectors "fn" fn-arguments)))))
               set)

      (or (contains? fn-like-heads head-name) (some-> head-name (str/starts-with? "def")))
      (cond-> (->> (parameter-vectors head-name arguments)
                   (mapcat symbol-names)
                   set)
        (and (= "fn" head-name) (= :token (some-> arguments first n/tag)))
        (into (symbol-names (first arguments))))

      (= "catch" head-name)
      (some-> (nth arguments 1 nil) symbol-names)

      (= "as->" head-name)
      (->> (take 2 arguments)
           (filter #(= :token (n/tag %)))
           (mapcat symbol-names)
           set)

      (= "this-as" head-name)
      (some-> (first arguments) symbol-names)

      (= "are" head-name)
      (some-> (first arguments) symbol-names)

      (contains? #{"amap" "areduce"} head-name)
      (->> (take 2 (rest arguments))
           (mapcat symbol-names)
           set))))

(def ^:private method-defining-heads
  "Forms whose lists define methods: a list's head names the method, and its vector the parameters."
  #{"reify" "deftype" "defrecord" "extend-type" "extend-protocol" "proxy" "specify" "specify!"
    "definterface" "defprotocol"})

(defn method-definition?
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

(defn locally-bound?
  "True when a form enclosing loc binds the unqualified name, so it is a local and not a var.

  Every binding form above loc is read, whatever part of it loc sits in: a local that shadows
  `filter` in one branch is reason enough to vouch for no `filter` there."
  [loc unqualified-name]
  (loop [current (z/up loc)]
    (cond
      (nil? current) false
      (and (contains? #{:list :fn} (z/tag current))
           (contains? (names-bound-by (z/node current)) unqualified-name)) true
      (and (method-definition? current)
           (contains? (method-parameter-names current) unqualified-name)) true
      :else (recur (z/up current)))))

(defn core-name-shadowed?
  "True when the ns form may take the unqualified name from somewhere other than core."
  [{:keys [shadowed-core]} unqualified-name]
  (or (= :all shadowed-core)
      (contains? shadowed-core unqualified-name)))

(def ^:private special-forms
  "Forms no ns form or binding can shadow: the compiler reads them before any var or local."
  #{"." "case*" "catch" "def" "deftype*" "defrecord*" "do" "finally" "fn*" "if" "js*" "let*"
    "letfn*" "loop*" "new" "ns*" "quote" "recur" "set!" "throw" "try" "var"})

(defn resolve-head
  "Return the fully qualified symbol the form at `loc` opens with, core's read as `cljs.core`.

  The head resolves through the ns form's aliases and refers, its macro ones after. An
  unqualified name nothing refers is core's unless the ns form or a binding around the form
  may shadow it, so Schema's `defn`, which writes a return schema before its parameters, is
  never read as core's. A special form is core's always. Any other head is returned as
  written."
  [loc ns-info]
  (let [head (some-> loc z/down)
        head-name (parser/sym-name head)
        head-str (some-> head parser/raw)
        bare? (= head-name head-str)
        resolved (when head-name
                   (or (when (and bare? (contains? special-forms head-name))
                         (symbol "cljs.core" head-name))
                       (parser/resolve-full-symbol head-str ns-info)
                       (parser/resolve-full-symbol head-str {:aliases (:macro-aliases ns-info)
                                                             :refers (:macro-refers ns-info)})
                       (when-not (or (not bare?)
                                     (core-name-shadowed? ns-info head-name)
                                     (locally-bound? loc head-name))
                         (symbol "cljs.core" head-name))
                       (symbol head-str)))]
    (if (= "clojure.core" (some-> resolved namespace))
      (symbol "cljs.core" (name resolved))
      resolved)))
