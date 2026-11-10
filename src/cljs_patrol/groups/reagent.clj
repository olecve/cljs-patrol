(ns cljs-patrol.groups.reagent
  "Reagent rule group: detects suboptimal patterns in Reagent hiccup templates."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [raw (try (z/string loc) (catch Exception _ ""))
        collapsed (str/replace raw #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

(defn- hiccup-head-loc?
  "True when `loc` is a token that heads a Hiccup vector rather than a data one.

  A keyword counts only when it names an element: an arbitrary one heads a Malli
  schema, a re-frame db path or an event vector far more often than an element. Any
  symbol counts — Reagent components appear here as ordinary refs (bare or namespaced)."
  [loc]
  (and loc
       (= :token (z/tag loc))
       (or (-> loc parser/raw hiccup/parse-tag hiccup/html-tag?)
           (some? (parser/sym-name loc)))))

(defn- data-vector?
  "True when the vector's second element is also a keyword literal.
  The shape `[:ns :key …]` almost always encodes a re-frame path or
  Malli/spec schema, not a Hiccup element."
  [vec-loc]
  (let [second-loc (some-> vec-loc z/down z/right)]
    (and second-loc (parser/kw-node? second-loc))))

(defn- first-in-subtree
  "Return the first loc in `loc`'s subtree, `loc` included, satisfying pred."
  [pred loc]
  (loop [current (z/subzip loc)]
    (cond
      (z/end? current) nil
      (pred current) current
      :else (recur (z/next current)))))

(def ^:private function-heads #{"fn" "fn*"})

(defn- function-body-locs
  "Return the body forms of an `(fn [params] …)` literal, or nil for anything else.

  A name may sit between `fn` and the parameters, so the parameter list is whatever
  vector comes first. A multi-arity `fn` states no vector of its own and reads as nil,
  which costs only a finding this rule was never going to be sure of."
  [list-loc]
  (when (contains? function-heads (some-> list-loc z/down parser/sym-name))
    (loop [child (some-> list-loc z/down z/right)]
      (cond
        (nil? child) nil
        (= :vector (z/tag child)) (parser/right-siblings child)
        :else (recur (z/right child))))))

(def ^:private mapping-heads #{"map" "map-indexed" "keep" "keep-indexed"})

(defn- mapping-body-locs
  "Return the body forms of a sequence expression, in order, or nil.

  `for` writes its body after the binding vector; `map` and friends hold theirs inside
  the function they are handed. A function named rather than written out — a bare ref
  or a transducer arity — has no body here, and nil is the honest answer: what that
  function attaches cannot be read from this call site."
  [loc]
  (when (= :list (z/tag loc))
    (let [head (z/down loc)
          head-name (parser/sym-name head)
          argument (z/right head)]
      (cond
        (= "for" head-name)
        (when (= :vector (some-> argument z/tag))
          (parser/right-siblings argument))

        (contains? mapping-heads head-name)
        (case (some-> argument z/tag)
          :fn (when-let [body (z/down argument)] [body])
          :list (function-body-locs argument)
          nil)))))

(defn- key-meta-map? [map-loc]
  (and (= :map (some-> map-loc z/tag))
       (contains? (hiccup/literal-map map-loc) :key)))

(defn- attaches-key?
  "True when `loc` gives React a key for the element it produces.

  All three live forms count: reader metadata, an explicit `with-meta`, and a `:key` in
  the props map of an element written out here. The last is why this is asked only of
  the forms a mapping body produces — a `:key` in the props of the container being
  spliced into belongs to the container, not to its children."
  [loc]
  (case (z/tag loc)
    :meta (key-meta-map? (z/down loc))
    :list (and (= "with-meta" (some-> loc z/down parser/sym-name))
               (key-meta-map? (some-> loc z/down z/rightmost)))
    :vector (and (hiccup-head-loc? (z/down loc))
                 (key-meta-map? (some-> loc z/down z/right)))
    false))

(defn- unkeyed-element?
  "True when `loc` is a Hiccup element written out with no key of its own."
  [loc]
  (and (= :vector (z/tag loc))
       (hiccup-head-loc? (z/down loc))
       (not (key-meta-map? (some-> loc z/down z/right)))))

(defn- reactive-deref?
  "True when `loc` derefs anything at all, ratom or not.

  Reagent's documented caveat is a ratom deref inside a lazy seq: the deref then happens outside
  the component's reactive context and the component stops re-rendering. Whether the target is a
  ratom is not readable here, so every deref counts — a body that derefs keeps the eager `into`
  rather than being turned into a lazy `for`."
  [loc]
  (or (= :deref (z/tag loc))
      (and (= :list (z/tag loc))
           (= "deref" (some-> loc z/down parser/sym-name)))))

(defn- keyed-mapping-form?
  "True when `loc` maps a collection to elements that all carry their own `:key`.

  One keyed branch is not enough: a body keying only some of what it produces still
  needs the `into` for the rest. And without any key the `into` is load-bearing — it
  splices the elements in as positional children, which React accepts, where a sequence
  sitting inside the vector would draw a missing-key warning."
  [loc]
  (boolean (when-let [bodies (seq (mapping-body-locs loc))]
             (let [results (hiccup/result-locs (last bodies))]
               (and (some attaches-key? results)
                    (not-any? unkeyed-element? results)
                    (not-any? #(first-in-subtree reactive-deref? %) bodies))))))

(defn- redundant-into?
  "True when `loc` is `(into [HEAD …] MAPPING …)` and the `into` earns nothing.
  Both halves have to hold: a Hiccup-shaped head, and a mapping whose body keys the
  elements it produces."
  [loc]
  (when (= "into" (parser/sym-name (z/down loc)))
    (let [vec-loc (some-> loc z/down z/right)
          more-loc (some-> vec-loc z/right)]
      (and vec-loc
           more-loc
           (= :vector (z/tag vec-loc))
           (hiccup-head-loc? (z/down vec-loc))
           (not (data-vector? vec-loc))
           (keyed-mapping-form? more-loc)))))

(defn- head-identifier
  "Return the head token's raw text as a keyword (for keyword tags) or symbol."
  [head-loc]
  (let [raw (parser/raw head-loc)]
    (if-let [tag (hiccup/parse-tag raw)]
      tag
      (symbol raw))))

(defn- redundant-into-finding [loc file]
  (let [head-loc (some-> loc z/down z/right z/down)
        [row col] (try (z/position loc) (catch Exception _ [0 1]))]
    {:kw (head-identifier head-loc)
     :type :redundant-into-hiccup
     :form (source-snippet loc)
     :file file
     :row row
     :col col}))

(defn- code-locs
  "Return the locs of `loc`'s children in order, a `#_` form left out."
  [loc]
  (->> (z/down loc)
       (iterate z/right)
       (take-while some?)
       (remove #(= :uneval (z/tag %)))))

(defn- empty-fragment-props?
  "True when `loc` is `[:<> {} …]`.

  A fragment takes no props but `:key`, so an empty `{}` configures nothing. All it changes
  is which child Reagent reads as props, and code should not depend on that."
  [loc]
  (let [[head props] (code-locs loc)]
    (and (= ":<>" (some-> head parser/raw))
         (= :map (some-> props z/tag))
         (empty? (code-locs props)))))

(defn- empty-fragment-props-finding [loc file]
  (let [[row col] (try (z/position loc) (catch Exception _ [0 1]))]
    {:kw :<>
     :type :empty-fragment-props
     :form (source-snippet loc)
     :file file
     :row row
     :col col}))

(def ^:private body-heads
  "Forms whose last form is the value a function returns."
  #{"defn" "defn-" "defc" "defmethod" "fn" "fn*"})

(defn- last-code-loc? [loc parent]
  (identical? (z/node loc) (some-> (last (code-locs parent)) z/node)))

(defn- result-of?
  "True when every value `loc` can yield is one `parent` yields, so `loc` stands in a result position of it.

  The values are compared rather than `loc` itself, since a `cond` the last form of a `let`
  holds yields its branches, not the `cond`."
  [loc parent]
  (let [parent-results (map z/node (hiccup/result-locs parent))]
    (every? (fn [result] (some #(identical? (z/node result) %) parent-results))
            (hiccup/result-locs loc))))

(defn- rendered-position?
  "True when `loc` stands where its value is rendered or returned, not handed to a call as data.

  The last form of a function body is returned. A child of an element, a value in its
  props map, the last form of a `for` body, and a value a `let`, `if` or `cond` yields
  render when what holds them does, asked again one level up. An argument of any other
  call, `(into [:<> [header]] rows)` among them, is data the call may still change, and a
  `with-meta` or reader metadata around `loc` gives it a key the element would need."
  [loc]
  (let [parent (z/up loc)
        head-name (some-> parent z/down parser/sym-name)]
    (case (some-> parent z/tag)
      :vector (and (hiccup-head-loc? (z/down parent))
                   (not (data-vector? parent))
                   (rendered-position? parent))
      :map (and (->> (code-locs parent)
                     (take-while #(not (identical? (z/node loc) (z/node %))))
                     count
                     odd?)
                (rendered-position? parent))
      :list (cond
              (contains? body-heads head-name) (last-code-loc? loc parent)
              (= "for" head-name) (and (last-code-loc? loc parent) (rendered-position? parent))
              :else (and (result-of? loc parent) (rendered-position? parent)))
      false)))

(defn- redundant-fragment?
  "True when `loc` is a fragment around a single Hiccup element, which can stand in its place.

  Only an element written out counts: a symbol or a call may yield a map, which Reagent
  would read as the fragment's props, or something that is no element at all. A fragment
  with props carries a key the element would have to take over."
  [loc]
  (let [[head & children] (code-locs loc)
        [child] children]
    (boolean
     (and (= ":<>" (some-> head parser/raw))
          (= 1 (count children))
          (= :vector (z/tag child))
          (hiccup-head-loc? (z/down child))
          (not (data-vector? child))
          (rendered-position? loc)))))

(defn- redundant-fragment-finding [loc file]
  (let [[row col] (try (z/position loc) (catch Exception _ [0 1]))]
    {:kw :<>
     :type :redundant-fragment
     :form (source-snippet loc)
     :file file
     :row row
     :col col}))

(def ^:private inline-component-min 2)

(def ^:private binding-heads
  "Forms whose bindings may hold Hiccup a component renders: `let` and Reagent's `with-let`."
  #{"let" "with-let"})

(defn- literal-value?
  "True when `loc` holds a literal value: a string, a number, a keyword, a boolean or nil, and no name."
  [loc]
  (case (z/tag loc)
    :multi-line true
    :token (not (symbol? (try (z/sexpr loc) (catch Exception _ :auto-resolved))))
    false))

(defn- handler-element?
  "True when the Hiccup element at `loc` takes an event handler.
  That is an unqualified attrs key starting with `:on-` whose value is no literal: `:on-label \"On\"`
  is a label, and a handler is a function or a name for one."
  [loc]
  (boolean (some (fn [[k value]]
                   (and (nil? (namespace k))
                        (str/starts-with? (name k) "on-")
                        (not (literal-value? value))))
                 (:attrs (hiccup/attrs-info loc)))))

(defn- hiccup-element?
  "True when `loc` is a Hiccup element: a vector headed by a tag, a fragment or a component, and no data vector."
  [loc]
  (and (= :vector (some-> loc z/tag))
       (hiccup-head-loc? (z/down loc))
       (not (data-vector? loc))))

(defn- element-children
  "Return the locs after the head of an element vector, its attrs among them, which count no element."
  [loc]
  (->> (z/down loc) z/right (iterate z/right) (take-while some?)))

(defn- count-handler-elements
  "Count the elements of the Hiccup at `loc` that take an event handler, each once.

  Only the Hiccup tree is read: an element and its children, and what a `when`, an `if`, a
  `let` or a `for` or `map` body yields. An attrs map, a handler's body and any other call are
  not, so the event vectors a handler dispatches are no elements. A loop body counts once
  however many items it gives, and a form choosing between branches as its busiest branch,
  since only one of them renders."
  [loc]
  (let [loc (hiccup/unwrap-meta loc)]
    (cond
      (hiccup-element? loc)
      (+ (if (handler-element? loc) 1 0)
         (->> loc element-children (map count-handler-elements) (reduce +)))

      (= :list (z/tag loc))
      (if-let [bodies (seq (remove #(= :uneval (z/tag %)) (mapping-body-locs loc)))]
        (count-handler-elements (last bodies))
        (let [results (hiccup/result-locs loc)]
          (if (= [loc] results)
            0
            (apply max (map count-handler-elements results)))))

      :else 0)))

(defn- inline-component-findings
  "Return a finding for each `let` binding whose Hiccup holds two or more elements with event handlers.
  The value is read as [[count-handler-elements]] reads Hiccup, so a `when` or an `if` around it counts too."
  [loc file]
  (when (contains? binding-heads (some-> loc z/down parser/sym-name))
    (let [bindings (some-> loc z/down z/right)]
      (when bindings
        (->> (z/down bindings)
             (iterate z/right)
             (take-while some?)
             (remove #(= :uneval (z/tag %)))
             (partition 2)
             (keep (fn [[name-loc value-loc]]
                     (let [binding-name (parser/sym-name name-loc)
                           handler-count (when binding-name (count-handler-elements value-loc))]
                       (when (and handler-count (>= handler-count inline-component-min))
                         (let [[row col] (try (z/position value-loc) (catch Exception _ [0 1]))]
                           {:kw (symbol binding-name)
                            :type :inline-component
                            :form (source-snippet value-loc)
                            :file file
                            :row row
                            :col col
                            :hint (str binding-name " holds " handler-count
                                       " elements with event handlers. Make it a component of its own.")})))))
             vec)))))

(def ^:private tail-body-offsets
  "Forms that return the last form of their body, by how many forms come before the body."
  {"binding" 1
   "do" 0
   "let" 1
   "letfn" 1
   "loop" 1
   "when" 1
   "when-first" 1
   "when-let" 1
   "when-not" 1
   "when-some" 1
   "with-let" 1
   "with-redefs" 1})

(def ^:private function-heads-with-body
  "Forms defining a function that returns the last form of its body."
  #{"defc" "defn" "defn-" "fn"})

(defn- arity-bodies
  "Return the body of each arity of a function form, its forms in order.

  A single-arity form writes its body after the first vector from `start`, the parameters;
  a name, a docstring or an attr map before it is no vector. A multi-arity form writes a
  list per arity whose first child is that arity's parameters."
  [start]
  (loop [child start]
    (cond
      (nil? child) []
      (= :vector (z/tag (hiccup/unwrap-meta child))) [(parser/right-siblings child)]
      (= :list (z/tag child)) (->> (cons child (parser/right-siblings child))
                                   (filter #(= :list (z/tag %)))
                                   (map #(parser/right-siblings (z/down %))))
      :else (recur (z/right child)))))

(defn- finally-form? [loc]
  (and (= :list (z/tag loc))
       (= "finally" (some-> loc z/down parser/sym-name))))

(def ^:private qualified-body-heads
  "Body forms written through their library's alias, Reagent's `with-let` and Rum's `defc`."
  #{"defc" "with-let"})

(defn- body-head-name
  "Return the name of the form `loc` opens, unless it is qualified by another library.
  A qualified look-alike such as Schema's `s/defn` writes a return schema before its parameters."
  [loc]
  (let [head (some-> loc z/down)
        head-name (parser/sym-name head)]
    (when (or (= head-name (parser/raw head))
              (contains? qualified-body-heads head-name))
      head-name)))

(defn- returning-bodies
  "Return the bodies `loc` returns the last form of, each as its forms in order, a `#_` form left out.

  Reagent's `with-let` returns the last form before a closing `(finally …)`, which runs on
  unmount. A `defmethod` dispatch value may be a vector, so its parameters are looked for
  after it."
  [loc]
  (let [head-name (body-head-name loc)
        bodies (cond
                 (contains? function-heads-with-body head-name)
                 (arity-bodies (some-> loc z/down z/right))

                 (= "defmethod" head-name)
                 (arity-bodies (some-> loc z/down z/right z/right z/right))

                 (contains? tail-body-offsets head-name)
                 [(cond->> (->> loc code-locs rest (drop (get tail-body-offsets head-name)))
                    (= "with-let" head-name) (remove finally-form?))])]
    (map #(remove (fn [form] (= :uneval (z/tag form))) %) bodies)))

(defn- discarded-hiccup-locs
  "Return the Hiccup elements written in a body before its last form, whose values are dropped."
  [loc]
  (->> (returning-bodies loc)
       (mapcat butlast)
       (filter #(hiccup-element? (hiccup/unwrap-meta %)))))

(defn- discarded-hiccup-finding [loc file]
  (let [[row col] (try (z/position loc) (catch Exception _ [0 1]))]
    {:kw (head-identifier (z/down (hiccup/unwrap-meta loc)))
     :type :discarded-hiccup
     :form (source-snippet loc)
     :file file
     :row row
     :col col}))

(defn- handle-let
  "Emit an :inline-component finding for each `let` binding holding an inline component."
  [loc _ns-info file]
  (when-not (or (hiccup/inside-unrendered-form? loc)
                (hiccup/inside-style-decl? loc))
    (when-let [findings (seq (inline-component-findings loc file))]
      {:decls (vec findings)
       :usages []
       :dynamics []})))

(defn- inside-comment-form?
  "True when a `(comment …)` encloses loc: code kept for the REPL, which no page renders."
  [loc]
  (boolean (some #(and (= :list (z/tag %)) (= "comment" (some-> % z/down parser/sym-name)))
                 (take-while some? (rest (iterate z/up loc))))))

(defn- handle-discarded-hiccup
  "Emit a :discarded-hiccup finding for each Hiccup element a body drops before its last form."
  [loc _ns-info file]
  (when-not (or (hiccup/inside-unrendered-form? loc)
                (hiccup/inside-style-decl? loc)
                (inside-comment-form? loc))
    (when-let [findings (seq (map #(discarded-hiccup-finding % file) (discarded-hiccup-locs loc)))]
      {:decls (vec findings)
       :usages []
       :dynamics []})))

(def ^:private spade-handle-list
  (get (group/parse-handlers spade/group) :handle-list))

(defn- handle-into
  "Emit a :redundant-into-hiccup finding when `loc` matches the anti-pattern."
  [loc _ns-info file]
  (when (and (not (hiccup/inside-unrendered-form? loc))
             (not (hiccup/inside-style-decl? loc))
             (not (hiccup/inside-ns-form? loc))
             (redundant-into? loc))
    {:decls [(redundant-into-finding loc file)]
     :usages []
     :dynamics []}))

(defn- handle-fragment
  "Emit an :empty-fragment-props or :redundant-fragment finding when `loc` is a fragment that does nothing."
  [loc _ns-info file]
  (when (and (not (hiccup/inside-unrendered-form? loc))
             (not (hiccup/inside-style-decl? loc))
             (not (hiccup/inside-ns-form? loc)))
    (when-let [finding (cond
                         (empty-fragment-props? loc) (empty-fragment-props-finding loc file)
                         (redundant-fragment? loc) (redundant-fragment-finding loc file))]
      {:decls [finding]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(= :defclass (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        usages-by-kw (group-by :kw style-calls)
        defclass-as-sole-attr (for [decl style-decls
                                    :let [uses (get usages-by-kw (:kw decl))]
                                    :when (seq uses)
                                    :when (every? #(= :class-only-map (:context %)) uses)]
                                decl)
        redundant-into (filter #(= :redundant-into-hiccup (:type %)) declarations)
        empty-fragment-props (filter #(= :empty-fragment-props (:type %)) declarations)
        redundant-fragment (filter #(= :redundant-fragment (:type %)) declarations)
        inline-component (filter #(= :inline-component (:type %)) declarations)
        discarded-hiccup (filter #(= :discarded-hiccup (:type %)) declarations)]
    {:defclass-as-sole-attr (vec defclass-as-sole-attr)
     :redundant-into-hiccup (vec redundant-into)
     :empty-fragment-props (vec empty-fragment-props)
     :redundant-fragment (vec redundant-fragment)
     :inline-component (vec inline-component)
     :discarded-hiccup (vec discarded-hiccup)}))

(defn- summary-lines* [{:keys [defclass-as-sole-attr
                               redundant-into-hiccup
                               empty-fragment-props
                               redundant-fragment
                               inline-component
                               discarded-hiccup]}]
  [["defclass as sole attr:" (count defclass-as-sole-attr)]
   ["Redundant into-hiccup:" (count redundant-into-hiccup)]
   ["Empty fragment props:" (count empty-fragment-props)]
   ["Redundant fragment:" (count redundant-fragment)]
   ["Inline component:" (count inline-component)]
   ["Discarded hiccup:" (count discarded-hiccup)]])

(defn- failed?* [{:keys [discarded-hiccup]}]
  (boolean (seq discarded-hiccup)))

(defrecord ReagentGroup []
  group/RuleGroup
  (group-id [_] :reagent)
  (group-name [_] "Reagent")
  (parse-handlers [_] {:handle-list [spade-handle-list handle-into handle-let handle-discarded-hiccup]
                       :handle-vector handle-fragment})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:defclass-as-sole-attr
     (str "Every use of this defclass is {:class (style-fn)}. Declare it with defattrs and pass (style-fn) as the props "
          "map, without the :class wrapper.")
     :redundant-into-hiccup
     (str "The mapped body already gives every element a :key, so the into does nothing useful. Put the sequence "
          "inside the Hiccup vector: [:<> (for [x xs] ^{:key x} [item x])].")
     :empty-fragment-props
     "A fragment takes no props but :key, so an empty {} configures nothing. Drop the {}."
     :redundant-fragment
     "A fragment around a single element renders the same as the element. Drop the [:<> ...] around it."
     :inline-component
     (str "A let binds Hiccup holding several elements with event handlers: a component written inline, with no "
          "name, props or docstring of its own, which cannot be rendered or tested alone. Make it a component.")
     :discarded-hiccup
     (str "Only the last form of the body is returned, so this Hiccup is built and dropped. Wrap the elements in "
          "[:<> ...] or a parent element, or remove the one that should not render.")})
  (docs-anchors [_]
    {:defclass-as-sole-attr "defclass-as-sole-attr"
     :redundant-into-hiccup "redundant-into-around-hiccup"
     :empty-fragment-props "empty-fragment-props"
     :redundant-fragment "redundant-fragment"
     :inline-component "inline-component"
     :discarded-hiccup "discarded-hiccup"})
  (rule->tier [_]
    {:defclass-as-sole-attr :deprecations
     :redundant-into-hiccup :cleanup
     :empty-fragment-props :cleanup
     :redundant-fragment :cleanup
     :inline-component :cleanup
     :discarded-hiccup :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReagentGroup))
