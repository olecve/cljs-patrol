(ns cljs-patrol.groups.reagent
  "Reagent rule group: detects suboptimal patterns in Reagent hiccup templates."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.heads :as heads]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z])
  (:import
   (java.util
    Collections
    IdentityHashMap
    Set)))

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

(def ^:private function-heads #{'cljs.core/fn 'cljs.core/fn*})

(defn- function-body-locs
  "Return the body forms of an `(fn [params] …)` literal, or nil for anything else.

  A name may sit between `fn` and the parameters, so the parameter list is whatever
  vector comes first. A multi-arity `fn` states no vector of its own and reads as nil,
  which costs only a finding this rule was never going to be sure of."
  [list-loc ns-info]
  (when (heads/resolve-head list-loc ns-info function-heads)
    (loop [child (some-> list-loc z/down z/right)]
      (cond
        (nil? child) nil
        (= :vector (z/tag child)) (parser/right-siblings child)
        :else (recur (z/right child))))))

(def ^:private mapping-heads #{'cljs.core/keep 'cljs.core/keep-indexed 'cljs.core/map 'cljs.core/map-indexed})

(def ^:private sequence-heads
  "Every form [[mapping-body-locs]] reads a body of."
  (conj mapping-heads 'cljs.core/for))

(defn- mapping-body-locs
  "Return the body forms of a sequence expression, in order, or nil.

  `for` writes its body after the binding vector; `map` and friends hold theirs inside
  the function they are handed. A function named rather than written out — a bare ref
  or a transducer arity — has no body here, and nil is the honest answer: what that
  function attaches cannot be read from this call site."
  [loc ns-info]
  (when (= :list (z/tag loc))
    (let [head (heads/resolve-head loc ns-info sequence-heads)
          argument (some-> loc z/down z/right)]
      (cond
        (= 'cljs.core/for head)
        (when (= :vector (some-> argument z/tag))
          (parser/right-siblings argument))

        (contains? mapping-heads head)
        (case (some-> argument z/tag)
          :fn (when-let [body (z/down argument)] [body])
          :list (function-body-locs argument ns-info)
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
  [loc ns-info]
  (case (z/tag loc)
    :meta (key-meta-map? (z/down loc))
    :list (and (heads/resolve-head loc ns-info #{'cljs.core/with-meta})
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
  sitting inside the vector would draw a missing-key warning.

  The keys are read where the body's heads resolve, the unkeyed elements wherever a head
  of that name might yield one, so a look-alike `x/when` neither keys nor hides one."
  [loc ns-info]
  (boolean (when-let [bodies (seq (mapping-body-locs loc ns-info))]
             (and (some #(attaches-key? % ns-info) (hiccup/result-locs (last bodies) ns-info))
                  (not-any? unkeyed-element? (hiccup/result-locs (last bodies)))
                  (not-any? #(first-in-subtree reactive-deref? %) bodies)))))

(defn- redundant-into?
  "True when `loc` is `(into [HEAD …] MAPPING …)` and the `into` earns nothing.
  Both halves have to hold: a Hiccup-shaped head, and a mapping whose body keys the
  elements it produces."
  [loc ns-info]
  (when (heads/resolve-head loc ns-info #{'cljs.core/into})
    (let [vec-loc (some-> loc z/down z/right)
          more-loc (some-> vec-loc z/right)]
      (and vec-loc
           more-loc
           (= :vector (z/tag vec-loc))
           (hiccup-head-loc? (z/down vec-loc))
           (not (data-vector? vec-loc))
           (keyed-mapping-form? more-loc ns-info)))))

(defn- head-identifier
  "Return the head token's raw text as a keyword (for keyword tags) or symbol."
  [head-loc]
  (let [raw (parser/raw head-loc)]
    (if-let [tag (hiccup/parse-tag raw)]
      tag
      (symbol raw))))

(defn- redundant-into-finding [loc file]
  (let [head-loc (some-> loc z/down z/right z/down)
        [row col] (parser/position loc)]
    {:kw (head-identifier head-loc)
     :type :redundant-into-hiccup
     :form (parser/source-snippet loc)
     :file file
     :row row
     :col col}))

(defn- empty-fragment-props?
  "True when `loc` is `[:<> {} …]`.

  A fragment takes no props but `:key`, so an empty `{}` configures nothing. All it changes
  is which child Reagent reads as props, and code should not depend on that."
  [loc]
  (let [[head props] (parser/written-child-locs loc)]
    (and (= ":<>" (some-> head parser/raw))
         (= :map (some-> props z/tag))
         (empty? (parser/written-child-locs props)))))

(defn- empty-fragment-props-finding [loc file]
  (let [[row col] (parser/position loc)]
    {:kw :<>
     :type :empty-fragment-props
     :form (parser/source-snippet loc)
     :file file
     :row row
     :col col}))

(def ^:private body-heads
  "Forms whose last form is the value a function returns."
  #{'cljs.core/defmethod 'cljs.core/defn 'cljs.core/defn- 'cljs.core/fn 'cljs.core/fn* 'rum.core/defc})

(def ^:private rendering-heads
  "Every form [[rendered-position?]] reads a body of."
  (conj body-heads 'cljs.core/for))

(defn- last-code-loc? [loc parent]
  (identical? (z/node loc) (some-> (last (parser/written-child-locs parent)) z/node)))

(defn- result-of?
  "True when every value `loc` can yield is one `parent` yields, so `loc` stands in a result position of it.

  The values are compared rather than `loc` itself, since a `cond` the last form of a `let`
  holds yields its branches, not the `cond`."
  [loc parent ns-info]
  (let [parent-results (map z/node (hiccup/result-locs parent ns-info))]
    (every? (fn [result] (some #(identical? (z/node result) %) parent-results))
            (hiccup/result-locs loc ns-info))))

(defn- rendered-position?
  "True when `loc` stands where its value is rendered or returned, not handed to a call as data.

  The last form of a function body is returned. A child of an element, a value in its
  props map, the last form of a `for` body, and a value a `let`, `if` or `cond` yields
  render when what holds them does, asked again one level up. An argument of any other
  call, `(into [:<> [header]] rows)` among them, is data the call may still change, and a
  `with-meta` or reader metadata around `loc` gives it a key the element would need."
  [loc ns-info]
  (let [parent (z/up loc)]
    (case (some-> parent z/tag)
      :vector (and (hiccup-head-loc? (z/down parent))
                   (not (data-vector? parent))
                   (rendered-position? parent ns-info))
      :map (and (->> (parser/written-child-locs parent)
                     (take-while #(not (identical? (z/node loc) (z/node %))))
                     count
                     odd?)
                (rendered-position? parent ns-info))
      :list (let [head (heads/resolve-head parent ns-info rendering-heads)]
              (cond
                (contains? body-heads head) (last-code-loc? loc parent)
                (= 'cljs.core/for head) (and (last-code-loc? loc parent) (rendered-position? parent ns-info))
                :else (and (result-of? loc parent ns-info) (rendered-position? parent ns-info))))
      false)))

(defn- redundant-fragment?
  "True when `loc` is a fragment around a single Hiccup element, which can stand in its place.

  Only an element written out counts: a symbol or a call may yield a map, which Reagent
  would read as the fragment's props, or something that is no element at all. A fragment
  with props carries a key the element would have to take over."
  [loc ns-info]
  (let [[head & children] (parser/written-child-locs loc)
        [child] children]
    (boolean
     (and (= ":<>" (some-> head parser/raw))
          (= 1 (count children))
          (= :vector (z/tag child))
          (hiccup-head-loc? (z/down child))
          (not (data-vector? child))
          (rendered-position? loc ns-info)))))

(defn- redundant-fragment-finding [loc file]
  (let [[row col] (parser/position loc)]
    {:kw :<>
     :type :redundant-fragment
     :form (parser/source-snippet loc)
     :file file
     :row row
     :col col}))

(def ^:private inline-component-min 2)

(def ^:private binding-heads
  "Forms whose bindings may hold Hiccup a component renders: `let` and Reagent's `with-let`."
  #{'cljs.core/let 'reagent.core/with-let})

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
  [loc ns-info]
  (let [loc (heads/unwrap-meta loc)]
    (cond
      (hiccup-element? loc)
      (+ (if (handler-element? loc) 1 0)
         (->> loc element-children (map #(count-handler-elements % ns-info)) (reduce +)))

      (= :list (z/tag loc))
      (if-let [bodies (seq (remove #(= :uneval (z/tag %)) (mapping-body-locs loc ns-info)))]
        (count-handler-elements (last bodies) ns-info)
        (let [results (hiccup/result-locs loc ns-info)]
          (if (= [loc] results)
            0
            (apply max (map #(count-handler-elements % ns-info) results)))))

      :else 0)))

(defn- inline-component-findings
  "Return a finding for each `let` binding whose Hiccup holds two or more elements with event handlers.
  The value is read as [[count-handler-elements]] reads Hiccup, so a `when` or an `if` around it counts too."
  [loc ns-info file]
  (when (heads/resolve-head loc ns-info binding-heads)
    (let [bindings (some-> loc z/down z/right)]
      (when bindings
        (->> (parser/written-child-locs bindings)
             (partition 2)
             (keep (fn [[name-loc value-loc]]
                     (let [binding-name (parser/sym-name name-loc)
                           handler-count (when binding-name (count-handler-elements value-loc ns-info))]
                       (when (and handler-count (>= handler-count inline-component-min))
                         (let [[row col] (parser/position value-loc)]
                           {:kw (symbol binding-name)
                            :type :inline-component
                            :form (parser/source-snippet value-loc)
                            :file file
                            :row row
                            :col col
                            :hint (str binding-name " holds " handler-count
                                       " elements with event handlers. Make it a component of its own.")})))))
             vec)))))

(def ^:private body-head->offset
  "Forms that return the last form of their body, to how many forms come before the body."
  {'cljs.core/binding 1
   'cljs.core/do 0
   'cljs.core/let 1
   'cljs.core/letfn 1
   'cljs.core/loop 1
   'cljs.core/when 1
   'cljs.core/when-first 1
   'cljs.core/when-let 1
   'cljs.core/when-not 1
   'cljs.core/when-some 1
   'cljs.core/with-redefs 1
   'reagent.core/with-let 1})

(def ^:private function-heads-with-body
  "Forms defining a function that returns the last form of its body."
  #{'cljs.core/defn 'cljs.core/defn- 'cljs.core/fn 'rum.core/defc})

(defn- finally-form? [loc]
  (and (= :list (z/tag loc))
       (= "finally" (some-> loc z/down parser/sym-name))))

(def ^:private returning-heads
  "Every form [[returning-bodies]] reads a body of."
  (into (conj function-heads-with-body 'cljs.core/defmethod) (keys body-head->offset)))

(defn- returning-bodies
  "Return the bodies `loc` returns the last form of, each as its forms in order, a `#_` form left out.

  Reagent's `with-let` returns the last form before a closing `(finally …)`, which runs on
  unmount. A `defmethod` dispatch value may be a vector, so its parameters are looked for
  after it."
  [loc ns-info]
  (let [head (heads/resolve-head loc ns-info returning-heads)
        bodies (cond
                 (contains? function-heads-with-body head)
                 (map :body (heads/arities (some-> loc z/down z/right)))

                 (= 'cljs.core/defmethod head)
                 (map :body (heads/arities (some-> loc z/down z/right z/right z/right)))

                 (contains? body-head->offset head)
                 [(cond->> (->> loc parser/written-child-locs rest (drop (body-head->offset head)))
                    (= 'reagent.core/with-let head) (remove finally-form?))])]
    (map #(remove (fn [form] (= :uneval (z/tag form))) %) bodies)))

(defn- discarded-hiccup-locs
  "Return the Hiccup elements written in a body before its last form, whose values are dropped."
  [loc ns-info]
  (->> (returning-bodies loc ns-info)
       (mapcat butlast)
       (filter #(hiccup-element? (heads/unwrap-meta %)))))

(defn- discarded-hiccup-finding [loc file]
  (let [[row col] (parser/position loc)]
    {:kw (head-identifier (z/down (heads/unwrap-meta loc)))
     :type :discarded-hiccup
     :form (parser/source-snippet loc)
     :file file
     :row row
     :col col}))

(defn- handle-let
  "Emit an :inline-component finding for each `let` binding holding an inline component."
  [loc ns-info file]
  (when-not (hiccup/inside-non-hiccup-form? loc)
    (when-let [findings (seq (inline-component-findings loc ns-info file))]
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
  [loc ns-info file]
  (when-not (or (hiccup/inside-non-hiccup-form? loc)
                (inside-comment-form? loc))
    (when-let [findings (seq (map #(discarded-hiccup-finding % file) (discarded-hiccup-locs loc ns-info)))]
      {:decls (vec findings)
       :usages []
       :dynamics []})))

(def ^:private branching-heads
  "Forms whose last two forms are the two branches, one of which renders."
  #{'cljs.core/if 'cljs.core/if-let 'cljs.core/if-not 'cljs.core/if-some})

(defn- element-parts
  "Return {:head … :children […]} for a Hiccup element vector, the fragment among them, or nil.
  The props slot, readable or not, is no child."
  [loc]
  (when (hiccup-element? loc)
    (let [[head & more] (parser/written-child-locs loc)
          props (some-> (hiccup/props-slot loc) z/node)]
      {:head head
       :children (remove #(identical? props (z/node %)) more)})))

(defn- fragment? [{:keys [head]}]
  (= ":<>" (parser/raw head)))

(defn- form-text [loc]
  (parser/normalize-form (parser/raw loc)))

(defn- focusable-element?
  "True when the element at `loc` takes keyboard focus, as the a11y group reads it, through its tag or a `:tab-index`.

  A widget role alone puts no element in the tab order, and props this cannot read may hold
  the `disabled` or the negative tabindex that keeps it out. Component aliases are not read."
  [loc ns-info]
  (let [tag (some-> loc z/down parser/raw hiccup/parse-tag)
        attrs (:attrs (hiccup/attrs-info loc))]
    (boolean (and tag
                  (a11y/focusable-loc? loc ns-info nil)
                  (or (contains? a11y/focusable-tags tag)
                      (= :a tag)
                      (contains? attrs :tab-index)
                      (contains? attrs :tabIndex))))))

(defn- holds-focus?
  "True when the Hiccup at `loc` holds a focusable element, or a component given an event handler.

  Only the Hiccup tree is read: an element's children, and what a `when`, an `if`, a `let`
  or a `for` or `map` body yields. Props and the arguments of any other call are data."
  [loc ns-info]
  (let [loc (heads/unwrap-meta loc)]
    (if-let [parts (element-parts loc)]
      (or (and (not (fragment? parts))
               (if (parser/sym-name (:head parts))
                 (handler-element? loc)
                 (focusable-element? loc ns-info)))
          (boolean (some #(holds-focus? % ns-info) (:children parts))))
      (and (= :list (z/tag loc))
           (boolean (if-let [bodies (seq (remove #(= :uneval (z/tag %)) (mapping-body-locs loc ns-info)))]
                      (holds-focus? (last bodies) ns-info)
                      (let [results (hiccup/result-locs loc ns-info)]
                        (and (not= [loc] results) (some #(holds-focus? % ns-info) results)))))))))

(defn- bound-node
  "Return the node a binding around the symbol at `loc` gives it, or nil when none does or it cannot be read."
  [loc]
  (let [value (some->> (parser/sym-name loc) (hiccup/lexically-bound-value loc))]
    (when (and value (not= ::hiccup/shadowed value))
      (z/node value))))

(defn- focus-holding-child?
  "True when the child at `loc` can hold keyboard focus that mounting it again drops.

  An element counts when its Hiccup holds a focusable element or a component given an event
  handler, and so does a `when`, `if` or `let` that yields one. A symbol counts when a binding
  around it gives it such Hiccup; a parameter, a name bound to something unreadable, or any
  other call may be text or data."
  [loc ns-info]
  (case (z/tag loc)
    (:vector :list) (holds-focus? loc ns-info)
    :token (boolean (when (bound-node loc)
                      (holds-focus? (hiccup/lexically-bound-value loc (parser/sym-name loc)) ns-info)))
    false))

(defn- other-children?
  "True when the wrapper holds children besides `child`.

  When the props slot could not be read, the vector's second form may be props, so it is not
  counted as a child."
  [wrapped children child]
  (let [maybe-props (some-> (second (parser/written-child-locs wrapped)) z/node)]
    (boolean (some #(and (not (identical? (z/node child) (z/node %)))
                         (not (and (identical? maybe-props (z/node %))
                                   (or (= :list (z/tag %)) (some? (parser/sym-name %))))))
                   children))))

(defn- same-form?
  "True when two forms are written the same, a symbol also naming the same binding in both places.
  The head tokens are compared before the text, since most candidates differ there."
  [a b]
  (and (= (z/tag a) (z/tag b))
       (= (some-> a z/down parser/raw) (some-> b z/down parser/raw))
       (= (form-text a) (form-text b))
       (or (not= :token (z/tag a)) (identical? (bound-node a) (bound-node b)))))

(defn- wrapped-child
  "Return {:hint … :child …} for a child an element in one branch wraps that the other branch renders bare, or nil."
  [wrapped {:keys [head children]
            :as parts} bare ns-info]
  (when-not (fragment? parts)
    (when-let [match (first (filter #(same-form? % bare) children))]
      (when (focus-holding-child? bare ns-info)
        {:hint (str (form-text bare) " renders inside " (parser/raw head) " in one branch and bare in the other, so"
                    " React mounts it again when the condition flips, and keyboard focus inside it falls to the page. "
                    (if (other-children? wrapped children match)
                      "Render the same elements around it, in the same order, in both branches."
                      (str "Render " (parser/raw head) " in both branches.")))
         :child bare}))))

(defn- inside-handler-fn?
  "True when an anonymous `fn` around `loc` is a handler or a call's argument, whose result no page renders.

  A `fn` in a map is a handler, unless it is the `:reagent-render` or `:render` of a class
  component, and one handed to a call other than `map` or `keep` is that call's to use."
  [loc ns-info]
  (boolean (some (fn [ancestor]
                   (when (and (= :list (z/tag ancestor)) (heads/resolve-head ancestor ns-info function-heads))
                     (let [parent (z/up ancestor)]
                       (case (some-> parent z/tag)
                         :map (not (contains? #{":reagent-render" ":render"} (some-> ancestor z/left parser/raw)))
                         :list (not (heads/resolve-head parent ns-info mapping-heads))
                         false))))
                 (take-while some? (rest (iterate z/up loc))))))

(defn- remounting-branch-finding
  "Return the finding for an `if` whose branches render the same focusable child, wrapped in one and bare in the other.

  The cheap shape check runs first, since most conditions choose between values, not elements."
  [loc ns-info file]
  (let [branches (drop 2 (parser/written-child-locs loc))]
    (when (= 2 (count branches))
      (let [[a b] branches
            parts-a (element-parts a)
            parts-b (element-parts b)]
        (when (or parts-a parts-b)
          (when-let [{:keys [hint child]} (or (when parts-a (wrapped-child a parts-a b ns-info))
                                              (when parts-b (wrapped-child b parts-b a ns-info)))]
            (when (and (rendered-position? loc ns-info) (not (inside-handler-fn? loc ns-info)))
              (let [[row col] (parser/position loc)]
                {:kw (symbol (parser/raw (z/down loc)))
                 :type :remounting-branch
                 :form (parser/source-snippet loc)
                 :inner-form (parser/source-snippet child)
                 :file file
                 :row row
                 :col col
                 :hint hint}))))))))

(defn- handle-remounting-branch [loc ns-info file]
  (when (and (heads/resolve-head loc ns-info branching-heads)
             (not (hiccup/inside-non-hiccup-form? loc))
             (not (inside-comment-form? loc)))
    (when-let [finding (remounting-branch-finding loc ns-info file)]
      {:decls [finding]
       :usages []
       :dynamics []})))

(def ^:private spade-handle-list
  (get (group/parse-handlers spade/group) :handle-list))

(defn- handle-into
  "Emit a :redundant-into-hiccup finding when `loc` matches the anti-pattern."
  [loc ns-info file]
  (when (and (not (hiccup/inside-non-hiccup-form? loc))
             (redundant-into? loc ns-info))
    {:decls [(redundant-into-finding loc file)]
     :usages []
     :dynamics []}))

(def ^:private text-input-types #{"text" "search" "email" "url" "tel" "password"})

(defn- local-input-value? [loc ns-info]
  (let [value (case (some-> loc z/tag)
                :deref (z/down loc)
                :list (when (heads/resolve-head loc ns-info #{'cljs.core/deref})
                        (second (parser/written-child-locs loc)))
                nil)
        init (when-let [local (some-> value hiccup/unqualified-symbol-name)]
               (hiccup/lexically-bound-value value local ns-info))]
    (and (not= ::hiccup/shadowed init)
         (= :list (some-> init z/tag))
         (heads/resolve-head init ns-info #{'reagent.core/atom}))))

(declare calls-dispatch? dispatch-in-call?)

(defn- handler-dispatch? [loc ns-info ^Set seen]
  (when (and loc (.add seen (z/node loc)))
    (case (z/tag loc)
      :fn (dispatch-in-call? loc ns-info seen)
      :list (when (heads/resolve-head loc ns-info function-heads)
              (some #(some (fn [body] (calls-dispatch? body ns-info seen)) (:body %))
                    (heads/arities (z/right (z/down loc)))))
      :token (if-let [value (hiccup/resolved-value loc)]
               (handler-dispatch? value ns-info seen)
               (when-let [{:keys [def-loc arities]} (hiccup/local-function loc)]
                 (when (.add seen (z/node def-loc))
                   (some #(some (fn [body] (calls-dispatch? body ns-info seen)) (:body %))
                         arities))))
      nil)))

(defn- dispatch-in-call? [loc ns-info seen]
  (when-let [head (z/down loc)]
    (let [bare (hiccup/unqualified-symbol-name head)]
      (when-not (or (heads/resolve-head loc ns-info function-heads)
                    (contains? #{"comment" "quote" "var"} (parser/raw head)))
        (or (and (not (and bare (heads/locally-bound? head bare)))
                 (heads/resolve-head loc ns-info #{'re-frame.core/dispatch}))
            (handler-dispatch? head ns-info seen)
            (some #(calls-dispatch? % ns-info seen) (rest (parser/written-child-locs loc))))))))

(defn- calls-dispatch? [loc ns-info seen]
  (when-not (or (hiccup/unrendered-form? loc)
                (= :fn (z/tag loc)))
    (if (= :list (z/tag loc))
      (dispatch-in-call? loc ns-info seen)
      (some #(calls-dispatch? % ns-info seen) (parser/written-child-locs loc)))))

(defn- controlled-input-dispatch-finding [loc ns-info file]
  (let [tag (some-> loc z/down parser/raw hiccup/parse-tag)]
    (when (and (contains? #{:input :textarea} tag) (not (inside-comment-form? loc)))
      (let [{:keys [kind attrs]} (hiccup/attrs-info loc)
            type-loc (:type attrs)
            input-type (a11y/literal-sexpr type-loc)]
        (when (and (= :map kind)
                   (or (= :textarea tag)
                       (not (contains? attrs :type))
                       (and (or (string? input-type) (keyword? input-type))
                            (contains? text-input-types (str/lower-case (name input-type)))))
                   (contains? attrs :value)
                   (not (local-input-value? (:value attrs) ns-info))
                   (handler-dispatch? (:on-change attrs) ns-info (Collections/newSetFromMap (IdentityHashMap.))))
          (let [[row col] (parser/position loc)]
            {:kw tag
             :type :controlled-input-dispatch
             :form (parser/source-snippet loc)
             :file file
             :row row
             :col col}))))))

(defn- handle-vector
  [loc ns-info file]
  (when-not (hiccup/inside-non-hiccup-form? loc)
    (when-let [finding (cond
                         (empty-fragment-props? loc) (empty-fragment-props-finding loc file)
                         (redundant-fragment? loc ns-info) (redundant-fragment-finding loc file)
                         :else (controlled-input-dispatch-finding loc ns-info file))]
      {:decls [(hiccup/add-note finding (hiccup/mixed-schema-note loc))]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(= :defclass (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        usages-by-kw (group-by :kw style-calls)
        defclass-as-sole-attr (for [decl style-decls
                                    :let [uses (get usages-by-kw (:kw decl))]
                                    :when (some #(= :class-only-map (:context %)) uses)
                                    :when (every? #(contains? #{:class-only-map :discarded} (:context %)) uses)]
                                decl)
        redundant-into (filter #(= :redundant-into-hiccup (:type %)) declarations)
        empty-fragment-props (filter #(= :empty-fragment-props (:type %)) declarations)
        redundant-fragment (filter #(= :redundant-fragment (:type %)) declarations)
        inline-component (filter #(= :inline-component (:type %)) declarations)
        discarded-hiccup (filter #(= :discarded-hiccup (:type %)) declarations)
        remounting-branch (filter #(= :remounting-branch (:type %)) declarations)
        controlled-input-dispatch (filter #(= :controlled-input-dispatch (:type %)) declarations)]
    {:defclass-as-sole-attr (vec defclass-as-sole-attr)
     :redundant-into-hiccup (vec redundant-into)
     :empty-fragment-props (vec empty-fragment-props)
     :redundant-fragment (vec redundant-fragment)
     :inline-component (vec inline-component)
     :discarded-hiccup (vec discarded-hiccup)
     :remounting-branch (vec remounting-branch)
     :controlled-input-dispatch (vec controlled-input-dispatch)}))

(defn- summary-lines* [{:keys [defclass-as-sole-attr
                               redundant-into-hiccup
                               empty-fragment-props
                               redundant-fragment
                               inline-component
                               discarded-hiccup
                               remounting-branch
                               controlled-input-dispatch]}]
  [["defclass as sole attr:" (count defclass-as-sole-attr)]
   ["Redundant into-hiccup:" (count redundant-into-hiccup)]
   ["Empty fragment props:" (count empty-fragment-props)]
   ["Redundant fragment:" (count redundant-fragment)]
   ["Inline component:" (count inline-component)]
   ["Discarded hiccup:" (count discarded-hiccup)]
   ["Remounting branch:" (count remounting-branch)]
   ["Controlled input dispatch:" (count controlled-input-dispatch)]])

(defn- failed?* [{:keys [discarded-hiccup]}]
  (boolean (seq discarded-hiccup)))

(defrecord ReagentGroup []
  group/RuleGroup
  (group-id [_] :reagent)
  (group-name [_] "Reagent")
  (parse-handlers [_] {:handle-list [spade-handle-list handle-into handle-let handle-discarded-hiccup handle-remounting-branch]
                       :handle-vector handle-vector})
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
          "[:<> ...] or a parent element, or remove the one that should not render.")
     :remounting-branch
     (str "The branches of a condition render the same child under different elements, so React mounts it again when "
          "the condition flips, and keyboard focus inside it falls to the page. Keep the same elements around it.")
     :controlled-input-dispatch
     (str "This controlled text input dispatches a re-frame event on change. The queued update can reset its value "
          "during CJK IME composition and lose characters. Keep the text in local state and commit on blur, "
          "or use an uncontrolled input with :default-value.")})
  (docs-anchors [_]
    {:defclass-as-sole-attr "defclass-as-sole-attr"
     :redundant-into-hiccup "redundant-into-around-hiccup"
     :empty-fragment-props "empty-fragment-props"
     :redundant-fragment "redundant-fragment"
     :inline-component "inline-component"
     :discarded-hiccup "discarded-hiccup"
     :remounting-branch "remounting-branch"
     :controlled-input-dispatch "controlled-input-dispatch"})
  (rule->tier [_]
    {:defclass-as-sole-attr :deprecations
     :redundant-into-hiccup :cleanup
     :empty-fragment-props :cleanup
     :redundant-fragment :cleanup
     :inline-component :cleanup
     :discarded-hiccup :bugs
     :remounting-branch :bugs
     :controlled-input-dispatch :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReagentGroup))
