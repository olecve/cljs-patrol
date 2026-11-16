(ns cljs-patrol.hiccup
  "Hiccup-shape helpers for rule groups analyzing literal Hiccup vectors.

  Every helper here is conservative — it returns nil / a sentinel when the
  form isn't a plain literal (dynamic tag, computed keys, spliced attrs,
  quoted vectors, etc.). Callers can then decide whether to silently skip
  or flag; this ns never makes that policy call."
  (:require
   [cljs-patrol.heads :as heads]
   [cljs-patrol.parser :as parser]
   [clojure.set :as set]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   (java.util
    IdentityHashMap
    Map)))

(def ^:private dynamic-attr-tags
  "Zipper tags that indicate the attrs slot is a non-literal form.
  Covers function calls, quoted / spliced forms, metadata-wrapped values, reader macros, etc."
  #{:list :fn :syntax-quote :unquote :unquote-splicing :reader-macro :meta})

(def ^:private unrendered-parent-tags
  "Parent tags whose children never reach the DOM.

  A quoting form turns its child vector into a data literal — Hiccup written as test
  data or in a macro body — and `#_` discards its child outright. Neither renders, so
  no rule should report what it finds inside one."
  #{:quote :syntax-quote :unquote :unquote-splicing :uneval})

(def ^:private style-decl-forms
  "Spade / garden macros whose body contains CSS declarations, not Hiccup.
  A vector like `[:button {:font-family …}]` inside these forms is a CSS
  selector + property map, not a Hiccup element."
  #{"defclass" "defattrs" "defglobal" "defkeyframes"})

(def html-tags
  "Element names a Hiccup tag keyword can name, plus the `:<>` fragment.

  Written out rather than inferred, because a keyword heading a vector is Hiccup only
  some of the time: `[:enum …]` is a Malli schema, `[:cart :items]` a re-frame db path,
  `[:checkout :submit]` an event vector. Only a name HTML actually has says element.

  `:map` is left out on purpose. The HTML `<map>` element is vanishingly rare in
  application code, while `[:map {:closed true} …]` is the everyday shape of a Malli
  schema, so reading one as Hiccup costs more than missing the other. `:search`, and the SVG
  `:filter`, `:image` and `:desc`, are left out for the same reason: each is an everyday db
  path or event id, as in `[:search :query]` or `[:filter :status]`."
  #{:<>
    ;; document and sections
    :html :head :body :header :footer :main :nav :aside :article :section :address :hgroup
    :h1 :h2 :h3 :h4 :h5 :h6 :div :span :p :hr :br :wbr :pre :blockquote
    ;; text
    :a :em :strong :b :i :u :s :small :mark :sub :sup :q :cite :abbr :dfn :time :data
    :code :kbd :samp :var :ruby :rt :rp :bdi :bdo :ins :del
    ;; lists
    :ul :ol :li :dl :dt :dd :menu
    ;; tables
    :table :thead :tbody :tfoot :tr :td :th :caption :colgroup :col
    ;; forms
    :form :input :textarea :select :option :optgroup :button :label :fieldset :legend
    :datalist :output :progress :meter
    ;; embedded content
    :img :picture :source :video :audio :track :canvas :svg :iframe :embed :object :param
    :figure :figcaption :area
    ;; interactive and metadata
    :details :summary :dialog :template :slot :noscript :script :style :link :title :base
    ;; svg
    :g :path :circle :ellipse :rect :line :polyline :polygon :defs :use :text :tspan
    :mask :pattern :clipPath :linearGradient :radialGradient :stop :marker :symbol :foreignObject
    :textPath :animate :animateMotion :animateTransform :feBlend :feColorMatrix :feComposite
    :feDropShadow :feFlood :feGaussianBlur :feMerge :feMergeNode :feOffset})

(defn html-tag?
  "True when `tag` names an HTML/SVG element or the fragment."
  [tag]
  (contains? html-tags tag))

(defn parse-tag
  "Return the base HTML tag keyword from a Hiccup tag string.

  Handles plain (`:img`), class (`:img.hero`), id (`:img#logo`), and mixed
  (`:img.a.b#c`) forms. Bare `:.class` / `:#id` shorthand — where the tag is
  omitted — is treated as `:div`, matching Hiccup's runtime convention.
  Returns nil for non-keyword tokens, namespaced keywords, or `::` aliases."
  [raw-str]
  (when (and raw-str
             (str/starts-with? raw-str ":")
             (not (str/starts-with? raw-str "::"))
             (not (str/includes? raw-str "/")))
    (let [body (subs raw-str 1)
          dot (str/index-of body ".")
          hash (str/index-of body "#")
          end (cond
                (and dot hash) (min dot hash)
                dot dot
                hash hash)
          tag-name (if end (subs body 0 end) body)]
      (cond
        (seq tag-name) (keyword tag-name)
        (or dot hash) :div))))

(defn literal-map
  "Return `{kw → value-zloc}` for a literal map zloc.

  Returns nil if any key is a non-keyword (e.g. computed keys), letting
  callers distinguish 'no such key' from 'unclassifiable map'."
  [map-loc]
  (loop [key-loc (z/down map-loc)
         acc {}]
    (cond
      (nil? key-loc) acc

      (not (parser/kw-node? key-loc)) nil

      :else
      (let [value-loc (z/right key-loc)]
        (recur (some-> value-loc z/right)
               (assoc acc (z/sexpr key-loc) value-loc))))))

(def ^:private map-binding-heads
  "Binding forms whose `[name init]` pairs we read a literal map out of.
  All of them bind left to right and shadow what encloses them, so the nearest
  binding of a symbol is the one that reaches the usage."
  #{"let" "let*" "when-let" "if-let" "when-some" "if-some"})

(def ^:private then-branch-binding-heads
  "Binding forms whose binding reaches only one of their branches.
  In the else branch the symbol is whatever an enclosing scope says it is, so the
  search has to carry on past the form rather than answer from its binding."
  #{"if-let" "if-some"})

(def ^:private opaque-binding-heads
  "Binding forms whose bound values we never read, but which still shadow.
  Their binding vectors hold shapes a pairwise scan would misread — `for` clauses,
  `letfn` fn specs, a `loop` name `recur` rebinds, the first element of a collection
  `when-first` takes — so any mention of the symbol inside one ends the search
  instead of letting an enclosing `let` answer for it."
  #{"loop" "go-loop" "for" "doseq" "dotimes" "when-first" "letfn" "with-let"
    "binding" "with-open" "with-redefs" "with-local-vars"})

(def ^:private parameter-binding-heads
  "Forms whose parameter vectors bind symbols for the body that follows.
  A `deftype` / `defrecord` field vector binds for every method of the type, so it
  reaches a Hiccup vector the same way a parameter list does."
  #{"fn" "fn*" "defn" "defn-" "defmethod" "defmacro" "deftype" "defrecord"})

(def ^:private method-holder-heads
  "Forms whose bodies are `(name [params] …)` method implementations.
  A method's parameter vector binds for its body exactly as a `fn`'s does, and
  `this` plus the arguments routinely carry the same names as a surrounding `let`."
  #{"reify" "specify" "specify!" "deftype" "defrecord" "definterface"
    "extend-type" "extend-protocol" "proxy"})

(def ^:private symbol-binding-heads
  "Forms binding one symbol written a fixed number of children after the head.
  `(catch js/Error error …)`, `(as-> expr name …)` and `(this-as me …)` each bind a
  name no binding vector holds, and each shadows the body that follows."
  {"catch" 1
   "as->" 1
   "this-as" 0})

(def ^:private parameter-list-offsets
  "Children to skip after the head before a parameter vector can appear.
  Only `defmethod` can put a vector — its dispatch value — before the parameters."
  {"defmethod" 2})

(defn- unqualified-symbol-name
  "Return the name of a bare symbol token, or nil for anything else.
  A qualified symbol names something another namespace binds, which no local
  binding form can shadow, so `ui/props` must not answer to a local `props`."
  [loc]
  (let [symbol-name (parser/sym-name loc)]
    (when (and symbol-name (= symbol-name (parser/raw loc)))
      symbol-name)))

(defn- mentions-symbol?
  "True when symbol-name appears anywhere in the form loc holds.
  symbol-name is known to be a bare symbol, and every other token prints with
  something a symbol cannot start with — a colon, a quote, a digit — so comparing
  raw source is the same test as reading the token, without building an sexpr for
  every node of every binding vector we scan."
  [loc symbol-name]
  (loop [current (z/subzip loc)]
    (cond
      (z/end? current) false
      (and (= :token (z/tag current)) (= symbol-name (parser/raw current))) true
      :else (recur (z/next current)))))

(defn- same-node? [a b]
  (boolean (and a b (identical? (z/node a) (z/node b)))))

(defn- skip-right [loc n]
  (reduce (fn [current _] (some-> current z/right)) loc (range n)))

(defn unwrap-meta
  "Return the value a `:meta` node wraps, or loc itself.
  `(defn f ^:static [a b] …)` reads as a parameter list with something in front of it,
  and the parameters bind whether or not anything is attached to them."
  [loc]
  (loop [current loc]
    (if (= :meta (some-> current z/tag))
      (recur (some-> current z/down z/rightmost))
      current)))

(defn- parameter-vectors
  "Return the vectors binding parameters in a fn-like form.

  The first vector after the head is the parameter list of a single-arity form; in a
  multi-arity one each arity is a list whose first child is that vector. A body vector
  is never mistaken for either, since a parameter list always precedes the body."
  [list-loc]
  (let [head (z/down list-loc)
        start (skip-right (some-> head z/right)
                          (get parameter-list-offsets (some-> head parser/sym-name) 0))]
    (loop [child start
           arity-vectors []]
      (cond
        (nil? child) arity-vectors

        ;; A single-arity form states its parameters directly, and then everything
        ;; after them is body — including a body that begins with a vector.
        (= :vector (some-> child unwrap-meta z/tag)) [(unwrap-meta child)]

        (= :list (z/tag child))
        (let [first-child (some-> child z/down unwrap-meta)]
          (recur (z/right child)
                 (cond-> arity-vectors (= :vector (some-> first-child z/tag)) (conj first-child))))

        :else (recur (z/right child) arity-vectors)))))

(defn- binding-pairs [vector-loc]
  (loop [form (z/down vector-loc)
         pairs []]
    (if (nil? form)
      pairs
      (let [init (z/right form)]
        (recur (some-> init z/right) (conj pairs [form init]))))))

(defn- bound-init
  "Return the init a let-style binding vector binds symbol-name to.

  `::shadowed` when a destructuring form binds it instead, nil when it is not bound
  here. Pairs from `cut` — the binding the usage itself sits in — on are not in scope
  yet, and the last binding left of it wins."
  [bindings-loc symbol-name cut]
  (reduce (fn [found [form init]]
            (cond
              (= symbol-name (unqualified-symbol-name form)) init
              (mentions-symbol? form symbol-name) ::shadowed
              :else found))
          nil
          (cond->> (binding-pairs bindings-loc)
            cut (take-while (fn [[form init]] (not (or (same-node? form cut) (same-node? init cut))))))))

(defn- fn-like-head?
  "True for a head that plausibly defines a function, and so binds a parameter list.

  Only the name can say so: `[props on-select]` and the Reagent `[photo-props alt]`
  in `(if wide? [wide-photo props] [:img props])` are the same vector of symbols, so
  reading contents alone would take an `if` arm for a parameter list and abandon a
  name it could have resolved. A macro naming itself neither `def…` nor `…fn…` —
  `(component [props] …)` — is left unread rather than read wrongly: a var then
  answers for a parameter only when one of that exact name is defined in the file."
  [head-name]
  (and head-name
       (or (str/starts-with? head-name "def")
           (str/includes? head-name "fn"))))

(defn- parameter-list?
  "True when a vector reads as a parameter list rather than as data.

  Parameters are binding forms — a symbol, a destructuring map or vector, `&` — and
  never a keyword, a string or a number, so the Hiccup value of `(def thumbnail
  [:img props])` is not one. Metadata rides along on a parameter (`^js props`) and a
  discarded one is not there at all, so neither disqualifies the list."
  [vector-loc]
  (loop [element (z/down vector-loc)]
    (let [value (some-> element unwrap-meta)]
      (cond
        (nil? element) true
        (= :uneval (z/tag element)) (recur (z/right element))
        (contains? #{:map :vector} (z/tag value)) (recur (z/right element))
        (some? (unqualified-symbol-name value)) (recur (z/right element))
        :else false))))

(defn- unknown-form-parameters
  "Return the parameter lists a fn-like form binds for the body holding `child`.

  A form that defines a function is the dangerous case: reading it as binding nothing
  lets a var of the same name answer for what is really a parameter. Its vectors count
  except the one the usage itself sits in, which is the form's body, and except those
  holding something no parameter list holds, which are data it was handed."
  [list-loc child]
  (->> (parameter-vectors list-loc)
       (remove #(same-node? % child))
       (filter parameter-list?)))

(defn- method-form?
  "True when list-loc is a `(name [params] …)` method inside a form that holds methods.
  Its head is the method name rather than anything we can recognize on its own, so
  the enclosing form is what identifies it."
  [list-loc]
  (let [holder (z/up list-loc)]
    (and (some? (some-> list-loc z/down parser/sym-name))
         (= :list (some-> holder z/tag))
         (contains? method-holder-heads (some-> holder z/down parser/sym-name)))))

(defn- in-scope-branch?
  "True when child is a branch the binding of a binding form reaches.
  Only `if-let` and `if-some` leave one out: their else branch runs with the symbol
  unbound, so a binding there is not the one the usage sees."
  [head-name child bindings]
  (or (not (contains? then-branch-binding-heads head-name))
      (same-node? child bindings)
      (same-node? child (z/right bindings))))

(defn- binder-lookup
  "Look up symbol-name in the bindings one enclosing form makes.
  `child` is that form's own child the usage descends from and `inner` the one below
  it: together they place a usage written inside the binding vector itself, where the
  bindings to its right are not in scope yet."
  [list-loc child inner symbol-name]
  (let [head (z/down list-loc)
        head-name (some-> head parser/sym-name)
        bindings (some-> head z/right)
        binding-vector? (= :vector (some-> bindings z/tag))]
    (cond
      (and binding-vector? (contains? map-binding-heads head-name))
      (when (in-scope-branch? head-name child bindings)
        (bound-init bindings symbol-name (when (same-node? child bindings) inner)))

      (and binding-vector? (contains? opaque-binding-heads head-name))
      (when (mentions-symbol? bindings symbol-name) ::shadowed)

      (contains? symbol-binding-heads head-name)
      (let [bound (skip-right bindings (get symbol-binding-heads head-name))]
        (when (= symbol-name (unqualified-symbol-name bound)) ::shadowed))

      (or (contains? parameter-binding-heads head-name) (method-form? list-loc))
      (when (some #(mentions-symbol? % symbol-name) (parameter-vectors list-loc)) ::shadowed)

      (fn-like-head? head-name)
      (when (some #(mentions-symbol? % symbol-name) (unknown-form-parameters list-loc child))
        ::shadowed))))

(defn lexically-bound-value
  "Return the zloc an enclosing binding form binds symbol-name to.

  `::shadowed` when a form binds the name to something we cannot read, nil when no
  enclosing form binds it at all — callers need the difference, since only the second
  leaves a var of the same name free to answer. Innermost first. A fn is followed
  through, since a closure really does see the binding around it; what stops the
  search is a form that binds the name itself."
  [loc symbol-name]
  (loop [inner nil
         child loc
         parent (z/up loc)]
    (when (some? parent)
      (let [found (when (= :list (z/tag parent))
                    (binder-lookup parent child inner symbol-name))]
        (if (some? found)
          found
          (recur child parent (z/up parent)))))))

(def ^:private top-level-def-heads #{"def" "defonce"})

(defn- defined-name
  "Find the symbol name in the name slot of a `def` form, or nil.

  Metadata stacks, as in `(def ^:private ^:const x …)`. Every metadata layer is peeled off the name."
  [name-loc]
  (case (some-> name-loc z/tag)
    :meta (recur (some-> name-loc z/down z/rightmost))
    :token (when (symbol? (z/sexpr name-loc))
             (parser/raw name-loc))
    nil))

(defn- def-binding
  "Find the name and the value a `def` form binds, as `[name value-loc]`, or nil.

  `#_` forms are dropped before the slots are read, so the value is the last written form. A `def` with no
  symbol in its name slot or no value binds nothing."
  [def-loc]
  (let [[_ name-loc & more] (parser/written-child-locs def-loc)
        defined (defined-name name-loc)
        value (last more)]
    (when (and defined value)
      [defined value])))

(defn- top-level-def?
  "True when loc is a `def` or `defonce` list that is a top-level form of the tree."
  [loc]
  (let [parent (z/up loc)]
    (and (or (nil? parent) (= :forms (z/tag parent)))
         (= :list (z/tag loc))
         (contains? top-level-def-heads (some-> loc z/down parser/raw)))))

(defn- first-top-level-form [loc]
  (let [root (loop [current loc]
               (if-let [parent (z/up current)]
                 (recur parent)
                 current))]
    (if (= :forms (z/tag root))
      (z/down root)
      (z/leftmost root))))

(defn- scan-top-level-defs
  "Return `{name → value-loc}` for every `def` in the file.
  The last `def` of a name wins, the way it does at load time: a re-`def` rebinds the
  var, and a function body called afterwards sees the value that rebinding left."
  [first-form]
  (loop [form first-form
         defs {}]
    (if (nil? form)
      defs
      (recur (z/right form)
             (or (when (top-level-def? form)
                   (when-let [[defined value] (def-binding form)]
                     (assoc defs defined value)))
                 defs)))))

(defn- def-index
  "Find the `def`s of the file being read, kept in [[parser/file-state]] for that file alone.

  A file's vectors ask about its vars over and over, so this turns a scan per question
  into a scan per file: the difference between linear and quadratic on a namespace with
  a thousand `def`s. Keyed by identity of the file's first top-level form, so a tree this
  never saw rebuilds it."
  []
  (parser/file-state ::def-index {:cached-root nil
                                  :defs {}}))

(defn- top-level-defs [loc]
  (let [first-form (first-top-level-form loc)
        root (some-> first-form z/node)
        state (def-index)
        {:keys [cached-root defs]} @state]
    (if (and root (identical? root cached-root))
      defs
      (let [scanned (scan-top-level-defs first-form)]
        (reset! state {:cached-root root
                       :defs scanned})
        scanned))))

(defn- top-level-def-value
  "Return the value a `def` in the same file binds symbol-name to, or nil.

  A file is where a var is visible, so a `def` answers wherever no local binding
  does. Nothing is read across a namespace: a symbol another file defines stays
  unknown, as it was before anything was looked up at all."
  [loc symbol-name]
  (get (top-level-defs loc) symbol-name))

(defn resolved-value
  "Return what a bare symbol names, or nil when nothing here answers for it.
  A local binding answers first, and a same-file `def` only when none does — a local
  that binds the name to something unreadable answers by stopping the search, so a
  shadowed symbol never falls through to a var that happens to share its name."
  [loc]
  (when-let [symbol-name (unqualified-symbol-name loc)]
    (let [local (lexically-bound-value loc symbol-name)]
      (cond
        (= ::shadowed local) nil
        (some? local) local
        :else (top-level-def-value loc symbol-name)))))

(defn- resolved-map
  "Return the map literal a bare symbol names, or nil."
  [loc]
  (let [value (resolved-value loc)]
    (when (= :map (some-> value z/tag))
      value)))

(def ^:private attrs-slot-kinds
  "Classifications whose second child holds attrs rather than body content."
  #{:map :dynamic-map})

(def ^:private attr-construction-heads
  "Calls that build a map from a base plus literal keys we can still read."
  #{"assoc" "merge" "assoc-in"})

(declare construction-attrs)

(def ^:private opaque-contribution
  {:attrs {}
   :complete? false
   :final #{}})

(defn- contributed
  "Return `{:attrs {kw → value-loc} :complete? bool :final #{kw}}` for one argument of such a call.
  `:complete?` says the argument's whole key set is known, not merely a floor of it. `:final` holds the keys whose
  value no unread part written after them can replace."
  [loc]
  (let [literal (fn [map-loc]
                  (let [attrs (literal-map map-loc)]
                    {:attrs (or attrs {})
                     :complete? (some? attrs)
                     :final (set (keys attrs))}))]
    (cond
      (nil? loc) {:attrs {}
                  :complete? true
                  :final #{}}
      (= :map (z/tag loc)) (literal loc)
      (= :list (z/tag loc)) (or (construction-attrs loc) opaque-contribution)
      (= :token (z/tag loc)) (if-let [resolved (resolved-map loc)]
                               (literal resolved)
                               opaque-contribution)
      :else opaque-contribution)))

(defn- splice-form?
  "True when loc is a `#?@` splice, which writes any number of forms where the source shows one."
  [loc]
  (and (= :reader-macro (z/tag loc))
       (= "?@" (some-> loc z/down parser/raw))))

(defn construction-attrs
  "Return `{:attrs {kw → value-loc} :complete? bool :final #{kw}}` for a map-building call.

  Handles `(assoc base :k v …)`, `(merge base {:k v} …)` and `(assoc-in base [:k] v)`,
  nesting through each other and through a symbol naming a map literal. Returns nil
  when the call is not one of those.

  `:complete?` is the whole point of the distinction: with an opaque base or a
  computed key the attrs are a floor, enough to answer that a key is present and
  never that one is absent. With every part readable the call states the whole map,
  and reads the same as one written out.

  `:final` holds the keys whose written value is the value of the built map: no part that cannot be read comes after
  them. `(assoc base :k v)` makes `:k` final, `(merge {:k v} base)` does not."
  [list-loc]
  (when (= :list (z/tag list-loc))
    (when-let [head (z/down list-loc)]
      (let [head-str (parser/raw head)]
        (when (contains? attr-construction-heads head-str)
          (let [args (rest (parser/written-child-locs list-loc))]
            (case head-str
              "merge"
              (reduce (fn [acc arg]
                        (let [{:keys [attrs complete? final]} (contributed arg)]
                          {:attrs (merge (:attrs acc) attrs)
                           :complete? (and (:complete? acc) complete?)
                           :final (if complete?
                                    (into (:final acc) final)
                                    final)}))
                      {:attrs {}
                       :complete? true
                       :final #{}}
                      args)

              "assoc"
              (->> args
                   rest
                   (partition-all 2)
                   (reduce (fn [acc [key-loc value-loc]]
                             (cond
                               (or (nil? value-loc) (splice-form? value-loc))
                               (reduced (assoc acc
                                               :complete? false
                                               :final #{}))

                               (parser/kw-node? key-loc)
                               (-> acc
                                   (assoc-in [:attrs (z/sexpr key-loc)] value-loc)
                                   (update :final conj (z/sexpr key-loc)))

                               :else
                               (assoc acc
                                      :complete? false
                                      :final #{})))
                           (contributed (first args))))

              "assoc-in"
              (let [[base path-loc value-loc] args
                    path-keys (when (= :vector (some-> path-loc z/tag))
                                (vec (parser/written-child-locs path-loc)))
                    outer-key (first path-keys)
                    readable-key? (and outer-key (parser/kw-node? outer-key))]
                (cond-> (contributed base)
                  ;; Only a one-key path states a value. A deeper one leaves the outer
                  ;; key holding a map that was not read, and an entry whose value is
                  ;; not the value is worse than no entry: rules read those values.
                  (and readable-key? (not (next path-keys)))
                  (-> (assoc-in [:attrs (z/sexpr outer-key)] value-loc)
                      (update :final conj (z/sexpr outer-key)))

                  (or (not readable-key?) (next path-keys))
                  (assoc :complete? false)

                  (and readable-key? (next path-keys))
                  (update :final disj (z/sexpr outer-key))

                  (not readable-key?)
                  (assoc :final #{}))))))))))

(defn attrs-info
  "Classify the second child of a Hiccup vector.

  Returns one of:
    {:kind :absent}                       ; [:img] with no children
    {:kind :map :attrs {kw → value-loc}}  ; literal map — attrs returned
    {:kind :map :attrs nil}               ; literal map with non-kw keys
    {:kind :non-map}                      ; e.g. [:img \"child\"] — no attrs slot
    {:kind :dynamic-map :attrs {kw → …}   ; (assoc base :k v) — the keys still readable
     :final-attrs {kw → …}}               ; the keys no unread part can replace
    {:kind :dynamic}                      ; non-literal (e.g. (build-attrs))

  Every classification that carries attrs also carries `:slot`, the child holding
  them, so a caller splitting attrs from body children asks once and gets both
  answers from the same reading.

  `:dynamic-map` carries a partial view, so it answers only that a key is present. Its
  `:final-attrs` also answer what a key's value is: `(assoc base :tab-index 1)` sets it,
  while `(merge {:tab-index 1} base)` lets the base replace it.
  Rules asserting something is missing need `:map` — which a built map earns too, once
  every part of the call is readable: `(assoc {:class \"c\"} :on-click f)` states its
  whole key set, while `(assoc opaque :on-click f)` states a floor of it.

  A symbol in the slot is looked up in the binding forms enclosing it and in the
  file's own `def`s, and classifies as what it names: a map literal reads as that
  literal, and a map-building call as the call would where it stands. Naming a form
  costs nothing either way. Every other symbol stays `:non-map`."
  [vec-loc]
  (let [second-child (some-> vec-loc z/down z/right)
        with-slot (fn [info]
                    (cond-> info
                      (contains? attrs-slot-kinds (:kind info)) (assoc :slot second-child)))
        built (fn [construction]
                (let [{:keys [attrs complete? final]} construction]
                  (cond
                    complete? {:kind :map
                               :attrs attrs}
                    (seq attrs) {:kind :dynamic-map
                                 :attrs attrs
                                 :final-attrs (select-keys attrs final)}
                    :else {:kind :dynamic})))]
    (with-slot
      (cond
        (nil? second-child) {:kind :absent}
        (= :map (z/tag second-child)) {:kind :map
                                       :attrs (literal-map second-child)}
        (contains? dynamic-attr-tags (z/tag second-child))
        (if-let [construction (construction-attrs second-child)]
          (built construction)
          {:kind :dynamic})

        :else
        (let [value (resolved-value second-child)]
          (case (some-> value z/tag)
            :map {:kind :map
                  :attrs (literal-map value)}

            ;; A call we cannot read at all stays `:non-map`, the way an unresolved
            ;; symbol does. A construction we can read answers as it would in the slot.
            :list (if-let [construction (construction-attrs value)]
                    (built construction)
                    {:kind :non-map})

            {:kind :non-map}))))))

(defn attrs-slot
  "Return the zloc occupying the element's attrs slot, or nil when nothing does.

  Whatever [[attrs-info]] read attrs out of occupies the slot, so the body begins
  after it — a symbol naming a map and a call building one included. A slot nothing
  could be read from is left where it was: callers have always treated it as body
  content, and narrowing that is a separate question from reading attrs."
  [vec-loc]
  (:slot (attrs-info vec-loc)))

(defn unrendered-form?
  "True when loc is itself a quoting or discarding node."
  [loc]
  (contains? unrendered-parent-tags (some-> loc z/tag)))

(def ^:private tail-result-heads
  "Forms whose value is the value of the last form written inside them."
  #{'cljs.core/binding 'cljs.core/do 'cljs.core/let 'cljs.core/let* 'cljs.core/letfn 'cljs.core/when
    'cljs.core/when-first 'cljs.core/when-let 'cljs.core/when-not 'cljs.core/when-some 'cljs.core/with-redefs
    'reagent.core/with-let})

(def ^:private branch-result-heads
  "Forms whose value is the value of one of two branches, written after a test or bindings."
  #{'cljs.core/if 'cljs.core/if-let 'cljs.core/if-not 'cljs.core/if-some})

(def ^:private result-heads
  "Every form [[result-locs]] reads."
  (into tail-result-heads (conj branch-result-heads 'cljs.core/case 'cljs.core/cond)))

(def ^:private result-head-by-name
  "Every form [[result-locs]] reads, by its bare name."
  (into {} (map (juxt name identity)) result-heads))

(defn- result-locs*
  [loc head-of]
  (let [head (when (= :list (z/tag loc)) (head-of loc))
        args (when head (remove #(= :uneval (z/tag %)) (parser/right-siblings (z/down loc))))
        branches (cond
                   (contains? tail-result-heads head) (take-last 1 args)
                   (contains? branch-result-heads head) (take 2 (rest args))
                   (= 'cljs.core/cond head) (take-nth 2 (rest args))
                   (= 'cljs.core/case head) (let [clauses (rest args)]
                                              (concat (take-nth 2 (rest clauses))
                                                      (when (odd? (count clauses)) (take-last 1 clauses)))))]
    (if (seq branches)
      (mapcat #(result-locs* % head-of) branches)
      [loc])))

(defn result-locs
  "Return the locs whose value can become the value of `loc`.

  A form written in one of these positions is one the expression yields; one written
  anywhere else — an argument, a map value, the target of a `with-meta` — is not, and
  reading it as a result answers about the wrong form. A form this does not recognize
  answers for itself, which is right for a call, a literal, or a symbol. A `#_` form is
  no argument, so it shifts no `cond` or `case` clause.

  Given `ns-info`, a head counts only when it resolves to core's form or Reagent's
  `with-let`, so a look-alike such as `x/when` answers for itself: the arity for a rule
  that flags what it reads. Without it, a head counts by its bare name: the arity for a
  guard, which gives up a finding when anything might yield what it looks for."
  ([loc]
   (result-locs* loc #(get result-head-by-name (some-> % z/down parser/sym-name))))
  ([loc ns-info]
   (result-locs* loc #(heads/resolve-head % ns-info result-heads))))

(defn- yields-hiccup?
  "True when the expression can yield a Hiccup vector rather than a value.
  Only the positions whose value becomes the expression's own are read: a vector held as
  an argument or a map value is data the call is given, not markup the call renders."
  [loc]
  (boolean (some #(and (= :vector (z/tag %))
                       (= :token (some-> % z/down z/tag)))
                 (result-locs loc))))

(defn props-slot
  "Return the child occupying the element's props slot, readable or not.

  Markup handed to an element as a prop is rendered wherever that element puts it, not
  where the prop is written, so a rule asking what an element renders must skip the whole
  slot — which [[attrs-slot]] cannot answer for, since it reports only a slot attrs can be
  read out of.

  A call in the slot is the hard case, and [[result-locs]] separates them: `(when open?
  [:a …])` yields that vector, so it is a body child, while `(build {:tooltip [:a …]})`
  yields whatever the call returns and merely holds a vector as a map value. Reagent draws
  the same line at runtime — a second child that is not a map is a child. Only a call is
  weighed this way; a quoted or metadata-wrapped second child is far more often the first
  body child than it is props."
  [vec-loc]
  (let [{:keys [kind slot]} (attrs-info vec-loc)
        second-child (some-> vec-loc z/down z/right)]
    (or slot
        (when (and (= :dynamic kind)
                   (= :list (some-> second-child z/tag))
                   (not (yields-hiccup? second-child)))
          second-child))))

(defn- form-kind
  "Return what loc's own form keeps from rendering: `:unrendered`, `:style-decl`, `:ns`, or nil."
  [loc]
  (cond
    (unrendered-form? loc) :unrendered
    (= :list (z/tag loc)) (let [head (some-> loc z/down parser/sym-name)]
                            (cond
                              (contains? style-decl-forms head) :style-decl
                              (= "ns" head) :ns))))

(defn- file-memo-table
  "Find the table by node that `file-memo`, the memo of the file being analyzed, keeps under `k`, or nil outside a file."
  ^Map [^Map file-memo k]
  (when file-memo
    (or (.get file-memo k)
        (let [table (IdentityHashMap.)]
          (.put file-memo k table)
          table))))

(declare clear-data?)

(defn- data-def-value-node
  "Find the node of the value of def-loc when def-loc is a top-level `def` whose value is a vector that is clearly data.

  A value with metadata of its own is not clearly data. With `memo`, the table of the file being analyzed, each
  top-level `def` is tested once, however many of the forms in it ask. Any other form is no `def` and needs no entry."
  [^Map memo def-loc]
  (when (top-level-def? def-loc)
    (let [node (z/node def-loc)]
      (if (and memo (.containsKey memo node))
        (.get memo node)
        (let [[_ value] (def-binding def-loc)
              value-node (when (and (= :vector (some-> value z/tag))
                                    (clear-data? value))
                           (z/node value))]
          (when memo (.put memo node value-node))
          value-node)))))

(defn- data-def-value?
  "True when loc is the value of a top-level `def` whose value is clearly data."
  [memo loc]
  (identical? (z/node loc) (some->> (z/up loc) (data-def-value-node memo))))

(defn- as-element-symbol-node? [node]
  (let [value (:value node)]
    (and (symbol? value) (= "as-element" (name value)))))

(defn- written-inner? [node]
  (and (n/inner? node) (not= :uneval (n/tag node))))

(defn- data-def-cut?
  "True when loc is a `#(…)` or a form that holds a symbol named `as-element` anywhere inside it, outside `#_`.

  The name is not resolved, as a `.cljc` file can require Reagent inside a reader conditional. A `fn` or
  `fn*` needs no check: its parameter vector is never clearly data, so no data `def` holds one."
  [loc]
  (or (= :fn (z/tag loc))
      (boolean (some as-element-symbol-node? (tree-seq written-inner? n/children (z/node loc))))))

(defn- children-kinds
  "Return the kinds of form that enclose the children of loc, given the kinds that enclose loc."
  [data-defs kinds loc]
  (let [own-kind (form-kind loc)
        kinds (cond-> kinds
                (data-def-value? data-defs loc) (conj :data-def)
                own-kind (conj own-kind))]
    (cond-> kinds
      (and (contains? kinds :data-def) (data-def-cut? loc)) (disj :data-def))))

(defn enclosing-forms
  "Return the set of the kinds of form that enclose loc at any depth: `:unrendered`, `:style-decl`, `:ns`, `:data-def`.

  `:data-def` marks the value of a top-level `def` whose value is a vector that [[clear-data?]] accepts, the value
  itself and every form in it, except the forms under a `#(…)` or a form that holds a symbol named `as-element`
  outside `#_`. One walk up to the root answers all four. While [[parser/analyze-file]] reads a file, every ancestor the walk passes
  keeps its answer, so the next vector in the same form stops at the first ancestor already asked about."
  [loc]
  (let [file-memo parser/*file-memo*
        memo (file-memo-table file-memo ::enclosing-forms)
        data-defs (file-memo-table file-memo ::data-defs)]
    (loop [below ()
           current (some-> loc z/up)]
      (if-let [known (cond
                       (nil? current) #{}
                       memo (.get memo (z/node current)))]
        (cond-> (reduce (fn [kinds ancestor]
                          (let [kinds (children-kinds data-defs kinds ancestor)]
                            (when memo (.put memo (z/node ancestor) kinds))
                            kinds))
                        known
                        below)
          (data-def-value? data-defs loc) (conj :data-def))
        (recur (cons current below) (z/up current))))))

(defn inside-non-hiccup-form?
  "True when a quoting, discarding, style-declaration or `ns` form encloses loc, or loc is in the value of a data `def`."
  [loc]
  (boolean (seq (enclosing-forms loc))))

(defn inside-unrendered-form?
  "True when a quoting or discarding form encloses loc at any depth.

  Every ancestor is asked, not just the parent: the vectors inside `'[:div [:img …]]`
  have an ordinary vector for a parent, and a rule reading one level would report the
  inner `[:img …]` while correctly skipping the outer one."
  [loc]
  (contains? (enclosing-forms loc) :unrendered))

(defn inside-style-decl?
  "True when loc has any ancestor list beginning with defclass / defattrs.
  Those macros use `[:tag {…}]` for CSS selectors + property maps, not
  Hiccup elements — rules that assume Hiccup should skip these."
  [loc]
  (contains? (enclosing-forms loc) :style-decl))

(defn inside-ns-form?
  "True when loc has an ancestor list beginning with `ns`.
  Data vectors inside `(ns … (:require […])) …` are library shapes, not
  Hiccup — rules that assume Hiccup should skip them."
  [loc]
  (contains? (enclosing-forms loc) :ns))

(defn inside-data-def?
  "True when loc is in the value of a top-level `def` whose value is a vector that [[clear-data?]] accepts.
  A form under a `#(…)` or a form that holds a symbol named `as-element` outside `#_` in that value is not."
  [loc]
  (contains? (enclosing-forms loc) :data-def))

(def malli-child-kinds
  "Malli keyword types, each with the kind of its children.

  The types are the keyword types of the `malli.core` base, type, sequence and comparator schemas at Malli 0.20.3. An
  `:entry` child is `[key properties? schema]`, a `:literal` child is a literal value, and a `:schema` child is a
  Malli schema."
  (merge (zipmap [:map :multi :catn :altn :orn :andn] (repeat :entry))
         (zipmap [:enum := :not= :< :<= :>= :re :ref] (repeat :literal))
         (zipmap [:and :or :not :map-of :vector :sequential :seqable :every :set :maybe :tuple :fn :=> :-> :function
                  :schema :malli.core/schema :any :some :nil :string :int :float :double :boolean :keyword :symbol
                  :qualified-keyword :qualified-symbol :uuid :+ :* :? :repeat :cat :alt :>]
                 (repeat :schema))))

(def ^:private data-types
  "Malli keyword types that make a vector head read as data.

  The tags in [[html-tags]], the Reagent interop `:>` and the Garden selectors `:*` and `:+` are left out. `:map`
  and `:set` stay, though they also name the HTML `<map>` and the SVG `<set>` elements, which [[html-tags]] leaves out."
  (set/difference (set (keys malli-child-kinds)) html-tags #{:> :* :+}))

(declare clear-data-element?
         unwrap-clear-meta)

(defn- clear-value?
  "True when every vector in the form at loc is clearly data.

  With inside-list? set, a vector of keywords counts only when its head is no HTML tag."
  [inside-list? loc]
  (let [value (unwrap-clear-meta loc)]
    (case (z/tag value)
      nil false
      :vector (clear-data-element? inside-list? value)
      (every? #(clear-value? inside-list? %) (parser/written-child-locs value)))))

(defn- unwrap-clear-meta
  "Find the value under the metadata of loc, or nil when a vector in that metadata is not clearly data."
  [loc]
  (if (= :meta (z/tag loc))
    (let [[metadata value] (parser/written-child-locs loc)]
      (when (clear-value? false metadata)
        (recur value)))
    loc))

(defn- vector-elements [loc]
  (let [value (unwrap-clear-meta loc)]
    (when (= :vector (z/tag value))
      (parser/written-child-locs value))))

(defn- clear-properties? [loc]
  (or (nil? loc) (clear-value? false loc)))

(defn- split-properties [elements]
  (if (= :map (z/tag (first elements)))
    [(first elements) (rest elements)]
    [nil elements]))

(declare malli-schema?)

(defn- literal-of?
  "True when loc is a token or string whose value passes one of the type predicates."
  [predicates loc]
  (and (contains? #{:token :multi-line} (z/tag loc))
       (let [literal (z/sexpr loc)]
         (some #(% literal) predicates))))

(defn- literal-value? [loc]
  (or (= :regex (z/tag loc))
      (literal-of? [keyword? symbol? string? number? boolean? nil?] loc)))

(defn- malli-entry? [loc]
  (let [[entry-key & more] (vector-elements loc)
        [properties [schema & extra]] (split-properties more)]
    (and (literal-of? [keyword? string? number?] entry-key)
         (clear-properties? properties)
         (malli-schema? schema)
         (nil? extra))))

(defn- malli-vector? [[head & more]]
  (let [[properties children] (split-properties more)
        type (when (parser/kw-node? head) (z/sexpr head))]
    (and (contains? data-types type)
         (clear-properties? properties)
         (every? (case (malli-child-kinds type)
                   :entry malli-entry?
                   :literal literal-value?
                   :schema malli-schema?)
                 children))))

(defn- malli-schema? [loc]
  (let [value (unwrap-clear-meta loc)]
    (case (z/tag value)
      :vector (malli-vector? (parser/written-child-locs value))
      :list (clear-value? true value)
      :regex true
      :token (let [literal (z/sexpr value)]
               (or (keyword? literal) (symbol? literal)))
      false)))

(defn malli-schema-vector?
  "True when the vector at loc is a Malli schema all through, as [[clear-data?]] reads one.

  Metadata on the vector is looked through, and a `#_` form is no element."
  [loc]
  (boolean (some-> (vector-elements loc) malli-vector?)))

(defn- clear-data-element? [inside-list? loc]
  (let [elements (vector-elements loc)
        head (first elements)]
    (cond
      (parser/kw-node? head) (or (malli-vector? elements)
                                 (and (next elements)
                                      (every? parser/kw-node? elements)
                                      (not (and inside-list? (html-tag? (parse-tag (parser/raw head)))))))
      head (every? #(clear-data-element? true %) elements))))

(defn clear-data?
  "True when the vector at loc has a shape that a data model has and markup does not.

  A vector is clearly data when it is a Malli schema all through, when it has two or more
  elements and each is a keyword, or when its head is a vector and each element is clearly
  data. Inside such a list, or inside a call in a schema position, a vector of keywords counts
  only when its head, read without class and id shorthand, is no HTML tag. Every key and value
  of a properties map is clearly data too. A `#_` form is no element, and metadata on a vector
  is looked through, but every vector in that metadata must be clearly data."
  [loc]
  (boolean (clear-data-element? false loc)))

(declare schema-parts)

(defn- properties-parts [properties]
  (when properties
    (remove #(clear-value? false %) (parser/written-child-locs properties))))

(defn- entry-parts [loc]
  (let [[entry-key & more] (vector-elements loc)
        [properties [schema & extra]] (split-properties more)]
    (if (and (literal-of? [keyword? string? number?] entry-key) schema)
      (concat (properties-parts properties) (schema-parts schema) extra)
      [loc])))

(defn- malli-vector-parts [loc]
  (let [[head & more] (vector-elements loc)
        [properties children] (split-properties more)
        type (when (parser/kw-node? head) (z/sexpr head))]
    (if (contains? data-types type)
      (concat (properties-parts properties)
              (mapcat (case (malli-child-kinds type)
                        :entry entry-parts
                        :literal #(when-not (literal-value? %) [%])
                        :schema schema-parts)
                      children))
      [loc])))

(defn- schema-parts [loc]
  (cond
    (malli-schema? loc) []
    (vector-elements loc) (malli-vector-parts loc)
    :else [loc]))

(defn- top-level-form [loc]
  (loop [current loc]
    (let [parent (z/up current)]
      (if (or (nil? parent) (= :forms (z/tag parent)))
        current
        (recur parent)))))

(defn- inside? [loc ancestor]
  (let [node (z/node ancestor)]
    (boolean (some #(identical? node (z/node %)) (take-while some? (iterate z/up loc))))))

(defn- snippet [loc]
  (let [text (str/replace (parser/raw loc) #"\s+" " ")]
    (if (< 40 (count text)) (str (subs text 0 37) "...") text)))

(defn- cut-node?
  "True when node is a `#(…)` or a symbol named `as-element`, either of which ends the data `def` rule."
  [node]
  (or (= :fn (n/tag node)) (as-element-symbol-node? node)))

(defn- reads-as-data-without?
  "True when the vector at value-loc is a data `def` value once a symbol takes the place of part, which value-loc holds.

  The value must be clearly data and hold no `#(…)` and no `as-element`, since either ends the data `def` rule."
  [value-loc part]
  (let [depth (count (take-while #(not (identical? (z/node %) (z/node value-loc))) (iterate z/up part)))
        value (nth (iterate z/up (z/replace part (n/token-node 'extracted))) depth)]
    (and (clear-data? value)
         (not-any? cut-node? (tree-seq written-inner? n/children (z/node value))))))

(defn mixed-schema-part
  "Show the one part that keeps the top-level Malli schema `def` around loc from reading as data, or nil.

  The `def`'s value is a vector, with no metadata of its own, that [[clear-data?]] does not accept because of exactly
  one part, such as Hiccup in a property value or a function. Moving that part into its own `def` or function must make
  the value read as data, so a symbol is put in its place and the value is tested again. A loc inside that part gets
  nil: it stays markup once the part moves out."
  [loc]
  (let [top (top-level-form loc)
        [_ value] (when (top-level-def? top) (def-binding top))]
    (when (and (= :vector (some-> value z/tag))
               (inside? loc value))
      (let [[part & more] (malli-vector-parts value)]
        (when (and part
                   (empty? more)
                   (not (inside? loc part))
                   (reads-as-data-without? value part))
          (snippet part))))))

(defn mixed-schema-note
  "Build the hint note for a finding at loc in a Malli schema that one part keeps from reading as data, or nil."
  [loc]
  (when-let [part (mixed-schema-part loc)]
    (str "The Malli schema around it reads as markup because of " part
         ". Move that part into its own def or function, so the schema reads as data.")))

(defn add-note
  "Add note to the hint of finding, after the hint it already has."
  [finding note]
  (cond-> finding
    note (update :hint #(if % (str % " " note) note))))
