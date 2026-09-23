(ns cljs-patrol.hiccup
  "Hiccup-shape helpers for rule groups analyzing literal Hiccup vectors.

  Every helper here is conservative — it returns nil / a sentinel when the
  form isn't a plain literal (dynamic tag, computed keys, spliced attrs,
  quoted vectors, etc.). Callers can then decide whether to silently skip
  or flag; this ns never makes that policy call."
  (:require
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private dynamic-attr-tags
  "Zipper tags that indicate the attrs slot is a non-literal form.
  Covers function calls, quoted / spliced forms, metadata-wrapped values, reader macros, etc."
  #{:list :fn :syntax-quote :unquote :unquote-splicing :reader-macro :meta})

(def ^:private quoted-parent-tags
  "Parent tags that turn their child vector into a data literal, not code.
  Rule groups skip those to avoid flagging Hiccup used as test data or in
  macro bodies."
  #{:quote :syntax-quote :unquote :unquote-splicing})

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
  schema, so reading one as Hiccup costs more than missing the other."
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
    :mask :pattern :clipPath :linearGradient :radialGradient :stop :marker :symbol :foreignObject})

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
                hash hash
                :else nil)
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

(defn- unwrap-meta
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

(defn- lexically-bound-value
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
  "Return the name a `def` form defines, or nil.
  Metadata stacks — `(def ^:private ^:const x …)` nests one `:meta` node inside
  another — so the name is whatever is left once every layer is peeled off."
  [list-loc]
  (loop [name-loc (some-> list-loc z/down z/right)]
    (case (some-> name-loc z/tag)
      :meta (recur (some-> name-loc z/down z/rightmost))
      :token (parser/raw name-loc)
      nil)))

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
             (or (when (and (= :list (z/tag form))
                            (contains? top-level-def-heads (some-> form z/down parser/raw)))
                   (let [name-loc (some-> form z/down z/right)
                         value (some-> form z/down z/rightmost)
                         defined (defined-name form)]
                     (when (and defined (not (same-node? name-loc value)))
                       (assoc defs defined value))))
                 defs)))))

(def ^:private def-index
  "The `def`s of the file being read, held one file at a time.
  Files are walked one after another, and a file's vectors ask about its vars over
  and over, so this turns a scan per question into a scan per file — the difference
  between linear and quadratic on a namespace with a thousand `def`s. Keyed by
  identity of the file's first top-level form, so a file this never saw rebuilds it.
  One entry is enough because files are read in sequence; walking them in parallel
  would make each file rebuild what the last one cached, correctly but pointlessly."
  (atom {:cached-root nil
         :defs {}}))

(defn- top-level-defs [loc]
  (let [first-form (first-top-level-form loc)
        root (some-> first-form z/node)
        {:keys [cached-root defs]} @def-index]
    (if (and root (identical? root cached-root))
      defs
      (let [scanned (scan-top-level-defs first-form)]
        (reset! def-index {:cached-root root
                           :defs scanned})
        scanned))))

(defn- top-level-def-value
  "Return the value a `def` in the same file binds symbol-name to, or nil.
  A file is where a var is visible, so a `def` answers wherever no local binding
  does. Nothing is read across a namespace: a symbol another file defines stays
  unknown, as it was before anything was looked up at all."
  [loc symbol-name]
  (get (top-level-defs loc) symbol-name))

(defn- resolved-value
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

(defn- contributed
  "Return `{:attrs {kw → value-loc} :complete? bool}` for one argument of such a call.
  `:complete?` says the argument's whole key set is known, not merely a floor of it."
  [loc]
  (let [literal (fn [map-loc]
                  (let [attrs (literal-map map-loc)]
                    {:attrs (or attrs {})
                     :complete? (some? attrs)}))]
    (cond
      (nil? loc) {:attrs {}
                  :complete? true}
      (= :map (z/tag loc)) (literal loc)
      (= :list (z/tag loc)) (or (construction-attrs loc) {:attrs {}
                                                          :complete? false})
      (= :token (z/tag loc)) (if-let [resolved (resolved-map loc)]
                               (literal resolved)
                               {:attrs {}
                                :complete? false})
      :else {:attrs {}
             :complete? false})))

(defn- right-args [head]
  (when-let [arg (z/right head)]
    (cons arg (lazy-seq (right-args arg)))))

(defn construction-attrs
  "Return `{:attrs {kw → value-loc} :complete? bool}` for a map-building call.
  Handles `(assoc base :k v …)`, `(merge base {:k v} …)` and `(assoc-in base [:k] v)`,
  nesting through each other and through a symbol naming a map literal. Returns nil
  when the call is not one of those.

  `:complete?` is the whole point of the distinction: with an opaque base or a
  computed key the attrs are a floor, enough to answer that a key is present and
  never that one is absent. With every part readable the call states the whole map,
  and reads the same as one written out."
  [list-loc]
  (when (= :list (z/tag list-loc))
    (when-let [head (z/down list-loc)]
      (let [head-str (parser/raw head)]
        (when (contains? attr-construction-heads head-str)
          (let [args (right-args head)]
            (case head-str
              "merge"
              (reduce (fn [acc arg]
                        (let [{:keys [attrs complete?]} (contributed arg)]
                          {:attrs (merge (:attrs acc) attrs)
                           :complete? (and (:complete? acc) complete?)}))
                      {:attrs {}
                       :complete? true}
                      args)

              "assoc"
              (reduce (fn [acc [key-loc value-loc]]
                        (if (parser/kw-node? key-loc)
                          (assoc-in acc [:attrs (z/sexpr key-loc)] value-loc)
                          (assoc acc :complete? false)))
                      (contributed (first args))
                      (partition 2 (rest args)))

              "assoc-in"
              (let [[base path-loc value-loc] args
                    path-keys (when (= :vector (some-> path-loc z/tag))
                                (vec (take-while some? (iterate z/right (z/down path-loc)))))
                    outer-key (first path-keys)
                    readable-key? (and outer-key (parser/kw-node? outer-key))]
                (cond-> (contributed base)
                  ;; Only a one-key path states a value. A deeper one leaves the outer
                  ;; key holding a map that was not read, and an entry whose value is
                  ;; not the value is worse than no entry: rules read those values.
                  (and readable-key? (not (next path-keys)))
                  (assoc-in [:attrs (z/sexpr outer-key)] value-loc)

                  (or (not readable-key?) (next path-keys))
                  (assoc :complete? false))))))))))

(defn attrs-info
  "Classify the second child of a Hiccup vector.

  Returns one of:
    {:kind :absent}                       ; [:img] with no children
    {:kind :map :attrs {kw → value-loc}}  ; literal map — attrs returned
    {:kind :map :attrs nil}               ; literal map with non-kw keys
    {:kind :non-map}                      ; e.g. [:img \"child\"] — no attrs slot
    {:kind :dynamic-map :attrs {kw → …}}  ; (assoc base :k v) — the keys still readable
    {:kind :dynamic}                      ; non-literal (e.g. (build-attrs))

  Every classification that carries attrs also carries `:slot`, the child holding
  them, so a caller splitting attrs from body children asks once and gets both
  answers from the same reading.

  `:dynamic-map` carries a partial view, so it answers only that a key is present.
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
                (let [{:keys [attrs complete?]} construction]
                  (cond
                    complete? {:kind :map
                               :attrs attrs}
                    (seq attrs) {:kind :dynamic-map
                                 :attrs attrs}
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

(defn inside-quoted-form? [loc]
  (some-> loc z/up z/tag quoted-parent-tags boolean))

(defn inside-style-decl?
  "True when loc has any ancestor list beginning with defclass / defattrs.
  Those macros use `[:tag {…}]` for CSS selectors + property maps, not
  Hiccup elements — rules that assume Hiccup should skip these."
  [loc]
  (loop [cur (some-> loc z/up)]
    (cond
      (nil? cur) false

      (and (= :list (z/tag cur))
           (contains? style-decl-forms (some-> cur z/down parser/sym-name)))
      true

      :else (recur (z/up cur)))))

(defn inside-ns-form?
  "True when loc has an ancestor list beginning with `ns`.
  Data vectors inside `(ns … (:require […])) …` are library shapes, not
  Hiccup — rules that assume Hiccup should skip them."
  [loc]
  (loop [cur (some-> loc z/up)]
    (cond
      (nil? cur) false

      (and (= :list (z/tag cur))
           (= "ns" (some-> cur z/down parser/sym-name)))
      true

      :else (recur (z/up cur)))))
