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
       (or (hiccup/html-tag? (hiccup/parse-tag (parser/raw loc)))
           (some? (parser/sym-name loc)))))

(defn- data-vector?
  "True when the vector's second element is also a keyword literal.
  The shape `[:ns :key …]` almost always encodes a re-frame path or
  Malli/spec schema, not a Hiccup element."
  [vec-loc]
  (let [second-loc (some-> vec-loc z/down z/right)]
    (and second-loc (parser/kw-node? second-loc))))

(defn- locs-right [loc]
  (take-while some? (rest (iterate #(some-> % z/right) loc))))

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
        (= :vector (z/tag child)) (locs-right child)
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
          (locs-right argument))

        (contains? mapping-heads head-name)
        (case (some-> argument z/tag)
          :fn (when-let [body (z/down argument)] [body])
          :list (function-body-locs argument)
          nil)))))

(def ^:private tail-result-heads
  "Forms whose value is the value of the last form written inside them."
  #{"do" "let" "let*" "letfn" "when" "when-not" "when-let" "when-some" "when-first"
    "binding" "with-let" "with-redefs"})

(def ^:private branch-result-heads
  "Forms whose value is the value of one of two branches, written after a test or bindings."
  #{"if" "if-not" "if-let" "if-some"})

(defn- result-locs
  "Return the locs whose value can become the value of `loc`.
  An element written in one of these positions is an element the mapping produces; one
  written anywhere else — an argument, the target of a `with-meta` — is not, and asking
  about it would read a key off the wrong element. A form this does not recognize
  answers for itself, which is right for a call, a literal, or a symbol."
  [loc]
  (let [head-name (when (= :list (z/tag loc)) (some-> loc z/down parser/sym-name))
        args (when head-name (locs-right (z/down loc)))
        branches (cond
                   (contains? tail-result-heads head-name) (take-last 1 args)
                   (contains? branch-result-heads head-name) (take 2 (rest args))
                   (= "cond" head-name) (take-nth 2 (rest args))
                   (= "case" head-name) (let [clauses (rest args)]
                                          (concat (take-nth 2 (rest clauses))
                                                  (when (odd? (count clauses)) (take-last 1 clauses)))))]
    (if (seq branches)
      (mapcat result-locs branches)
      [loc])))

(defn- key-meta-map?
  "True when a literal map attaches a `:key`."
  [map-loc]
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
  "True when `loc` derefs something.
  Reagent's documented caveat is a ratom deref inside a lazy seq: the deref then happens
  outside the component's reactive context and the component stops re-rendering. Whether
  the target is a ratom is not readable here, so every deref counts — a body that derefs
  keeps the eager `into` rather than being turned into a lazy `for`."
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
             (let [results (result-locs (last bodies))]
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

(def ^:private spade-handle-list
  (get (group/parse-handlers spade/group) :handle-list))

(defn- handle-into
  "Emit a :redundant-into-hiccup finding when `loc` matches the anti-pattern."
  [loc _ns-info file]
  (when (and (not (hiccup/inside-quoted-form? loc))
             (not (hiccup/inside-style-decl? loc))
             (not (hiccup/inside-ns-form? loc))
             (redundant-into? loc))
    {:decls [(redundant-into-finding loc file)]
     :usages []
     :dynamics []}))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(= :defclass (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        usages-by-kw (group-by :kw style-calls)
        defclass-as-sole-attr (for [decl style-decls
                                    :let [uses (get usages-by-kw (:kw decl))]
                                    :when (seq uses)
                                    :when (every? #(= :class-only-map (:context %)) uses)]
                                decl)
        redundant-into (filter #(= :redundant-into-hiccup (:type %)) declarations)]
    {:defclass-as-sole-attr (vec defclass-as-sole-attr)
     :redundant-into-hiccup (vec redundant-into)}))

(defn- summary-lines* [{:keys [defclass-as-sole-attr redundant-into-hiccup]}]
  [["defclass as sole attr:" (count defclass-as-sole-attr)]
   ["Redundant into-hiccup:" (count redundant-into-hiccup)]])

(defn- failed?* [_] false)

(defrecord ReagentGroup []
  group/RuleGroup
  (group-id [_] :reagent)
  (group-name [_] "Reagent")
  (parse-handlers [_] {:handle-list [spade-handle-list handle-into]})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:defclass-as-sole-attr
     "Declared with defclass but every usage is {:class (style-fn)}. Use defattrs instead to avoid the :class wrapper."
     :redundant-into-hiccup
     "The mapped body already attaches a `:key` to every element it produces, so nothing is left for the `into` to earn: write the sequence inside the Hiccup vector instead — `[:<> (for [x xs] ^{:key x} [item x])]`. A keyless sequence is not flagged; there the `into` is load-bearing, splicing the elements in as positional children so React never sees a sequence missing its keys."})
  (rule->tier [_]
    {:defclass-as-sole-attr :deprecations
     :redundant-into-hiccup :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReagentGroup))
