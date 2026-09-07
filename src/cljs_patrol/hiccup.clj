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

(def ^:private attr-construction-heads
  "Calls that build a map from a base plus literal keys we can still read."
  #{"assoc" "merge" "assoc-in"})

(declare construction-attrs)

(defn- contributed-attrs [loc]
  (cond
    (nil? loc) {}
    (= :map (z/tag loc)) (or (literal-map loc) {})
    (= :list (z/tag loc)) (or (construction-attrs loc) {})
    :else {}))

(defn- right-args [head]
  (when-let [arg (z/right head)]
    (cons arg (lazy-seq (right-args arg)))))

(defn construction-attrs
  "Return `{kw → value-zloc}` for the keys a map-building call definitely contributes.
  Handles `(assoc base :k v …)`, `(merge base {:k v} …)` and `(assoc-in base [:k …] v)`,
  nesting through each other. Returns nil when the call is not one of those.

  The base may be opaque and computed keys are skipped, so the result is a floor on
  what the map holds: it can answer that a key is present, never that one is absent."
  [list-loc]
  (when (= :list (z/tag list-loc))
    (when-let [head (z/down list-loc)]
      (let [head-str (parser/raw head)]
        (when (contains? attr-construction-heads head-str)
          (let [args (right-args head)]
            (case head-str
              "merge"
              (reduce (fn [acc arg] (merge acc (contributed-attrs arg))) {} args)

              "assoc"
              (reduce (fn [acc [key-loc value-loc]]
                        (if (parser/kw-node? key-loc)
                          (assoc acc (z/sexpr key-loc) value-loc)
                          acc))
                      (contributed-attrs (first args))
                      (partition 2 (rest args)))

              "assoc-in"
              (let [[base path-loc value-loc] args
                    outer-key (when (and path-loc (= :vector (z/tag path-loc)))
                                (z/down path-loc))]
                (cond-> (contributed-attrs base)
                  (and outer-key (parser/kw-node? outer-key))
                  (assoc (z/sexpr outer-key) value-loc))))))))))

(defn attrs-info
  "Classify the second child of a Hiccup vector.

  Returns one of:
    {:kind :absent}                       ; [:img] with no children
    {:kind :map :attrs {kw → value-loc}}  ; literal map — attrs returned
    {:kind :map :attrs nil}               ; literal map with non-kw keys
    {:kind :non-map}                      ; e.g. [:img \"child\"] — no attrs slot
    {:kind :dynamic-map :attrs {kw → …}}  ; (assoc base :k v) — the keys still readable
    {:kind :dynamic}                      ; non-literal (e.g. (build-attrs))

  `:dynamic-map` carries a partial view, so it answers only that a key is present.
  Rules asserting something is missing need `:map`."
  [vec-loc]
  (let [second-child (some-> vec-loc z/down z/right)]
    (cond
      (nil? second-child) {:kind :absent}
      (= :map (z/tag second-child)) {:kind :map
                                     :attrs (literal-map second-child)}
      (contains? dynamic-attr-tags (z/tag second-child))
      (if-let [built (seq (construction-attrs second-child))]
        {:kind :dynamic-map
         :attrs (into {} built)}
        {:kind :dynamic})
      :else {:kind :non-map})))

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
