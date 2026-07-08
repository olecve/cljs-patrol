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
  "Zipper tags that indicate the attrs slot is a non-literal form: function
  calls, quoted / spliced forms, metadata-wrapped values, reader macros, etc."
  #{:list :fn :syntax-quote :unquote :unquote-splicing :reader-macro :meta})

(def ^:private quoted-parent-tags
  "Parent tags that turn their child vector into a data literal, not code.
  Rule groups skip those to avoid flagging Hiccup used as test data or in
  macro bodies."
  #{:quote :syntax-quote :unquote :unquote-splicing})

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

(defn attrs-info
  "Classify the second child of a Hiccup vector.

  Returns one of:
    {:kind :absent}                       ; [:img] with no children
    {:kind :map :attrs {kw → value-loc}}  ; literal map — attrs returned
    {:kind :map :attrs nil}               ; literal map with non-kw keys
    {:kind :non-map}                      ; e.g. [:img \"child\"] — no attrs slot
    {:kind :dynamic}                      ; non-literal (e.g. (build-attrs))"
  [vec-loc]
  (let [second-child (some-> vec-loc z/down z/right)]
    (cond
      (nil? second-child) {:kind :absent}
      (= :map (z/tag second-child)) {:kind :map
                                     :attrs (literal-map second-child)}
      (contains? dynamic-attr-tags (z/tag second-child)) {:kind :dynamic}
      :else {:kind :non-map})))

(defn inside-quoted-form? [loc]
  (some-> loc z/up z/tag quoted-parent-tags boolean))
