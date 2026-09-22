(ns cljs-patrol.groups.spade.selectors
  "Nested-selector rules for Spade style declarations.
  Both rules flag selector syntax Garden accepts and compiles into CSS that matches nothing."
  (:require
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private combinator-keywords
  "Keywords Garden emits as separate selectors in a comma-joined list instead of as combinators.
  The general-sibling combinator has no keyword spelling to check — the reader rejects `:~`."
  #{":>" ":+"})

(defn- children [loc]
  (take-while some? (iterate z/right (z/down loc))))

(defn- nested-vectors
  "Every vector under `loc`, depth-first, without descending into property maps.
  Garden property values are vectors too (`{:margin [[0 :auto]]}`), and none of them are selectors."
  [loc]
  (mapcat (fn [child]
            (case (z/tag child)
              :map nil
              :vector (cons child (nested-vectors child))
              (nested-vectors child)))
          (children loc)))

(defn- selector-head
  "Zlocs of a nested selector vector up to its property map.
  Garden reads those as the selector; the map and anything after it is the body."
  [vector-loc]
  (take-while #(not= :map (z/tag %)) (children vector-loc)))

(defn- selector-block? [vector-loc]
  (boolean (some #(= :map (z/tag %)) (children vector-loc))))

(defn- string-value [loc]
  (when (= :token (z/tag loc))
    (let [value (try (z/sexpr loc) (catch Exception _ nil))]
      (when (string? value) value))))

(defn- ampersand-off-front?
  "True when `&` appears at any index but 0.
  Garden substitutes the parent reference only at the front of a string selector; anywhere
  else the `&` survives into the stylesheet as a literal character."
  [selector]
  (and (seq selector)
       (some? (str/index-of (subs selector 1) "&"))))

(defn- ampersand-findings [block style-kw file]
  (for [head-loc (selector-head block)
        :let [selector (string-value head-loc)]
        :when (and selector (ampersand-off-front? selector))]
    {:kw style-kw
     :type :spade-ampersand-not-at-start
     :selector selector
     :form (str style-kw " " (parser/raw head-loc))
     :file file
     :row (parser/position-row head-loc)}))

(defn- combinator-finding [block style-kw file]
  (let [selectors (mapv parser/raw (selector-head block))]
    (when (and (> (count selectors) 1)
               (some combinator-keywords selectors))
      {:kw style-kw
       :type :spade-keyword-combinator-selector
       :selectors selectors
       :form (str style-kw " [" (str/join " " selectors) "]")
       :file file
       :row (parser/position-row block)})))

(defn findings
  "Selector-syntax findings for one defclass/defattrs form, at every nesting depth."
  [list-loc style-kw file]
  (let [blocks (filter selector-block? (nested-vectors list-loc))]
    (concat (mapcat #(ampersand-findings % style-kw file) blocks)
            (keep #(combinator-finding % style-kw file) blocks))))
