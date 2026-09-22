(ns cljs-patrol.groups.spade.selectors
  "Nested-selector rules for Spade style declarations.
  Both rules flag selector syntax Garden accepts and compiles into CSS that matches nothing."
  (:require
   [cljs-patrol.parser :as parser]
   [cljs-patrol.style-blocks :as blocks]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private combinator-keywords
  "Keywords Garden emits as separate selectors in a comma-joined list instead of as combinators.
  The general-sibling combinator has no keyword spelling to check — the reader rejects `:~`."
  #{":>" ":+"})

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
  (for [head-loc (blocks/selector-head block)
        :let [selector (string-value head-loc)]
        :when (and selector (ampersand-off-front? selector))]
    {:kw style-kw
     :type :spade-ampersand-not-at-start
     :selector selector
     :form (str style-kw " " (parser/raw head-loc))
     :file file
     :row (parser/position-row head-loc)}))

(defn- combinator-finding [block style-kw file]
  (let [selectors (mapv parser/raw (blocks/selector-head block))]
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
  (let [selector-blocks (blocks/selector-blocks list-loc)]
    (concat (mapcat #(ampersand-findings % style-kw file) selector-blocks)
            (keep #(combinator-finding % style-kw file) selector-blocks))))
