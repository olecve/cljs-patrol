(ns cljs-patrol.groups.spade
  "Spade rule group: detects unused CSS-in-CLJS style declarations."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade.selectors :as selectors]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.style-blocks :as blocks]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(defn- pseudo-selector-key? [kw-str]
  (str/starts-with? kw-str ":&"))

(defn- pseudo-findings
  "A `:&`-prefixed key belongs to a selector, not to a style map, wherever the map sits.
  Nested blocks carry the same defect as the base map, so every one of them is read."
  [list-loc style-kw file]
  (for [{:keys [map-loc selector]} (blocks/style-maps list-loc)
        key-loc (blocks/map-key-locs map-loc)
        :when (parser/kw-node? key-loc)
        :let [pseudo (parser/raw key-loc)]
        :when (pseudo-selector-key? pseudo)]
    {:kw style-kw
     :type :pseudo-in-main-map
     :selector pseudo
     :block selector
     :form (str style-kw " " (if (str/blank? selector) pseudo (str selector " " pseudo)))
     :file file
     :row (parser/position-row key-loc)}))

(defn- leading-self-selectors [vector-loc]
  (loop [loc (z/down vector-loc)
         acc []]
    (if (and loc
             (parser/kw-node? loc)
             (pseudo-selector-key? (parser/raw loc)))
      (recur (z/right loc) (conj acc (parser/raw loc)))
      acc)))

(defn- parent-selector
  "The selector path above `block`, empty at the top level of the declaration."
  [list-loc block]
  (let [parent (z/up block)]
    (when-not (= (z/node parent) (z/node list-loc))
      (blocks/selector-label parent))))

(defn- consecutive-self-selector-findings
  "Garden reads [:a :b {…}] as a descendant selector at every depth, not only at the top."
  [list-loc style-kw file]
  (for [block (blocks/selector-blocks list-loc)
        :let [selectors (leading-self-selectors block)
              parent (parent-selector list-loc block)]
        :when (>= (count selectors) 2)]
    {:kw style-kw
     :type :consecutive-self-selectors
     :selectors selectors
     :block (or parent "")
     :form (str style-kw " " (when parent (str parent " ")) "[" (str/join " " selectors) "]")
     :file file
     :row (parser/position-row block)}))

(defn- class-only-map?
  "True if `value-loc` is the value of `:class` in a map with no other keys."
  [value-loc]
  (let [left (z/left value-loc)
        parent (z/up value-loc)]
    (and left
         (= :token (z/tag left))
         (= ":class" (parser/raw left))
         parent
         (= :map (z/tag parent))
         (= 2 (count (z/child-sexprs parent))))))

(defn- handle-list
  "Detect style declarations and usages from list nodes.
  Handles: defclass/defattrs declarations, and catch-all style function calls."
  [loc {:keys [ns-name aliases]} file]
  (let [op-token (z/down loc)
        operator (parser/sym-name op-token)
        op-raw (when (and op-token (= :token (z/tag op-token))) (parser/raw op-token))
        row (parser/position-row loc)]
    (cond
      (contains? blocks/style-decl-fns operator)
      (let [name-loc (parser/declared-name-loc loc)]
        (when (and name-loc (= :token (z/tag name-loc)))
          (when-let [style-name (parser/sym-name name-loc)]
            (let [style-kw (keyword ns-name style-name)]
              {:decls (-> [{:kw style-kw
                            :type (keyword operator)
                            :file file
                            :row (parser/position-row name-loc)}]
                          (into (pseudo-findings loc style-kw file))
                          (into (consecutive-self-selector-findings loc style-kw file))
                          (into (selectors/findings loc style-kw file)))
               :usages []
               :dynamics []}))))

      operator
      (when-let [resolved (parser/resolve-sym op-raw ns-name aliases)]
        (let [parent (z/up loc)
              parent-first (when parent (z/down parent))
              parent-op (when parent-first (parser/sym-name parent-first))
              context (cond
                        (= "merge" parent-op)
                        :in-merge

                        (class-only-map? loc)
                        :class-only-map

                        (and parent
                             (= :vector (z/tag parent))
                             (= 1 (count (z/child-sexprs parent)))
                             (class-only-map? parent))
                        :class-only-map

                        :else nil)]
          {:decls []
           :dynamics []
           :usages [{:kw resolved
                     :type :style-call
                     :file file
                     :row row
                     :context context}]}))

      :else nil)))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(contains? #{:defclass :defattrs} (:type %)) declarations)
        pseudo-in-main-map (filter #(= :pseudo-in-main-map (:type %)) declarations)
        consecutive-self-selectors (filter #(= :consecutive-self-selectors (:type %)) declarations)
        ampersand-not-at-start (filter #(= :spade-ampersand-not-at-start (:type %)) declarations)
        keyword-combinator (filter #(= :spade-keyword-combinator-selector (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        style-call-kws (set (map :kw style-calls))
        unused-styles (remove #(contains? style-call-kws (:kw %)) style-decls)
        usages-by-kw (group-by :kw style-calls)
        defattrs-in-merge (for [decl style-decls
                                :when (= :defattrs (:type decl))
                                :when (some #(= :in-merge (:context %))
                                            (get usages-by-kw (:kw decl)))]
                            decl)]
    {:unused-styles (parser/distinct-by :kw unused-styles)
     :defattrs-in-merge (vec defattrs-in-merge)
     :pseudo-in-main-map (vec pseudo-in-main-map)
     :consecutive-self-selectors (vec consecutive-self-selectors)
     :spade-ampersand-not-at-start (vec ampersand-not-at-start)
     :spade-keyword-combinator-selector (vec keyword-combinator)}))

(defn- summary-lines* [{:keys [unused-styles defattrs-in-merge pseudo-in-main-map consecutive-self-selectors
                               spade-ampersand-not-at-start spade-keyword-combinator-selector]}]
  [["Unused styles:" (count unused-styles)]
   ["defattrs in merge:" (count defattrs-in-merge)]
   ["Pseudo-selector in main map:" (count pseudo-in-main-map)]
   ["Consecutive self-selectors:" (count consecutive-self-selectors)]
   ["Ampersand not at start:" (count spade-ampersand-not-at-start)]
   ["Keyword combinator selector:" (count spade-keyword-combinator-selector)]])

(defn- failed?* [{:keys [unused-styles pseudo-in-main-map consecutive-self-selectors
                         spade-ampersand-not-at-start spade-keyword-combinator-selector]}]
  (boolean (or (seq unused-styles)
               (seq pseudo-in-main-map)
               (seq consecutive-self-selectors)
               (seq spade-ampersand-not-at-start)
               (seq spade-keyword-combinator-selector))))

(defrecord SpadeGroup []
  group/RuleGroup
  (group-id [_] :spade)
  (group-name [_] "Spade")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:unused-styles
     "Declared with defclass or defattrs but never called. Remove the declaration, or add a call site where the style should be applied."
     :defattrs-in-merge
     "Declared with defattrs but used inside merge. Use defclass instead so callers can pass it via :class without merge."
     :pseudo-in-main-map
     "Pseudo-selector key placed inside the main style map. Spade emits it as an invalid CSS property, so the rule is silently dropped. Move it out into its own vector, e.g. [:&:hover {...}], after the main map."
     :consecutive-self-selectors
     "Two or more self-selector keywords (e.g. :&:before :&:after) appear consecutively before the style map. Garden treats [:a :b {...}] as a descendant selector (a b), not the comma-joined selector the author intended. Split into separate sibling vectors ([:&:before {...}] [:&:after {...}]) or use a set for comma-join (#{:&:before :&:after})."
     :spade-ampersand-not-at-start
     "Garden only substitutes & at the start of a string selector; move & to position 0 or restructure the selector."
     :spade-keyword-combinator-selector
     "Combinators inside Spade selector vectors are treated as separate selectors joined by ,, not applied to the following element. Use the string form, e.g. [\"> span\" {...}] for the child combinator."})
  (rule->tier [_]
    {:unused-styles :cleanup
     :defattrs-in-merge :deprecations
     :pseudo-in-main-map :bugs
     :consecutive-self-selectors :bugs
     :spade-ampersand-not-at-start :bugs
     :spade-keyword-combinator-selector :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->SpadeGroup))
