(ns cljs-patrol.groups.spade
  "Spade rule group: detects unused CSS-in-CLJS style declarations."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade.selectors :as selectors]
   [cljs-patrol.heads :as heads]
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
         (= 2 (count (heads/code-forms (z/node parent)))))))

(def ^:private whole-reading-heads
  "Core functions that read every argument as a collection."
  #{'cljs.core/into 'cljs.core/merge})

(def ^:private target-reading-heads
  "Core functions that read their first argument as a map, and take the others as keys or values."
  #{'cljs.core/assoc 'cljs.core/assoc-in 'cljs.core/conj 'cljs.core/dissoc 'cljs.core/get 'cljs.core/get-in
    'cljs.core/update})

(def ^:private thread-first-heads
  "Forms that pass each step a value before the arguments the step writes."
  #{"->" "cond->" "some->"})

(def ^:private class-map-merging-heads
  "Functions that merge a `{:class …}` map with other props, so the map is not the whole props map."
  #{'reagent.core/merge-props})

(def ^:private js-object-heads
  "Functions that turn a `{:class …}` map into a JS object, so the map is not a props map."
  #{'cljs.core/clj->js})

(def ^:private map-reading-names
  "The names of the heads that can read a call as a map, whatever namespace they resolve to."
  (into #{"->"} (map name) (concat whole-reading-heads target-reading-heads)))

(defn- may-read-as-map?
  "True when list `loc` has a keyword head or a head named like one that reads a map. A cheap test before resolving."
  [loc]
  (let [head (z/down loc)]
    (or (parser/kw-node? head)
        (contains? map-reading-names (parser/sym-name head)))))

(defn- argument-index
  "Count the forms before `loc` in its list, the head and the `#_` forms excluded."
  [loc]
  (->> (parser/written-child-locs (z/up loc))
       (take-while #(not (identical? (z/node %) (z/node loc))))
       count
       dec))

(defn- next-written-sibling [loc]
  (->> loc parser/right-siblings (remove #(= :uneval (z/tag %))) first))

(defn- inside-discard? [loc]
  (some #(= :uneval (z/tag %)) (take-while some? (iterate z/up loc))))

(defn- threaded-step?
  "True when list `loc` is a step of a thread-first form, so its own first argument is the second one."
  [loc]
  (let [site (heads/site loc)
        head (heads/parent-head site)
        index (argument-index site)]
    (and (contains? thread-first-heads head)
         (pos? index)
         (or (not= "cond->" head) (even? index)))))

(defn- splice-reader-macro
  "Find the `#?@` whose branch is the vector at `loc`, or nil."
  [loc]
  (let [branch (some-> loc z/up)
        reader-macro (some-> branch z/up)]
    (when (and (= :vector (some-> loc z/tag))
               (= :list (some-> branch z/tag))
               (= :reader-macro (some-> reader-macro z/tag))
               (= "?@" (some-> reader-macro z/down parser/raw)))
      reader-macro)))

(defn- written-parent
  "Find the form that `site` is written in, past any `#?@` splices, and whether a splice was crossed."
  [site]
  (loop [loc site
         spliced? false]
    (let [parent (z/up loc)]
      (if-let [reader-macro (splice-reader-macro parent)]
        (recur (heads/site reader-macro) true)
        [parent spliced?]))))

(defn- spliced-into-thread?
  "True when list `loc` is spliced by a `#?@` into a thread-first form, where its argument positions cannot be read."
  [loc]
  (let [[outer spliced?] (written-parent loc)]
    (and spliced?
         (= :list (some-> outer z/tag))
         (contains? thread-first-heads (parser/sym-name (z/down outer))))))

(defn- reads-argument-as-map?
  "True when list `loc` reads its argument at `index` as a map, not as one value it passes on."
  [loc index ns-info]
  (let [head (z/down loc)]
    (if (parser/kw-node? head)
      (zero? index)
      (let [resolved (heads/resolve-head loc ns-info (into whole-reading-heads target-reading-heads))]
        (or (contains? whole-reading-heads resolved)
            (and (some? resolved) (zero? index)))))))

(defn- step-reads-as-map?
  "True when `step`, a step of a `->`, reads the threaded value as a map: a keyword, or a call that does."
  [step ns-info]
  (or (parser/kw-node? step)
      (and (= :list (z/tag step))
           (reads-argument-as-map? step 0 ns-info))))

(defn- read-as-data?
  "True when the call at `loc` hands its result to a form that reads it as a map.

  That form is a core function changing or merging it, a key lookup on it, or a `->` whose step
  after the call is one of these. A call passed to any other function is passed on whole, and is left alone."
  [loc ns-info]
  (let [site (heads/site loc)
        [parent spliced?] (written-parent site)]
    (and (contains? #{:list :fn} (some-> parent z/tag))
         (may-read-as-map? parent)
         (let [head-name (parser/sym-name (z/down parent))]
           (or (and (contains? #{"merge" "into"} head-name)
                    (contains? whole-reading-heads (heads/resolve-head parent ns-info whole-reading-heads)))
               (and (not spliced?)
                    (not (spliced-into-thread? (heads/site parent)))
                    (let [index (cond-> (argument-index site) (threaded-step? parent) inc)]
                      (or (and (zero? index) (reads-argument-as-map? parent index ns-info))
                          (and (= "->" head-name)
                               (some? (heads/resolve-head parent ns-info #{'cljs.core/->}))
                               (step-reads-as-map? (next-written-sibling site) ns-info))))))))))

(defn- merged-class-map?
  "True when the `{:class …}` map at `loc` is an argument of a function that merges it with other props."
  [loc ns-info]
  (let [parent (z/up (heads/site loc))]
    (and (= :list (some-> parent z/tag))
         (= "merge-props" (parser/sym-name (z/down parent)))
         (contains? class-map-merging-heads (heads/resolve-head parent ns-info class-map-merging-heads)))))

(defn- declared-style-names
  "Collect the names of the styles that the file of `loc` declares, once per file."
  [loc]
  (let [state (parser/file-state ::declared-style-names nil)]
    (or @state
        (let [root (last (take-while some? (iterate z/up loc)))
              lists-in (fn [top-level]
                         (if (= :reader-macro (z/tag top-level))
                           (->> (iterate z/next (z/subzip top-level))
                                (take-while (complement z/end?))
                                (filter #(= :list (z/tag %))))
                           [top-level]))
              names (->> (parser/written-child-locs root)
                         (mapcat lists-in)
                         (filter #(and (= :list (z/tag %))
                                       (contains? blocks/style-decl-fns (parser/sym-name (z/down %)))))
                         (keep #(some-> (parser/declared-name-loc %) parser/sym-name))
                         set)]
          (reset! state names)))))

(defn- resolve-style-call
  "Resolve the head of a call to the keyword of the style it may name, through the ns form's aliases or refers.

  A refer counts only for a name that the file does not declare as a style itself."
  [loc op-raw {:keys [ns-name aliases refers]}]
  (if-let [referred-ns (when-not (contains? (declared-style-names loc) op-raw) (get refers op-raw))]
    (keyword referred-ns op-raw)
    (parser/resolve-sym op-raw ns-name aliases)))

(defn- js-object?
  "True when the map at `loc` becomes a JS object, which takes the class string, not props.

  That is a `#js` literal, or the argument of `clj->js`."
  [loc ns-info]
  (let [parent (z/up (heads/site loc))]
    (case (some-> parent z/tag)
      :reader-macro (= "js" (parser/raw (z/down parent)))
      :list (and (= "clj->js" (parser/sym-name (z/down parent)))
                 (contains? js-object-heads (heads/resolve-head parent ns-info js-object-heads)))
      false)))

(defn- class-only-map-call?
  "True when the call at `loc` is the only value of a `:class` map, and nothing changes or merges that map."
  [loc ns-info]
  (let [parent (z/up loc)
        class-map (cond
                    (class-only-map? loc) parent

                    (and parent
                         (= :vector (z/tag parent))
                         (= 1 (count (heads/code-forms (z/node parent))))
                         (class-only-map? parent))
                    (z/up parent))]
    (and class-map
         (not (js-object? class-map ns-info))
         (not (read-as-data? class-map ns-info))
         (not (merged-class-map? class-map ns-info)))))

(defn- handle-list
  "Detect style declarations and usages from list nodes.
  Handles: defclass/defattrs declarations, and catch-all style function calls."
  [loc {:keys [ns-name]
        :as ns-info} file]
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
      (when-let [resolved (resolve-style-call loc op-raw ns-info)]
        (let [context (cond
                        (inside-discard? loc)
                        :discarded

                        (and (read-as-data? loc ns-info)
                             (not (heads/locally-bound? loc op-raw)))
                        :as-data

                        (class-only-map-call? loc ns-info)
                        :class-only-map)]
          {:decls []
           :dynamics []
           :usages [{:kw resolved
                     :type :style-call
                     :file file
                     :row row
                     :context context}]})))))

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
        defattrs-as-data (for [decl style-decls
                               :when (= :defattrs (:type decl))
                               :when (some #(= :as-data (:context %))
                                           (get usages-by-kw (:kw decl)))]
                           decl)]
    {:unused-styles (parser/distinct-by :kw unused-styles)
     :defattrs-as-data (vec defattrs-as-data)
     :pseudo-in-main-map (vec pseudo-in-main-map)
     :consecutive-self-selectors (vec consecutive-self-selectors)
     :spade-ampersand-not-at-start (vec ampersand-not-at-start)
     :spade-keyword-combinator-selector (vec keyword-combinator)}))

(defn- summary-lines* [{:keys [unused-styles defattrs-as-data pseudo-in-main-map consecutive-self-selectors
                               spade-ampersand-not-at-start spade-keyword-combinator-selector]}]
  [["Unused styles:" (count unused-styles)]
   ["defattrs as data:" (count defattrs-as-data)]
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
     "Nothing calls this defclass or defattrs. Remove it, or call it where the style should apply."
     :defattrs-as-data
     (str "This defattrs is changed or read as a map, so the other attributes are hidden in it. Declare it with "
          "defclass and pass it as :class in a literal props map at every call. If no call needs props beside "
          ":class, pass the defattrs call whole instead.")
     :pseudo-in-main-map
     (str "Spade emits a pseudo-selector key inside a style map as an invalid CSS property and drops the rule. Move it into "
          "its own vector after the map, e.g. [:&:hover {...}].")
     :consecutive-self-selectors
     (str "Garden compiles [:&:before :&:after {...}] to a descendant selector, not a comma-joined one. "
          "Split it into sibling vectors [:&:before {...}] [:&:after {...}].")
     :spade-ampersand-not-at-start
     (str "Garden replaces & only at the start of a string selector; anywhere else it stays as literal text and the rule "
          "matches nothing. Move & to the front, or restructure the selector.")
     :spade-keyword-combinator-selector
     (str "Garden reads a keyword combinator like :> as its own selector in a comma list, not as a link to the next "
          "element. Use the string form, e.g. [\"> span\" {...}].")})
  (docs-anchors [_]
    {:consecutive-self-selectors "consecutive-self-selectors"
     :defattrs-as-data "defattrs-as-data"
     :pseudo-in-main-map "pseudo-selector-in-spade-main-map"
     :spade-ampersand-not-at-start "ampersand-not-at-start"
     :spade-keyword-combinator-selector "keyword-combinator-selector"
     :unused-styles "unused-styles"})
  (rule->tier [_]
    {:unused-styles :cleanup
     :defattrs-as-data :deprecations
     :pseudo-in-main-map :bugs
     :consecutive-self-selectors :bugs
     :spade-ampersand-not-at-start :bugs
     :spade-keyword-combinator-selector :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->SpadeGroup))
