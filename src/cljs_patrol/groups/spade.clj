(ns cljs-patrol.groups.spade
  "Spade rule group: detects unused CSS-in-CLJS style declarations."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private style-decl-fns #{"defclass" "defattrs"})

(defn- main-map-loc
  "Return the first argument map zloc of a defclass/defattrs list.
  Expected shape: (defclass|defattrs NAME [ARGS] {main-map} ...).
  Returns nil when the shape doesn't match."
  [list-loc]
  (let [map-loc (some-> list-loc z/down z/right z/right z/right)]
    (when (and map-loc (= :map (z/tag map-loc)))
      map-loc)))

(defn- map-key-locs
  "Return a seq of key zlocs for a map zloc, skipping values."
  [map-loc]
  (loop [loc (z/down map-loc)
         acc []]
    (if (nil? loc)
      acc
      (recur (some-> loc z/right z/right)
             (conj acc loc)))))

(defn- pseudo-selector-key?
  "True when the raw keyword text starts with `:&`."
  [kw-str]
  (str/starts-with? kw-str ":&"))

(defn- pseudo-findings
  "Scan the main map of a defclass/defattrs form for pseudo-selector keys."
  [list-loc style-kw file]
  (when-let [map-loc (main-map-loc list-loc)]
    (for [key-loc (map-key-locs map-loc)
          :when (parser/kw-node? key-loc)
          :let [selector (parser/raw key-loc)]
          :when (pseudo-selector-key? selector)]
      {:kw style-kw
       :type :pseudo-in-main-map
       :selector selector
       :form (str style-kw " " selector)
       :file file
       :row (parser/position-row key-loc)})))

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
  [loc ns-name aliases file]
  (let [op-token (z/down loc)
        operator (parser/sym-name op-token)
        op-raw (when (and op-token (= :token (z/tag op-token))) (parser/raw op-token))
        row (parser/position-row loc)]
    (cond
      (contains? style-decl-fns operator)
      (let [name-loc (z/right (z/down loc))]
        (when (and name-loc (= :token (z/tag name-loc)))
          (when-let [style-name (parser/sym-name name-loc)]
            (let [style-kw (keyword ns-name style-name)]
              {:decls (into [{:kw style-kw
                              :type (keyword operator)
                              :file file
                              :row (parser/position-row name-loc)}]
                            (pseudo-findings loc style-kw file))
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
     :pseudo-in-main-map (vec pseudo-in-main-map)}))

(defn- summary-lines* [{:keys [unused-styles defattrs-in-merge pseudo-in-main-map]}]
  [["Unused styles:" (count unused-styles)]
   ["defattrs in merge:" (count defattrs-in-merge)]
   ["Pseudo-selector in main map:" (count pseudo-in-main-map)]])

(defn- failed?* [{:keys [unused-styles pseudo-in-main-map]}]
  (or (seq unused-styles) (seq pseudo-in-main-map)))

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
     "Pseudo-selector key placed inside the main style map. Spade emits it as an invalid CSS property, so the rule is silently dropped. Move it out into its own vector, e.g. [:&:hover {...}], after the main map."})
  (rule->tier [_]
    {:unused-styles :cleanup
     :defattrs-in-merge :deprecations
     :pseudo-in-main-map :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->SpadeGroup))
