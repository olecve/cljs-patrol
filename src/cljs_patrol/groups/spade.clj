(ns cljs-patrol.groups.spade
  "Spade rule group: detects unused CSS-in-CLJS style declarations."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [rewrite-clj.zip :as z]))

(def ^:private style-decl-fns #{"defclass" "defattrs"})

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
            {:decls [{:kw (keyword ns-name style-name)
                      :type (keyword operator)
                      :file file
                      :row (parser/position-row name-loc)}]
             :usages [] :dynamics []})))

      operator
      (when-let [resolved (parser/resolve-sym op-raw ns-name aliases)]
        {:decls [] :dynamics []
         :usages [{:kw resolved :type :style-call :file file :row row}]})

      :else nil)))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(contains? #{:defclass :defattrs} (:type %)) declarations)
        style-call-kws (set (map :kw (filter #(= :style-call (:type %)) usages)))
        unused-styles (remove #(contains? style-call-kws (:kw %)) style-decls)]
    {:unused-styles (parser/distinct-by :kw unused-styles)}))

(defn- summary-lines* [{:keys [unused-styles]}]
  [["Unused styles:" (count unused-styles)]])

(defn- failed?* [{:keys [unused-styles]}]
  (seq unused-styles))

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
     "Declared with defclass or defattrs but never called. Remove the declaration, or add a call site where the style should be applied."}))

(def group (->SpadeGroup))
