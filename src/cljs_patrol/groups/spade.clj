(ns cljs-patrol.groups.spade
  "Spade rule group: detects unused CSS-in-CLJS style declarations."
  (:require
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporters.console :as reporter]
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

(defn analyze
  "Compute spade dead code from parsed declarations and usages."
  [{:keys [declarations usages]}]
  (let [style-decls (filter #(contains? #{:defclass :defattrs} (:type %)) declarations)
        style-call-kws (set (map :kw (filter #(= :style-call (:type %)) usages)))
        unused-styles (remove #(contains? style-call-kws (:kw %)) style-decls)]
    {:unused-styles (parser/distinct-by :kw unused-styles)}))

(defn report
  "Print the spade analysis report sections."
  [{:keys [unused-styles]}]
  (reporter/print-section "UNUSED STYLES (defclass/defattrs)" unused-styles))

(defn summary-lines
  "Return [[label count] ...] for the summary section."
  [{:keys [unused-styles]}]
  [["Unused styles:" (count unused-styles)]])

(defn failed?
  "Return true if there are unused style declarations."
  [{:keys [unused-styles]}]
  (seq unused-styles))

(def group
  "Spade rule group map."
  {:id :spade
   :name "Spade"
   :parse {:handle-list handle-list}
   :analyze analyze
   :report report
   :summary-lines summary-lines
   :failed? failed?
   :suggestions
   {:unused-styles
    "Declared with defclass or defattrs but never called. Remove the declaration, or add a call site where the style should be applied."}
   :html-sections [{:title "Unused Styles"
                    :description "Declared with defclass or defattrs but never called."
                    :data-fn :unused-styles
                    :columns [:keyword :file :line]}]})
