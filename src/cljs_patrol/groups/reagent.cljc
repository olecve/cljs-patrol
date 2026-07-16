(ns cljs-patrol.groups.reagent
  "Reagent rule group: detects suboptimal patterns in Reagent hiccup templates."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(= :defclass (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        usages-by-kw (group-by :kw style-calls)
        defclass-as-sole-attr (for [decl style-decls
                                    :let [uses (get usages-by-kw (:kw decl))]
                                    :when (seq uses)
                                    :when (every? #(= :class-only-map (:context %)) uses)]
                                decl)]
    {:defclass-as-sole-attr (vec defclass-as-sole-attr)}))

(defn- summary-lines* [{:keys [defclass-as-sole-attr]}]
  [["defclass as sole attr:" (count defclass-as-sole-attr)]])

(defn- failed?* [_] false)

(defrecord ReagentGroup []
  group/RuleGroup
  (group-id [_] :reagent)
  (group-name [_] "Reagent")
  (parse-handlers [_] (group/parse-handlers spade/group))
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:defclass-as-sole-attr
     "Declared with defclass but every usage is {:class (style-fn)}. Use defattrs instead to avoid the :class wrapper."})
  (rule->tier [_]
    {:defclass-as-sole-attr :deprecations})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReagentGroup))
