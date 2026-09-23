(ns webapp.into-hiccup
  (:require
   [reagent.core :as r]
   [webapp.pseudo-styles :as pseudo-styles]))

(defonce expanded? (r/atom false))

(defn redundant-into-keyword-view [items]
  (into [:ul {:class "list"}]
        (for [x items]
          ^{:key x} [:li x])))

(defn redundant-into-bare-symbol-view [cards card-body]
  (into [card-body]
        (for [c cards]
          ^{:key (:id c)} [:div (:title c)])))

(defn redundant-into-namespaced-view [items]
  (into [pseudo-styles/panel-style {:aria-label "Group"}]
        (for [x items]
          ^{:key x} [:div x])))

(defn redundant-into-map-view [items]
  (into [:<>]
        (map (fn [x]
               ^{:key x} [:span x])
             items)))

(defn redundant-into-with-meta-view [segments]
  (into [:p]
        (map-indexed (fn [idx segment]
                       (with-meta [:em segment] {:key (str "segment-" idx)}))
                     segments)))

(defn redundant-into-props-key-view [rows]
  (into [:tbody]
        (keep (fn [row]
                (when (:visible? row)
                  [:tr {:key (:id row)} (:label row)]))
              rows)))

(defn redundant-into-cond-view [rows]
  (into [:tbody]
        (map (fn [row]
               (cond
                 (:header? row) ^{:key (:id row)} [:tr.header (:label row)]
                 (:total? row) (with-meta [:tr.total (:label row)] {:key (:id row)})
                 :else [:tr {:key (:id row)} (:label row)]))
             rows)))

(defn ok-into-partially-keyed-view [rows]
  (into [:tbody]
        (map (fn [row]
               (if (:special? row)
                 ^{:key (:id row)} [:tr.special (:label row)]
                 [:tr (:label row)]))
             rows)))

(defn ok-into-keyed-inner-element-view [rows]
  (into [:tbody]
        (map (fn [row]
               [:tr [:td {:key (:id row)} (:label row)]])
             rows)))

(defn ok-into-keyless-transducer [nodes node->hiccup]
  (into [:<>] (map node->hiccup) nodes))

(defn ok-into-keyless-for [items]
  (into [:ol]
        (for [x items]
          [:li x])))

(defn ok-into-malli-enum [tab-sections]
  (into [:enum] tab-sections))

(defn ok-into-malli-map [entries]
  (into [:map {:closed true}] entries))

(defn ok-into-db-path [ks]
  (into [:cart] ks))

(defn ok-into-spliced-argument [content content-key]
  (into [:div {:class "panel"
               :key content-key}] content))

(defn ok-into-deref-in-body [segments]
  (doall
   (into [:span]
         (map-indexed (fn [idx segment]
                        ^{:key idx} [:em (when @expanded? "* ") segment])
                      segments))))

(defn ok-into-empty [xs]
  (into [] (map inc xs)))

(defn ok-into-literal-head [more]
  (into [1 2 3] more))

(defn ok-into-arity-1 []
  (into [:span]))

(defn ok-into-keyword-path [ks]
  (into [:cart :items] ks))
