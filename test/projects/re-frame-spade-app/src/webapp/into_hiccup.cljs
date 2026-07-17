(ns webapp.into-hiccup
  (:require
   [webapp.pseudo-styles :as pseudo-styles]))

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

(defn ok-into-empty [xs]
  (into [] (map inc xs)))

(defn ok-into-literal-head [more]
  (into [1 2 3] more))

(defn ok-into-arity-1 []
  (into [:span]))

(defn ok-into-keyword-path [ks]
  (into [:cart :items] ks))
