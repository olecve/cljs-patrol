(ns webapp.shadowed-names
  (:require
   [spade.core :refer [defattrs]]))

(defattrs shadow-param-attrs [] {:padding "8px"})
(defattrs shadow-let-attrs [] {:padding "8px"})
(defattrs shadow-letfn-attrs [] {:padding "8px"})

(defn whole-views []
  [:div
   [:div (shadow-param-attrs)]
   [:div (shadow-let-attrs)]
   [:div (shadow-letfn-attrs)]])

(defn param-view [shadow-param-attrs]
  [:div (assoc (shadow-param-attrs) :id 1)])

(defn let-view []
  (let [shadow-let-attrs (fn [] {:class "a"})]
    [:div {:class (:class (shadow-let-attrs))}]))

(defn letfn-view [props]
  (letfn [(shadow-letfn-attrs [] {:class "a"})]
    [:div (merge (shadow-letfn-attrs) props)]))
