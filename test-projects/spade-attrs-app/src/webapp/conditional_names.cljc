(ns webapp.conditional-names
  (:require
   [spade.core :refer [defattrs]]
   [webapp.attrs-styles :refer [conditional-attrs]]))

#?(:cljs (defattrs conditional-attrs [] {:padding "8px"}))

(defn conditional-view [props]
  [:div (merge (conditional-attrs) props)])
