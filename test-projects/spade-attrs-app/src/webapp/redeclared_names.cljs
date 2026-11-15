(ns webapp.redeclared-names
  (:require
   [spade.core :refer [defattrs]]
   [webapp.attrs-styles :refer [redeclared-attrs]]))

(defattrs redeclared-attrs [] {:padding "8px"})

(defn redeclared-view [props]
  [:div (merge (redeclared-attrs) props)])
