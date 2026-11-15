(ns webapp.referred-styles
  (:require
   [webapp.attrs-styles :refer [referred-merge-attrs]]))

(defn referred-merge-view [props]
  [:div (merge (referred-merge-attrs) props)])
