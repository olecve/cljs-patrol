(ns webapp.referred-names
  (:require
   [webapp.attrs-styles :as styles]
   [webapp.util :refer [assoc]]))

(defn referred-assoc-view []
  [:div (assoc (styles/referred-assoc-attrs) :id "x")])
