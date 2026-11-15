(ns webapp.local-names
  (:require
   [webapp.attrs-styles :as styles]
   [webapp.util :as util]))

(defn local-merge-view [props]
  (let [merge (fn [a b] (into b a))]
    [:div (merge (styles/local-merge-attrs) props)]))

(defn local-assoc-view [assoc]
  [:div (assoc (styles/local-assoc-attrs) :id "x")])

(defn util-merge-view [props]
  [:div (util/merge (styles/util-merge-attrs) props)])
