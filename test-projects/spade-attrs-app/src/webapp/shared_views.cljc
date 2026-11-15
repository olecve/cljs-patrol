(ns webapp.shared-views
  (:require
   [webapp.attrs-styles :as styles]))

(defn shared-view [props]
  [:div (merge #?(:cljs (styles/reader-conditional-attrs) :clj {}) props)])

(defn shared-props-view []
  [:div #?(:cljs (styles/reader-conditional-props-attrs) :clj {})])

(defn shared-cache-view [cache]
  [:div (-> cache #?(:cljs (get (styles/reader-conditional-step-key-attrs)) :clj nil))])
