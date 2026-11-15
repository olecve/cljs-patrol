(ns webapp.spliced-steps
  (:require
   [webapp.attrs-styles :as styles]))

(defn spliced-views [props cache]
  [:div
   [:div (-> cache #?@(:cljs [(get (styles/spliced-step-attrs))]))]
   [:div (cond-> props #?@(:cljs [true (get (styles/spliced-cond-step-attrs))]))]
   [:div (-> cache #?@(:cljs [#?@(:cljs [(get (styles/nested-spliced-step-attrs))])]))]
   [:div (cond-> props #?@(:cljs [(:hidden (styles/spliced-cond-test-attrs)) (assoc :id 1)]))]
   [:div (-> props #?@(:cljs [(merge (styles/spliced-merge-step-attrs))]))]
   [:div (merge #?@(:cljs [(styles/spliced-merge-argument-attrs)]) props)]
   [:div (get #?@(:cljs [cache (styles/spliced-key-attrs)]))]
   (into [:div] #?(:cljs [(styles/conditional-element-attrs)]))])
