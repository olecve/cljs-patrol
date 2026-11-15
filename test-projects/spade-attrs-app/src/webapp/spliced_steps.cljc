(ns webapp.spliced-steps
  (:require
   [webapp.attrs-styles :as styles]))

(defn spliced-views [props cache]
  [:div
   [:div (-> cache #?@(:cljs [(get (styles/spliced-step-attrs))]))]
   [:div (cond-> props #?@(:cljs [true (get (styles/spliced-cond-step-attrs))]))]])
