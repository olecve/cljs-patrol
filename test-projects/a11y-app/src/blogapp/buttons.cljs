(ns blogapp.buttons
  (:require
   [blogapp.ui :as ui :refer [button]]))

(defn bad-icon-only-wrapper [on-close]
  [ui/button {:icon :x
              :on-click on-close
              :type :tertiary}])

(defn bad-icon-only-wrapper-refer [on-close]
  [button {:icon :x
           :on-click on-close}])

(defn ok-icon-with-aria-label [on-close]
  [ui/button {:icon :x
              :on-click on-close
              :aria-label "Close"}])

(defn ok-icon-with-visible-child [on-close]
  [ui/button {:icon :x
              :on-click on-close}
   "Close"])

(defn ok-text-only [on-click]
  [ui/button {:on-click on-click}
   "Submit"])

(defn ok-icon-and-text [on-click]
  [ui/button {:icon :x
              :on-click on-click}
   "Delete"])
