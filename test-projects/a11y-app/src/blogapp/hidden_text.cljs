(ns blogapp.hidden-text
  (:require
   [blogapp.icons :as icons]
   [blogapp.ui :as ui]))

(defn bad-button-holding-only-hidden-text [on-click]
  [:button {:on-click on-click
            :type "button"}
   [:span {:aria-hidden true} "Save"]])

(defn bad-button-holding-text-hidden-with-a-string [on-click]
  [:button {:on-click on-click
            :type "button"}
   [:span {:aria-hidden "true"} "Save"]])

(defn- hidden-label []
  [:span {:aria-hidden true} "Save"])

(defn bad-button-holding-a-component-rendering-hidden-text [on-click]
  [:button {:on-click on-click
            :type "button"}
   [hidden-label]])

(defn ok-button-with-hidden-icon-beside-text [on-click]
  [:button {:on-click on-click
            :type "button"}
   [:span {:aria-hidden true} [icons/square {:size 16}]]
   "Save"])

(defn ok-button-with-text-hidden-only-when-collapsed [on-click hidden?]
  [:button {:on-click on-click
            :type "button"}
   [:span {:aria-hidden hidden?} "Save"]])

(defn ok-button-holding-a-component-given-aria-hidden [on-click]
  [:button {:on-click on-click
            :type "button"}
   [ui/row {:aria-hidden true} "Save"]])

(defn ok-button-holding-a-keyword-lookup [on-click]
  [:button {:on-click on-click
            :type "button"}
   (:title {:aria-hidden true
            :title "Save"})])
