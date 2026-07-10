(ns webapp.views
  (:require
   [re-frame.core :as rf]
   [webapp.pseudo-styles :as pseudo-styles]
   [webapp.styles :as styles]
   [webapp.subs :as subs]))

(defn main-view []
  (let [data @(rf/subscribe [::subs/used-sub])]
    [:div {:class (styles/container-style)
           :id "main"} data]))

;; Subscribes to a sub that is never declared (phantom)
(defn other-view []
  @(rf/subscribe [:webapp.phantom/ghost-sub]))

;; defattrs used inside merge — should be defclass
(defn merged-view []
  [:div (merge (styles/merged-attrs) {:on-click #(js/alert "hi")})])

;; defclass used as sole attr in map — could be defattrs
(defn sole-attr-view []
  [:div {:class (styles/sole-attr-style)}])

;; defclass in single-element :class vector — could be defattrs
(defn vector-sole-attr-view []
  [:div {:class [(styles/vector-sole-attr-style)]}])

;; defclass in multi-element :class vector — NOT flagged
(defn vector-multi-class-view []
  [:div {:class [(styles/vector-multi-class-style) "extra-class"]}])

;; Consumes the pseudo-styles fixture so those styles are not flagged as unused
;; and do not trip other Spade/Reagent rules unrelated to the pseudo check.
(defn pseudo-styles-view []
  [:section (pseudo-styles/card-section-attrs)
   [:a {:class [(pseudo-styles/menu-item-style) "extra"]} "menu"]
   [:button {:class [(pseudo-styles/tab-style) "extra"]} "tab"]
   [:button {:class [(pseudo-styles/icon-button-style) "extra"]} "icon"]
   [:mark (pseudo-styles/badge-marker-attrs) "badge"]
   [:div {:class [(pseudo-styles/callout-style) "extra"]} "callout"]
   [:div {:class [(pseudo-styles/panel-style) "extra"]} "panel"]])
