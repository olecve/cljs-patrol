(ns webapp.views
  (:require
   [re-frame.core :as rf]
   [webapp.styles :as styles]
   [webapp.subs :as subs]))

(defn main-view []
  (let [data @(rf/subscribe [::subs/used-sub])]
    [:div {:class (styles/container-style)} data]))

;; Subscribes to a sub that is never declared (phantom)
(defn other-view []
  @(rf/subscribe [:webapp.phantom/ghost-sub]))
