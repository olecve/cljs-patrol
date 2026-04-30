(ns baseline-app.views
  (:require
   [baseline-app.events :as events]
   [baseline-app.styles :as styles]
   [baseline-app.subs :as subs]
   [re-frame.core :as rf]))

(defn header []
  (let [users @(rf/subscribe [::subs/active-users])]
    [:div {:class (styles/container)}
     [:h1 "Users: " (count users)]]))

(defn settings-page []
  (let [settings @(rf/subscribe [::subs/settings])]
    [:div (styles/button-attrs)
     [:p "Settings: " (pr-str settings)]]))

(defn actions []
  [:button {:on-click #(rf/dispatch [::events/initialize])}
   "Init"]
  [:button {:on-click #(rf/dispatch [::events/fetch-data])}
   "Fetch"])

;; Phantom - subscribes to something never declared
(defn ghost-widget []
  (let [data @(rf/subscribe [::subs/deleted-feature])]
    [:span (str data)]))
