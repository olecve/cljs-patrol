(ns webapp.events
  (:require
   [re-frame.core :as rf]))

(rf/reg-event-db ::used-event
                 (fn [db _] db))

(rf/reg-event-db ::unused-event
                 (fn [db _] db))

(rf/dispatch [::used-event])

(rf/reg-event-fx ::multi-event
                 (fn [_ _]
                   {:dispatch-n [[::used-event] [::used-event]]}))

(rf/dispatch [::multi-event])
