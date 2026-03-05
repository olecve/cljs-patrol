(ns webapp.events
  (:require
   [re-frame.core :as rf]))

(rf/reg-event-db ::used-event
                 (fn [db _] db))

(rf/reg-event-db ::unused-event
                 (fn [db _] db))

(rf/dispatch [::used-event])
