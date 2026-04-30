(ns baseline-app.events
  (:require
   [re-frame.core :as rf]))

(rf/reg-event-db ::initialize
                 (fn [db _] (assoc db :ready? true)))

(rf/reg-event-fx ::fetch-data
                 (fn [{:keys [db]} _]
                   {:db (assoc db :loading? true)}))

;; Unused - should appear in baseline
(rf/reg-event-db ::legacy-reset
                 (fn [_ _] {}))

;; Deprecated effect - should appear in baseline with file path
(rf/reg-event-fx ::batch-notify
                 (fn [{:keys [db]} [_ items]]
                   {:db db
                    :dispatch-n (mapv (fn [_] [::initialize]) items)}))
