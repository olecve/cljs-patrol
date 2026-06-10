(ns webapp.events
  (:require
   [re-frame.core :as rf]))

(rf/reg-event-db
 ::cart-add-success-bug
 (fn [{:keys [db]} [_ item]]
   {:db (-> db
            (update :cart-items conj item)
            (assoc :loading? false))
    :dispatch [:analytics/track :item-added]}))

(rf/reg-event-db
 ::with-fx-key-bug
 (fn [{:keys [db]} _]
   {:db (assoc db :loading? false)
    :fx [[:dispatch [::another-event]]]}))

(rf/reg-event-db
 ::let-wrapped-bug
 (fn [{:keys [db]} [_ payload]]
   (let [new-db (assoc db :payload payload)]
     {:db new-db
      :dispatch [::after]})))

(rf/reg-event-db
 ::with-interceptors-bug
 [rf/trim-v]
 (fn [{:keys [db]} [payload]]
   {:db (assoc db :payload payload)
    ::side-effect true}))

(rf/reg-event-db
 ::if-then-bug
 (fn [{:keys [db]} [_ ok?]]
   (if ok?
     {:db (assoc db :status :ok)
      :dispatch [::after]}
     (assoc db :status :error))))

(rf/reg-event-db
 ::plain-db-update
 (fn [db _]
   (assoc db :counter 0)))

(rf/reg-event-db
 ::reduce-over-accumulator
 (fn [db [_ items]]
   (reduce (fn [db item] (assoc-in db [:items (:id item)] item))
           db
           items)))

(rf/reg-event-fx
 ::correct-event-fx
 (fn [{:keys [db]} [_ item]]
   {:db (update db :cart-items conj item)
    :dispatch [:analytics/track :item-added]}))

(rf/reg-event-fx
 ::correct-fx-with-reduce
 (fn [{:keys [db]} [_ items]]
   {:db (reduce (fn [db item] (assoc-in db [:items (:id item)] item))
                db
                items)
    :dispatch [:analytics/track :items-imported]}))
