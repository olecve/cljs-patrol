(ns baseline-app.subs
  (:require
   [re-frame.core :as rf]))

(rf/reg-sub ::active-users
            (fn [db _] (:active-users db)))

(rf/reg-sub ::settings
            (fn [db _] (:settings db)))

;; Unused - should appear in baseline
(rf/reg-sub ::old-dashboard
            (fn [db _] (:dashboard db)))

;; Wrong sugar: :=> passes (signal-value query-vector) but `last` is 1-arity.
;; Should be :-> instead.
(rf/reg-sub ::latest-active-user
            :<- [::active-users]
            :=> last)
