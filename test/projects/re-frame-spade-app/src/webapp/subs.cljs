(ns webapp.subs
  (:require
   [re-frame.core :as rf]))

(rf/reg-sub ::used-sub
            (fn [db _] (:data db)))

(rf/reg-sub ::unused-sub
            (fn [db _] (:other db)))
