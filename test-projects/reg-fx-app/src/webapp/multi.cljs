(ns webapp.multi-a
  (:require
   [re-frame.core :as rf]))

(defn shared [value]
  (js/console.log value))

(ns webapp.multi-b
  (:require
   [re-frame.core :as rf]
   [webapp.scroll :refer [shared]]))

(rf/reg-fx ::shared shared)
