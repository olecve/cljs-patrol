(ns webapp.in-ns
  (:require
   [re-frame.core :as rf]))

(defn hello! [name]
  (js/console.log name))

(in-ns 'webapp.other)

(rf/reg-fx ::hello hello!)
