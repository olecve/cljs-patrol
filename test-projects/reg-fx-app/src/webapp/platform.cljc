(ns webapp.platform
  (:require
   [re-frame.core :as rf]))

#?(:cljs (defn conditional-defn! [id] (js/console.log id)))

(rf/reg-fx ::conditional-defn conditional-defn!)

(defn conditional-reg! [id]
  (println id))

#?(:cljs (rf/reg-fx ::conditional-reg conditional-reg!))

(defn plain! [id]
  (println id))

(rf/reg-fx ::plain plain!)
