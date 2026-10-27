(ns webapp.fx
  (:require
   [re-frame.core :as rf :refer [reg-fx]]
   [webapp.scroll :as scroll :refer [shared!]]))

(defn show-error! [response]
  (js/console.error response))

(rf/reg-fx ::error show-error!)

(defn track! [event]
  (js/console.log event))

(reg-fx ::track track!)

(defn- focus! [id]
  (some-> (js/document.getElementById id) .focus))

(rf/reg-fx ::focus focus!)

(defn ^:private blur! [id]
  (some-> (js/document.getElementById id) .blur))

(rf/reg-fx ::blur blur!)

(defn ^{:private true} select! [id]
  (some-> (js/document.getElementById id) .select))

(rf/reg-fx ::select select!)

(rf/reg-fx
 ::alert
 (fn [message]
   (js/alert message)))

(defn scroll-to! [id]
  (js/console.log id))

(rf/reg-fx ::scroll scroll/scroll-to!)

(defn register-log! [log!]
  (rf/reg-fx ::log log!))

(defn log! [message]
  (js/console.log message))

(rf/reg-fx ::shared-name shared!)

(comment
  (defn clear! [] (js/console.clear)))

(rf/reg-fx ::clear clear!)

(defn handle-save [_ _]
  {::track :saved})

(rf/reg-event-fx ::save handle-save)

(defn attr-private {:private true} [id]
  (js/console.log id))

(rf/reg-fx ::attr-private attr-private)

(defn doc-attr-private
  "Log an id."
  {:private true}
  [id]
  (js/console.log id))

(rf/reg-fx ::doc-attr-private doc-attr-private)

(defn notify!
  "Log a message."
  ([message] (notify! message :info))
  ([message level] (js/console.log level message)))

(rf/reg-fx ::notify notify!)

(declare reset!)

(rf/reg-fx ::reset reset!)

(defn reset! [_]
  (js/console.clear))

#_(defn dropped! [] nil)

(rf/reg-fx ::dropped dropped!)

(rf/reg-fx ::no-handler)

(rf/reg-fx ::short-fn #(js/console.log %))

(rf/reg-fx ::partial (partial notify! "x"))

(defn build-request! [id]
  {:id id})

(rf/reg-fx ::build-request build-request!)

(comment
  (ns webapp.scratch))
