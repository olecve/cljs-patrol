(ns webapp.views)

(def polygon
  {:coordinates [{:x 0
                  :y 0} {:x 10
                         :y 0} {:x 10
                                :y 10}]})

(def merged
  (merge {:a 1
          :b 2} {:c 3
                 :d 4}))

(defn add-button [on-click]
  [button {:size :s
           :on-click on-click} [icon {:name :plus
                                      :size :s}]])

(defn edit-button [on-click]
  [button {:size :s
           :on-click on-click} "Edit"])

(defn handler [{:keys [db]} {:keys [id]
                             :as profile}]
  (write-source-note {:db db} {:draft-id id
                               :source profile}))

(defn event-handler [cofx [_ {:keys [id]
                              :as user-group}]]
  (assoc-in cofx [:db :group] {:id id
                               :group user-group}))

(defn tidy [{:keys [db]}
            {:keys [id]
             :as profile}]
  [{:x 0
    :y 0}
   {:x 10
    :y 0}
   (str "{:a 1} {:b" id) ; {:a 1} {:b
   #"\{a\} \{"
   db
   profile])
