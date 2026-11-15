(ns blogapp.built-attrs)

(defn base-attrs [] {:class "card"})

(defn bad-built-positive-tabindex []
  [:div (assoc (base-attrs) :tab-index 1)
   "Card"])

(defn bad-built-positive-tabindex-merged-last []
  [:div (merge (base-attrs) {:tab-index 1})
   "Card"])

(defn ok-built-positive-tabindex-before-base []
  [:div (merge {:tab-index 1} (base-attrs))
   "Card"])

(defn ok-built-positive-tabindex-before-computed-key [k v]
  [:div (assoc (base-attrs) :tab-index 1 k v)
   "Card"])

(defn bad-built-alert-downgraded-to-polite []
  [:div (assoc (base-attrs) :role "alert" :aria-live "polite")
   "Saved"])

(defn ok-built-aria-live-without-role []
  [:div (assoc (base-attrs) :aria-live "polite")
   "Saved"])

(defn ok-built-alert-before-base []
  [:div (merge {:role "alert" :aria-live "polite"} (base-attrs))
   "Saved"])

(defn bad-built-hidden-wrapper [on-close]
  [:span (assoc (base-attrs) :aria-hidden true)
   [:button {:on-click on-close
             :type "button"}
    "Close"]])

(defn ok-built-hidden-wrapper-before-base [on-close]
  [:span (merge {:aria-hidden true} (base-attrs))
   [:button {:on-click on-close
             :type "button"}
    "Close"]])

(defn ok-built-hidden-self [on-close]
  [:button (assoc (base-attrs) :aria-hidden true :on-click on-close :type "button")
   "Close"])
