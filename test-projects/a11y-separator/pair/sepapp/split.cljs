(ns sepapp.split)

(defn bad-first-separator []
  [:div {:role "separator"
         :tab-index 0}])

(defn bad-second-separator []
  [:hr {:tab-index 0}])
