(ns sepapp.layout)

(defn bad-unnamed-separator []
  [:div {:role "separator"
         :tab-index 0
         :aria-valuenow 30}])
