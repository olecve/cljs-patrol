(ns sharedids.button-row)

(defn button-row []
  [:div
   [:button {:data-testid "save"
             :type :button}
    "Save"]])
