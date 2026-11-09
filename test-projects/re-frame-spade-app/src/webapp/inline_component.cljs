(ns webapp.inline-component)

(defn settings-form [{:keys [on-cancel on-save]} button fields]
  (let [actions [:div {:class "action-row"}
                 [button {:on-click on-save}
                  "Save"]
                 [button {:on-click on-cancel}
                  "Cancel"]]]
    [:form
     [fields]
     actions]))

(defn run-button [{:keys [busy? on-run]} button tooltip]
  (let [launcher [button {:disabled busy?
                          :on-click on-run}
                  "Run"]]
    (if busy?
      [tooltip launcher]
      launcher)))
