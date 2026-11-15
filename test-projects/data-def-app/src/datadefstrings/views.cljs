(ns datadefstrings.views)

(def save-id-schema [:enum "save-button" "cancel-button"])

(def props-schema [:map [:test-id {:default "open-button"} :string]])

(def close-props-schema [:vector {:props {:data-testid "close-button"}} :int])

(def edit-id-schema [:tuple [:enum :data-testid "edit-button"]])

(defn toolbar []
  [:div
   [:button {:aria-label "Save"
             :data-testid "save-button"}]
   [:button {:aria-label "Open"
             :data-testid "open-button"}]
   [:button {:aria-label "Close"
             :data-testid "close-button"}]
   [:button {:aria-label "Edit"
             :data-testid "edit-button"}]])
