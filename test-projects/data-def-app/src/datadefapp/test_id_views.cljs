(ns datadefapp.test-id-views)

(def save-schema [:map [:button {:aria-label "Save"} :string]])

(def open-markup [:button {:aria-label "Open"} "Open"])

(defn toolbar []
  [:div
   [:button {:aria-label "Save"
             :data-testid "save-button"}]
   [:button {:aria-label "Open"
             :data-testid "open-button"}]])
