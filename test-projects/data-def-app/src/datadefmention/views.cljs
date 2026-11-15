(ns datadefmention.views)

(def save-schema [:map [:button {:aria-label "Save" :data-testid "save-button"} :string]])

(defn toolbar [] [:div [:button {:aria-label "Save" :data-testid "save-button"}]])
