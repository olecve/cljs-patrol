(ns datadefbuilt.views)

(def section-schema [:map [:div {:data-testid (str "save-" suffix)} :string]])

(def quoted-section-schema [:map [:div {:data-testid 'save-button} :string]])

(defn toolbar [] [:div [:button {:aria-label "Save" :data-testid "save-button"}]])
