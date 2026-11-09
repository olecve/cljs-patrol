(ns webapp.fragment-props)

(defn empty-props-before-component [accept-button]
  [:<> {}
   [:span "Saved"]
   [accept-button]])

(defn empty-props-before-when [unsaved?]
  [:<> {}
   (when unsaved?
     [:span "Unsaved"])])

(defn empty-props-before-symbol [busy-reason spinner]
  [:<> {} busy-reason [spinner]])

(defn keyed-fragment [id row item]
  [:<> {:key id} [row item]])
