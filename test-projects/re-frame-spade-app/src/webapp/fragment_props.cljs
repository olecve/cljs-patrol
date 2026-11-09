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

(defn single-element-fragment [items]
  [:<>
   [:ul {:class "list"}
    (for [x items]
      ^{:key x} [:li x])]])

(defn fragment-a-branch-yields [searching? panel]
  (if searching?
    [:<> [panel {:mode :search}]]
    [panel {:mode :empty}]))

(defn fragment-as-data [header rows]
  (into [:<> [header]] rows))
