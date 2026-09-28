(ns blogapp.containers)

(defn ok-listbox-with-label [tags]
  [:ul {:role "listbox"
        :aria-label "Tags"}
   (for [tag tags]
     ^{:key tag} [:li {:role "option"} tag])])

(defn ok-grid-with-labelledby []
  [:div {:role "grid"
         :aria-labelledby "stats-caption"}])

(defn ok-tree-with-title []
  [:div {:role "tree"
         :title "Categories"}])

(defn ok-alertdialog-with-label []
  [:div {:role "alertdialog"
         :aria-label "Delete post"}])

(defn ok-tablist-unnamed []
  ;; the spec does not require a name on tablist, so this is not reported
  [:div {:role "tablist"}])

(defn ok-menu-unnamed []
  ;; likewise for menu and menubar
  [:div {:role "menu"}])

(defn ok-dynamic-container-role [role]
  ;; conservative: non-literal role — skipped
  [:div {:role role}])

(defn bad-listbox-unnamed [tags]
  [:ul {:role "listbox"}
   (for [tag tags]
     ^{:key tag} [:li {:role "option"} tag])])

(defn bad-grid-unnamed []
  [:div {:role "grid"}])

(defn bad-tree-unnamed []
  [:div {:role "tree"}])

(defn bad-alertdialog-unnamed []
  [:div {:role "alertdialog"}])

(defn bad-listbox-keyword-spelling []
  [:ul {:role :listbox}])

(defn bad-listbox-empty-label []
  [:ul {:role "listbox"
        :aria-label ""}])
