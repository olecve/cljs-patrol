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

(defn ok-grid-table-named-by-caption []
  ;; HTML-AAM names a <table> from its caption
  [:table {:role "grid"}
   [:caption "Quarterly stats"]
   [:tbody]])

(defn bad-grid-empty-child []
  ;; an empty child vector has no head to read; the scan must survive it
  [:table {:role "grid"}
   []
   [:tbody]])

(defn ok-caption-carrying-metadata []
  [:table {:role "grid"}
   ^{:key 1} [:caption "Quarterly stats"]
   [:tbody]])

(defn bad-caption-under-dialog []
  ;; nothing but a <table> is named by a caption
  [:div {:role "dialog"}
   [:caption "Not a name for a dialog"]])

(defn bad-empty-caption []
  [:table {:role "grid"}
   [:caption ""]
   [:tbody]])
