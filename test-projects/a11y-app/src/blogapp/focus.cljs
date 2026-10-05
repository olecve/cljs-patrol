(ns blogapp.focus)

(defn ok-tabindex-zero []
  [:div {:tabIndex 0}])

(defn ok-tabindex-negative []
  [:div {:tabIndex -1}])

(defn ok-tab-index-kebab-zero []
  [:div {:tab-index 0}])

(defn ok-dynamic-tabindex [n]
  ;; conservative: non-literal value — skipped
  [:div {:tabIndex n}])

(defn bad-positive-tabindex []
  [:div {:tabIndex 1}])

(defn bad-large-positive-tabindex []
  [:button {:tabIndex 100} "Save"])

(defn bad-string-tabindex []
  [:div {:tabIndex "1"}])

(defn bad-float-tabindex []
  [:div {:tabIndex 1.5}])

(defn bad-keyword-tabindex []
  [:div {:tabIndex :something}])

(defn bad-kebab-positive-tabindex []
  ;; kebab-case :tab-index also matches
  [:div {:tab-index 5}])
