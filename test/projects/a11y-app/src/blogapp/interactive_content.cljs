(ns blogapp.interactive-content)

(defn ok-button-with-text []
  [:button {:on-click :save} "Save"])

(defn ok-button-with-child-vector []
  [:button {:on-click :save} [:span "Save"]])

(defn ok-anchor-with-text []
  [:a {:href "/"} "Home"])

(defn ok-icon-button-with-aria-label [handler]
  [:button {:on-click handler
            :aria-label "Close dialog"}
   [:svg]])

(defn ok-icon-anchor-with-title []
  [:a {:href "/settings"
       :title "Settings"}
   [:svg]])

(defn ok-anchor-with-aria-labelledby []
  [:a {:href "/x"
       :aria-labelledby "menu-heading"}
   [:svg]])

(defn ok-button-with-dynamic-child [content]
  ;; conservative: dynamic body — could expand to text at runtime
  [:button {:on-click :save} content])

(defn bad-empty-button []
  [:button])

(defn bad-empty-button-attrs []
  [:button {:on-click :nothing}])

(defn bad-empty-anchor []
  [:a {:href "/"}])

(defn bad-empty-anchor-no-attrs []
  [:a])

(defn bad-button-empty-aria-label [handler]
  ;; :aria-label "" doesn't provide a name — screen readers still announce nothing
  [:button {:on-click handler
            :aria-label ""}])

(defn bad-anchor-nil-aria-label []
  ;; :aria-label nil doesn't provide a name either
  [:a {:href "/"
       :aria-label nil}])
