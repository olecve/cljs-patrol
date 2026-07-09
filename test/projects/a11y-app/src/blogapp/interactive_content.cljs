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

(defn ok-div-role-button-with-text []
  [:div {:role "button"
         :on-click :save} "Save"])

(defn ok-span-role-link-with-aria-label []
  [:span {:role "link"
          :on-click :navigate
          :aria-label "Home"}])

(defn ok-div-role-presentation-empty []
  ;; role=presentation removes semantics — not an interactive element
  [:div {:role "presentation"}])

(defn ok-div-role-dynamic-empty [role]
  ;; non-literal role — skip conservatively
  [:div {:role role}])

(defn bad-empty-div-role-button []
  [:div {:role "button"
         :on-click :save
         :on-key-down :save}])

(defn bad-empty-span-role-link []
  [:span {:role "link"
          :on-click :navigate
          :on-key-down :navigate}])

(defn bad-empty-div-role-button-empty-aria-label []
  ;; empty :aria-label doesn't confer a name
  [:div {:role "button"
         :on-click :save
         :on-key-down :save
         :aria-label ""}])

(defn bad-empty-div-role-kw-button []
  ;; keyword form of role is also valid at runtime (Reagent stringifies it)
  [:div {:role :button
         :on-click :save
         :on-key-down :save}])

(defn ok-div-role-kw-link-with-text []
  [:div {:role :link
         :on-click :navigate} "Go home"])

(defn bad-icon-only-button [handler]
  ;; body is a nested icon vector with no text — screen readers announce "button"
  [:button {:on-click handler}
   [:svg]])

(defn bad-icon-only-anchor []
  [:a {:href "/settings"}
   [:svg]])

(defn bad-nested-icon-only-button []
  ;; even one level deeper: [:span [icons/x]] is still just an icon
  [:button {:on-click :save}
   [:span [:svg]]])

(defn ok-button-icon-with-own-aria-label [handler]
  ;; icon child carries its own aria-label — accessible name flows up
  [:button {:on-click handler}
   [:svg {:aria-label "Close dialog"}]])

(defn ok-button-with-text-and-icon [handler]
  [:button {:on-click handler}
   [:svg]
   "Close"])
