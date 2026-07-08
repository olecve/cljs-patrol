(ns blogapp.interactivity)

(defn ok-button-click [handler]
  ;; :button is natively interactive; :on-click is fine
  [:button {:on-click handler} "Save"])

(defn ok-anchor-click [handler]
  ;; :a is natively interactive
  [:a {:href "#"
       :on-click handler} "Link"])

(defn ok-div-with-role-and-keydown [handler key-handler]
  ;; explicit :role + keyboard handler — the developer opted in
  [:div {:role "button"
         :on-click handler
         :on-key-down key-handler}
   "Custom button"])

(defn ok-div-with-camelcase-onkeydown [handler key-handler]
  ;; camelCase keyboard handler also counts as an escape hatch
  [:div {:role "button"
         :onClick handler
         :onKeyDown key-handler}
   "Custom button"])

(defn ok-div-without-onclick []
  [:div {:class "card"} "Content"])

(defn ok-dynamic-attrs [attrs]
  ;; conservative: non-literal attrs — skipped
  [:div (merge {:class "card"} attrs)])

(defn bad-clickable-div [handler]
  [:div {:on-click handler} "Fake button"])

(defn bad-clickable-span [handler]
  [:span {:on-click handler} "Fake link"])

(defn bad-clickable-li [handler]
  [:li {:on-click handler
        :class "item"}
   "Selectable item"])

(defn bad-camelcase-onclick-on-section [handler]
  ;; :onClick (camelCase) matches too
  [:section {:onClick handler} "Fake"])

(defn ok-clickable-div-with-role-only [handler]
  ;; :role alone acts as an escape hatch — the developer signaled semantics.
  ;; Stricter rules could still require a keyboard handler; we don't in v1.
  [:div {:on-click handler
         :role "button"}
   "Custom button (no keydown yet)"])

(defn bad-clickable-class-shorthand [handler]
  ;; :.card is Hiccup shorthand for :div.card — treated as :div by parse-tag
  [:.card {:on-click handler} "Card"])

(defn bad-clickable-id-shorthand [handler]
  ;; :#header is Hiccup shorthand for :div#header — also :div
  [:#header {:on-click handler} "Header"])

(defn bad-role-nil-does-not-count [handler]
  ;; :role nil is not a role at runtime; can't be an escape hatch
  [:div {:on-click handler
         :role nil}
   "Fake button"])

(defn bad-role-empty-string [handler]
  [:div {:on-click handler
         :role ""}
   "Fake button"])

(defn bad-role-presentation [handler]
  ;; :role "presentation" explicitly REMOVES semantics — worse than no role
  [:div {:on-click handler
         :role "presentation"}
   "Hidden-from-AT button"])

(defn bad-role-none [handler]
  ;; :role "none" is the ARIA 1.1 synonym for "presentation"
  [:div {:on-click handler
         :role "none"}
   "Hidden-from-AT button"])

(defn bad-keyboard-handler-nil [handler]
  ;; :on-key-down nil is a no-op; not a real keyboard handler
  [:div {:on-click handler
         :on-key-down nil}
   "Fake button"])

(defn bad-mouse-down-on-div [start-drag]
  ;; :on-mouse-down has the same keyboard-inaccessibility problem as :on-click
  [:div {:on-mouse-down start-drag} "Drag handle"])

(defn bad-touch-start-on-div [tap]
  [:div {:on-touch-start tap} "Tap target"])

(defn bad-pointer-down-on-span [press]
  [:span {:on-pointer-down press} "Press"])

(defn ok-mouse-down-with-keydown [press key-handler]
  [:div {:on-mouse-down press
         :on-key-down key-handler}
   "OK"])
