(ns blogapp.icon-controls
  (:require
   [blogapp.icons :as icons]))

(defn bad-icon-as-its-own-control [on-select]
  ;; the icon is the control: not focusable, no button semantics, no name
  [icons/square {:on-click on-select}])

(defn ok-icon-without-handler []
  [icons/square {:size 16}])

(defn ok-icon-inside-a-named-button [on-select]
  [:button {:aria-label "Select row"
            :on-click on-select}
   [icons/square {:size 16}]])
