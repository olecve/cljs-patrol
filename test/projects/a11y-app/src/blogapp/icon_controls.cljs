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

(defn bad-icon-only-button-cond [toggle all-selected? some-selected?]
  ;; every branch renders an icon: a screen reader hears "button", no name
  [:button {:on-click toggle}
   (cond
     all-selected? [icons/check-square {:size 16}]
     some-selected? [icons/minus-square {:size 16}]
     :else [icons/square {:size 16}])])

(defn bad-icon-only-button-if [toggle expanded?]
  [:button {:on-click toggle}
   (if expanded?
     [icons/minus-square {:size 16}]
     [icons/square {:size 16}])])

(defn ok-icon-only-button-with-label [toggle all-selected?]
  [:button {:aria-label "Toggle selection"
            :on-click toggle}
   (cond
     all-selected? [icons/check-square {:size 16}]
     :else [icons/square {:size 16}])])

(defn ok-icon-only-button-with-state [toggle all-selected?]
  [:button {:aria-checked all-selected?
            :role "checkbox"
            :on-click toggle}
   (cond
     all-selected? [icons/check-square {:size 16}]
     :else [icons/square {:size 16}])])

(defn ok-button-with-text-and-icon [save]
  [:button {:on-click save}
   [:span "Save"]
   [icons/square {:size 16}]])

(defn ok-button-with-branch-producing-text [toggle expanded?]
  ;; one branch is a call that may render text, so the form stays opaque
  [:button {:on-click toggle}
   (if expanded?
     [icons/minus-square {:size 16}]
     (label-for expanded?))])

(defn ok-button-named-by-image-alt [on-click icon-visible?]
  ;; alt text is announced, so the image names the button
  [:button {:on-click on-click}
   [:img {:alt "Open in viewer"
          :src "/img/viewer.svg"}]
   (when icon-visible?
     [icons/square {:size 16}])])

(defn bad-icon-handler-via-assoc [base-icon-props on-select]
  ;; the handler is literal even though the base map is opaque
  [icons/square (assoc base-icon-props :on-click on-select)])

(defn bad-icon-handler-via-merge [base-icon-props on-select]
  [icons/square (merge base-icon-props {:on-click on-select})])

(defn ok-icon-given-role-and-keyboard-in-the-assoc [base-icon-props on-select]
  [icons/square (assoc base-icon-props
                       :role "button"
                       :tabIndex 0
                       :on-click on-select
                       :on-key-down on-select)])

(defn ok-icon-with-opaque-props [base-icon-props]
  ;; nothing literal to read: no construction call, so no claim either way
  [icons/square base-icon-props])
