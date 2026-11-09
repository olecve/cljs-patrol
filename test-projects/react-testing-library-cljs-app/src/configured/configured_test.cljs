(ns configured.configured-test
  (:require
   [cljs.test :refer [deftest]]
   [my.ui :as ui :refer [button]]
   [my.widgets :as widgets]
   [react-testing-library-cljs.screen :as s]))

(defn view [icon attrs]
  [:div
   [ui/button {:aria-label "Send"
               :data-testid "ui-send"}
    [icon]]
   [button {:data-testid "ui-save"}
    "Save"]
   [ui/button {:data-testid "ui-link"
               :href "/home"}
    "Home"]
   [ui/button {:aria-label "Close"
               :data-testid "ui-icon"
               :icon-only true}]
   [ui/button {:data-testid "ui-tab"
               :role "tab"}
    "Tab"]
   [ui/button {:data-testid "ui-hidden"
               :hidden true}
    "Hidden"]
   [ui/text-field {:aria-label "Email"
                   :data-testid "ui-email"}]
   [widgets/toggle {:aria-label "Dark mode"
                    :data-testid "ui-toggle"}]
   [ui/card {:data-testid "ui-card"}
    "Card"]
   [ui/panel {:hidden true}
    [:button {:data-testid "in-hidden-panel"}
     "Inside"]]
   [ui/panel attrs
    [:button {:data-testid "in-unreadable-panel"}
     "Unreadable"]]
   (ui/with-props {:hidden true}
     [:button {:data-testid "call-argument"}
      "Call argument"])
   [:button {:data-testid "label-icon-only"}
    [ui/label {:icon-only true}
     "Close dialog"]]
   [:button {:data-testid "label-text"}
    [ui/label "Label text"]]])

(defn role-entries [children]
  [:div
   [ui/popover-content {:aria-label "Palette"
                        :data-testid "ui-palette"}
    children]
   [ui/popover-content {:data-testid "ui-unnamed-popover"}
    children]
   [ui/tab-list {:aria-label "Search type"
                 :data-testid "ui-tab-list"}
    children]
   [ui/popover-content {:aria-label "Menu"
                        :data-testid "ui-menu-popover"
                        :role "menu"}
    children]
   [ui/tab {:data-testid "ui-tab-text"}
    "Overview"]
   [ui/tab {:data-testid "ui-tab-presentation"
            :role "presentation"}
    "Hidden role"]
   [ui/popover-content {:aria-label "Closed"
                        :data-testid "ui-hidden-popover"
                        :hidden true}
    children]
   [ui/popover-content {:aria-label "Shared"
                        :data-testid "ui-shared-popover"}
    children]
   [:div {:aria-label "Shared"
          :role "dialog"}]
   [ui/native-dialog {:aria-label "Native closed"
                      :data-testid "ui-native-closed"}
    children]
   [ui/native-dialog {:aria-label "Native open"
                      :data-testid "ui-native-open"
                      :open true}
    children]])

(defn shadowed [button]
  [button {:data-testid "shadowed-save"}
   "Shadowed"])

(deftest finds-ui-send (.click (s/get-by-test-id "ui-send")))

(deftest finds-ui-save (.click (s/get-by-test-id "ui-save")))

(deftest finds-ui-link (.click (s/get-by-test-id "ui-link")))

(deftest finds-ui-icon (.click (s/get-by-test-id "ui-icon")))

(deftest finds-ui-tab (.click (s/get-by-test-id "ui-tab")))

(deftest finds-ui-hidden (.click (s/get-by-test-id "ui-hidden")))

(deftest finds-ui-email (.click (s/get-by-test-id "ui-email")))

(deftest finds-ui-toggle (.click (s/get-by-test-id "ui-toggle")))

(deftest finds-ui-card (.click (s/get-by-test-id "ui-card")))

(deftest finds-in-hidden-panel (.click (s/get-by-test-id "in-hidden-panel")))

(deftest finds-in-unreadable-panel (.click (s/get-by-test-id "in-unreadable-panel")))

(deftest finds-call-argument (.click (s/get-by-test-id "call-argument")))

(deftest finds-label-icon-only (.click (s/get-by-test-id "label-icon-only")))

(deftest finds-label-text (.click (s/get-by-test-id "label-text")))

(deftest finds-shadowed-save (.click (s/get-by-test-id "shadowed-save")))

(deftest finds-ui-palette (.click (s/get-by-test-id "ui-palette")))

(deftest finds-ui-unnamed-popover (.click (s/get-by-test-id "ui-unnamed-popover")))

(deftest finds-ui-tab-list (.click (s/get-by-test-id "ui-tab-list")))

(deftest finds-ui-menu-popover (.click (s/get-by-test-id "ui-menu-popover")))

(deftest finds-ui-tab-text (.click (s/get-by-test-id "ui-tab-text")))

(deftest finds-ui-tab-presentation (.click (s/get-by-test-id "ui-tab-presentation")))

(deftest finds-ui-hidden-popover (.click (s/get-by-test-id "ui-hidden-popover")))

(deftest finds-ui-shared-popover (.click (s/get-by-test-id "ui-shared-popover")))

(deftest finds-ui-native-closed (.click (s/get-by-test-id "ui-native-closed")))

(deftest finds-ui-native-open (.click (s/get-by-test-id "ui-native-open")))
