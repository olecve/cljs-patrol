(ns blogapp.forms
  (:require
   [blogapp.ui :as ui :refer [textarea]]))

;; Native <textarea>. No aria-label / aria-labelledby -> flagged.
(defn bad-native-placeholder-only []
  [:textarea {:placeholder "Search materials"
              :rows 4}])

;; Native <textarea> with no props at all -> also flagged.
(defn bad-native-no-attrs []
  [:textarea])

;; Native <textarea> with :aria-label -> OK.
(defn ok-native-aria-label []
  [:textarea {:aria-label "Search materials"
              :placeholder "Search materials"
              :rows 4}])

;; Native <textarea> with :aria-labelledby -> OK.
(defn ok-native-aria-labelledby []
  [:label {:id "notes-label"} "Notes"]
  [:textarea {:aria-labelledby "notes-label"
              :rows 4}])

;; Native <textarea> with a computed aria-label -> optimistically accepted.
(defn ok-native-dynamic-aria-label [label]
  [:textarea {:aria-label label
              :rows 4}])

;; Aliased wrapper (`[ui/textarea …]`). Flagged only when the project's
;; :component-aliases config maps blogapp.ui/textarea -> :textarea.
(defn bad-wrapper-alias-placeholder-only []
  [ui/textarea {:placeholder "Search materials"
                :rows 4}])

;; Aliased wrapper with :aria-label -> OK even under config.
(defn ok-wrapper-alias-aria-label []
  [ui/textarea {:aria-label "Search materials"
                :rows 4}])

;; :refer'd wrapper (bare `textarea` symbol). Same rule as the aliased case.
(defn bad-wrapper-refer-placeholder-only []
  [textarea {:placeholder "Search materials"
             :rows 4}])

;; Modal-dialog shapes ------------------------------------------------

;; Native <div role="dialog"> with no name -> flagged.
(defn bad-native-dialog-role []
  [:div {:role "dialog"}
   [:h2 "Confirm"]])

;; Keyword form of role -> Reagent stringifies at runtime; flag.
(defn bad-native-dialog-role-keyword []
  [:div {:role :dialog}
   [:h2 "Confirm"]])

;; :aria-modal true also marks a dialog per WAI-ARIA -> flag.
(defn bad-native-aria-modal []
  [:section {:aria-modal true}
   [:h2 "Confirm"]])

;; Native <dialog> element -> flag; requires :aria-label / :aria-labelledby.
(defn bad-native-dialog-tag []
  [:dialog {:open true}])

;; Dialog with :aria-label -> OK.
(defn ok-native-dialog-with-aria-label []
  [:div {:role "dialog"
         :aria-label "Confirmation"}])

;; Dialog with :aria-labelledby -> OK.
(defn ok-native-dialog-with-aria-labelledby []
  [:div {:role "dialog"
         :aria-labelledby "confirm-title"}
   [:h2 {:id "confirm-title"} "Confirm"]])

;; Dynamic :role value -> skipped (conservative — we can't statically
;; determine whether the runtime value is "dialog").
(defn ok-native-dynamic-role [role]
  [:div {:role role}])

;; Wrapper alias case. Flagged only when :component-aliases maps
;; blogapp.ui/dialog-root -> :dialog.
(defn bad-wrapper-dialog []
  [ui/dialog-root {:open? true}])

;; Wrapper alias with :aria-label -> OK even under config.
(defn ok-wrapper-dialog-with-aria-label []
  [ui/dialog-root {:aria-label "Export"
                   :open? true}])
