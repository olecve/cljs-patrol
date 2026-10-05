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

;; `let`-bound map literal carrying the name -> resolved, OK.
(defn ok-let-bound-props []
  (let [dialog-props {:aria-label "Export"
                      :open true}]
    [:dialog dialog-props
     [:p "Pick a format."]]))

;; Same binding reused by two dialogs -> both resolved, both OK.
(defn ok-let-bound-props-reused []
  (let [dialog-props {:aria-label "Export"
                      :open true}]
    [:div
     [:dialog dialog-props [:p "Pick a format."]]
     [:dialog dialog-props [:p "Nothing to export."]]]))

;; `let`-bound map literal without a name -> flagged, as if written inline.
(defn bad-let-bound-props []
  (let [dialog-props {:open true}]
    [:dialog dialog-props
     [:p "Pick a format."]]))

;; `when-let` and `if-let` bind the same way.
(defn ok-when-let-bound-props []
  (when-let [dialog-props {:aria-label "Export"}]
    [:dialog dialog-props]))

(defn ok-if-let-bound-props []
  (if-let [dialog-props {:aria-label "Export"}]
    [:dialog dialog-props]
    [:p "Loading"]))

;; Inner binding shadows the unnamed outer one -> the inner map answers, OK.
(defn ok-shadowing-let-bound-props []
  (let [dialog-props {:open true}]
    [:div
     (let [dialog-props {:aria-label "Export"
                         :open true}]
       [:dialog dialog-props])]))

;; Bound to a call -> unreadable, flagged as before.
(defn bad-let-bound-call []
  (let [dialog-props (build-dialog-props)]
    [:dialog dialog-props]))

;; Bound to another symbol -> unreadable, flagged as before.
(defn bad-let-bound-symbol [incoming]
  (let [dialog-props incoming]
    [:dialog dialog-props]))

;; A parameter of the same name shadows the outer binding -> flagged.
(defn bad-parameter-shadows-let-bound-props []
  (let [dialog-props {:aria-label "Export"}]
    (fn [dialog-props]
      [:dialog dialog-props])))

;; Bound to a map-building call -> the keys the call names are read, exactly as
;; they are when the same call is written in the slot.
(defn ok-let-bound-construction []
  (let [dialog-props (merge base-props {:aria-label "Export"})]
    [:dialog dialog-props]))

;; Bound to a call naming no key of its own -> an unknown map, not an empty one.
(defn ok-let-bound-opaque-merge [opts]
  (let [dialog-props (merge base-props opts)]
    [:dialog dialog-props]))

(def export-dialog-props
  {:aria-label "Export"
   :open true})

;; A def in this file names the map as plainly as a let does.
(defn ok-def-bound-props []
  [:dialog export-dialog-props
   [:p "Pick a format."]])

(def unnamed-dialog-props {:open true})

(defn bad-def-bound-props []
  [:dialog unnamed-dialog-props
   [:p "Pick a format."]])
