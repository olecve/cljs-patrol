(ns blogapp.field-wrappers
  (:require [blogapp.ui :as ui :refer [field]]))

(defn ok-select-in-labelled-field []
  [ui/field {:label "Role"}
   [ui/select {:placeholder "Select"}]])

(defn ok-textarea-in-referred-field []
  [field {:label "Note"}
   [:textarea]])

(defn ok-field-with-computed-label [label]
  [ui/field {:label label}
   [ui/select {}]])

(defn ok-field-with-opaque-props [props]
  [ui/field props
   [ui/select {}]])

(defn ok-field-with-computed-key [label-key]
  [ui/field {label-key "Role"}
   [ui/select {}]])

(defn ok-select-through-component [options]
  [ui/field {:label "Role"}
   [ui/select-root {:options options}
    [ui/select {}]]])

(defn ok-select-in-conditional-control [show?]
  [ui/field {:label "Role"}
   (when show?
     [ui/select {}])])

(defn ok-control-after-discarded-form []
  [ui/field {:label "Role"}
   #_[:old]
   [:textarea]])

(defn ok-props-after-discarded-form []
  [ui/field #_{:old true} {:label "Role"}
   [:textarea]])

(defn ok-control-with-metadata [k]
  [ui/field {:label "Role"}
   ^{:key k} [:textarea]])

(defn ok-field-with-built-props [base]
  [ui/field (assoc base :class "field")
   [ui/select {}]])

(defn ok-control-through-unlabelled-field []
  [ui/field {:label "Outer"}
   [ui/field {}
    [:textarea]]])

(defn ok-second-control-inside-native-label []
  [:label "Notes"
   [ui/field {}
    [:span "Required"]
    [:textarea]]])

(defn bad-select-in-field-without-label []
  [ui/field {:hint-text "Pick one"}
   [ui/select {}]])

(defn bad-select-in-field-with-nil-label []
  [ui/field {:label nil}
   [ui/select {}]])

(defn bad-select-in-field-without-props []
  [ui/field
   [ui/select {}]])

(defn bad-select-after-keyword-argument []
  [ui/field :compact
   [ui/select {}]])

(defn bad-textarea-inside-div-in-field []
  [ui/field {:label "Note"}
   [:div
    [:textarea]]])

(defn bad-second-control-in-field []
  [ui/field {:label "Notes"}
   [:textarea]
   [ui/select {}]])

(defn bad-select-in-other-prop-of-field []
  [ui/field {:end-adornment [ui/select {}]
             :label "Price"}
   [:textarea]])

(defn bad-dialog-in-labelled-field []
  [ui/field {:label "Details"}
   [:dialog {:open true}]])

(defn bad-div-with-metadata-in-field [k]
  [ui/field {:label "Note"}
   ^{:key k} [:div
              [:textarea]]])

(defn bad-second-control-of-inner-field []
  [ui/field {:label "Outer"}
   [ui/field {:label "Inner"}
    [:textarea]
    [:textarea {:placeholder "Second"}]]])

(defn bad-second-control-below-layout []
  [ui/field {:label "Group"}
   [ui/row {}
    [ui/field {:label "Name"}
     [:textarea]
     [ui/select {}]]]])

(defn bad-field-with-bound-hint []
  (let [hint [:span "Pick one"]]
    [ui/field hint
     [:textarea]]))

(defn bad-field-with-bound-string []
  (let [hint "Pick one"]
    [ui/field hint
     [:textarea]]))
