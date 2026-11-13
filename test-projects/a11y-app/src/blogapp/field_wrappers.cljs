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
