(ns blogapp.field-wrappers
  (:require [blogapp.ui :as ui :refer [field]]))

(defn ok-select-in-labelled-field []
  [ui/field {:label "Role"}
   [ui/select {:placeholder "Select"}]])

(defn ok-textarea-deep-in-referred-field []
  [field {:label "Note"}
   [:div
    [:textarea]]])

(defn ok-field-with-computed-label [label]
  [ui/field {:label label}
   [ui/select {}]])

(defn ok-field-with-opaque-props [props]
  [ui/field props
   [ui/select {}]])

(defn bad-select-in-field-without-label []
  [ui/field {:hint-text "Pick one"}
   [ui/select {}]])

(defn bad-select-in-field-with-nil-label []
  [ui/field {:label nil}
   [ui/select {}]])

(defn bad-dialog-in-labelled-field []
  [ui/field {:label "Details"}
   [:dialog {:open true}]])
