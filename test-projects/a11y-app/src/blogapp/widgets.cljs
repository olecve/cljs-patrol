(ns blogapp.widgets
  (:require [blogapp.ui :as ui :refer [switch]]))

(defn bad-switch-unnamed [on?]
  [ui/switch {:checked? on?}])

(defn bad-refer-switch-no-attrs []
  [switch])

(defn bad-select-unnamed [options]
  [ui/select {:options options}])

(defn bad-search-field-placeholder-only []
  [ui/search-field {:placeholder "Search"}])

(defn bad-stepper-unnamed []
  [ui/stepper {:min 1}])

(defn bad-slider-unnamed []
  [ui/range-slider {:max 10}])

(defn bad-switch-beside-its-label [on?]
  [:div
   [:span "Dark mode"]
   [ui/switch {:checked? on?}]])

(defn ok-switch-aria-label [on?]
  [ui/switch {:aria-label "Dark mode"
              :checked? on?}])

(defn ok-select-aria-labelledby [options]
  [ui/select {:aria-labelledby "country-label"
              :options options}])

(defn ok-search-field-dynamic-name [label]
  [ui/search-field {:aria-label label}])

(defn ok-stepper-title []
  [ui/stepper {:min 1
               :title "Quantity"}])

(defn ok-slider-dynamic-props [props]
  [ui/range-slider (merge {:max 10} props)])

(defn ok-switch-wrapped-in-label [on?]
  [:label "Dark mode" [ui/switch {:checked? on?}]])

(defn ok-switch-deep-in-label [on?]
  [:label
   [:span "Dark mode"]
   [:div [ui/switch {:checked? on?}]]])

(defn ok-select-with-id [options]
  [:div
   [:label {:for "country"} "Country"]
   [ui/select {:id "country"
               :options options}]])

(defn ok-switch-computed-id [id on?]
  [ui/switch {:checked? on?
              :id (str id "-switch")}])

(defn ok-literal-switch-tag []
  [:switch {}])

(defn ok-literal-combobox-tag []
  [:combobox])

(defn ok-textarea-wrapped-in-label []
  [:label "Notes" [:textarea {:placeholder "Notes"}]])

(defn ok-textarea-with-id []
  [:div
   [:label {:for "notes"} "Notes"]
   [:textarea {:id "notes"}]])

(defn ok-textarea-shorthand-id []
  [:textarea#notes])

(defn bad-textarea-id-nil []
  [:textarea {:id nil}])

(defn bad-dialog-in-label []
  [:label "Settings" [:dialog {:open true}]])

(defn bad-textarea-under-label-key [field]
  [field {:label "Notes"
          :input [:textarea {}]}])

(defn ok-switch-props-parameter [props]
  [ui/switch props])

(defn ok-switches-per-item [items]
  (for [item items]
    [ui/switch item]))

(defn ok-switch-deref-props [state]
  [ui/switch @state])

(defn ok-switch-positional-args [on? toggle!]
  [ui/switch on? toggle!])

(defn ok-search-field-positional-label [search!]
  [ui/search-field "Search patents" search!])

(defn ok-switch-text-child [on?]
  [ui/switch {:checked? on?} "Dark mode"])

(defn bad-select-with-children [options]
  [ui/select {:options options}
   [:span "Pick one"]])

(defn ok-switch-in-interop-label []
  [:> "label" [ui/switch {}]])

(defn bad-switch-in-interop-div []
  [:> "div" [ui/switch {}]])

(defn ok-switch-aria-hidden []
  [ui/switch {:aria-hidden true}])

(defn ok-textarea-props-parameter [props]
  [:textarea props])

(defn ok-textarea-deref-props [state]
  [:textarea @state])

(defn ok-textarea-string-keys []
  [:textarea {"aria-label" "Notes"}])

(defn bad-textarea-string-child []
  [:textarea "Draft"])

(defn bad-textarea-keyword-child []
  [:textarea :draft])

(defn bad-textarea-vector-child []
  [:textarea [:span "Draft"]])

(defn bad-dialog-with-id []
  [:dialog {:id "settings"}])

(defn bad-label-around-literal-switch []
  [:label "Dark mode" [:switch]])

(defn ok-dialog-role-aria-hidden []
  [:div {:aria-hidden true
         :role "dialog"}])
