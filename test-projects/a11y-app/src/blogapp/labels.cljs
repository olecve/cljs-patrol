(ns blogapp.labels
  (:require
   [blogapp.styles :as styles]))

(defn ok-label-with-for []
  [:label {:for "post-title"} "Title"])

(defn ok-label-camel-case-for []
  [:label {:htmlFor "post-title"} "Title"])

(defn ok-label-wrapping-its-input [on-change]
  [:label {:class "field"}
   "Title"
   [:input {:id "post-title"
            :on-change on-change}]])

(defn ok-label-wrapping-a-deep-control [on-change]
  [:label {:class "field"}
   "Tags"
   [:div {:class "field__control"}
    [:select {:on-change on-change}
     [:option "Draft"]]]])

(defn ok-label-wrapping-a-mapped-control [options on-change]
  [:label {:class "field"}
   "Tags"
   (for [option options]
     ^{:key option} [:input {:type "checkbox"
                             :value option
                             :on-change on-change}])])

(defn ok-label-holding-a-component [field-props]
  ;; the component may render the control, so the question is left open
  [:label {:class "field"}
   "Summary"
   [editor-field field-props]])

(defn ok-label-with-built-for [base]
  [:label (assoc base :for "post-title") "Title"])

(defn ok-label-schema-entry []
  ;; a Malli map entry, not markup
  [:map
   [:label string?]
   [:index int?]])

(defn ok-label-naming-a-symbol [caption]
  ;; one bare symbol child reads the same as the schema entry, so it is left alone
  [:label caption])

(defn bad-label-beside-its-field [on-change]
  [:label {:class "field__label"} "Usage instruction"]
  [:textarea {:aria-label "Usage instruction"
              :on-change on-change}])

(defn bad-label-with-style-call-props [caption]
  [:label (styles/field-label) caption])

(defn bad-label-plain-text []
  [:label "Title"])

(defn bad-label-around-non-controls [caption]
  [:label {:class "field__label"}
   [:span {:class "icon"}]
   [:strong caption]])

(defn bad-label-wrapping-a-hidden-input [caption]
  ;; a hidden input renders nothing, so it is not the control this labels
  [:label {:class "field__label"}
   caption
   [:input {:type "hidden"
            :name "post-id"}]])

(defn ok-label-with-computed-id [field-id]
  ;; a field component builds the same id on both sides
  [:div
   [:label {:id (str field-id "-label")} "Name"]
   [:input {:aria-labelledby (str field-id "-label")}]])

(defn ok-label-with-id-shorthand []
  [:div
   [:label#notes-label "Notes"]
   [:textarea {:aria-labelledby "notes-label"}]])

(defn ok-label-with-opaque-props [props]
  ;; a symbol names a map per call site, which may carry the :for
  [:label props "Title"])

(defn ok-label-with-computed-props-key [k]
  [:label {k "post-title"} "Title"])

(defn ok-label-with-a-call-in-its-body [field]
  ;; the call may render the control
  [:label {:class "field__label"} "Name" (render-control field)])

(defn ok-label-schema-entry-with-properties []
  ;; a Malli entry carrying properties is still not markup
  [:map
   [:label {:optional true} string?]
   [:index int?]])

(defn ok-label-schema-entry-computed []
  [:map
   [:label (caption-schema)]])

(defn bad-label-referenced-only-by-describedby []
  ;; aria-describedby is a description, not a name, so this labels nothing
  [:div
   [:label {:id "usage-hint"} "Hint"]
   [:input {:aria-describedby "usage-hint"}]])
