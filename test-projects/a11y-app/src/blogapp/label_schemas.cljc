(ns blogapp.label-schemas
  (:require
   [malli.util :as mu]))

(def call-def-schema
  (mu/closed-schema
   [:map
    [:label {:optional true} [:maybe :string]]
    [:index :int]]))

(defn ok-label-entry-with-properties-and-maybe []
  [:map
   [:label {:optional true
            :description "x"}
    [:maybe [:or :string :int]]]])

(defn ok-label-entry-with-or []
  [:map
   [:label [:or :string :int]]])

(defn ok-label-entry-with-fn []
  [:map
   [:label {:optional true} [:fn pos?]]])

(defn ok-label-entry-with-reader-conditional-properties []
  [:map
   [:label #?(:cljs {:optional true}) [:maybe :string]]])

(defn ok-label-entry-with-vector []
  [:map
   [:label [:vector :string]]])

(defn ok-label-entry-with-set []
  [:map
   [:label [:set :keyword]]])

(defn ok-label-around-a-vector-with-a-discarded-head [caption]
  [:label [#_:maybe :span caption]])

(defn bad-label-around-a-span [caption]
  [:label [:span caption]])

(defn bad-label-around-a-strong-with-props [caption]
  [:label {:class "field__label"}
   [:strong caption]])

(defn bad-label-with-nil-for [caption]
  [:label {:for nil} [:span caption]])

(defn bad-label-around-interop [Comp]
  [:label [:> Comp]])

(defn bad-label-text-beside-a-schema-head []
  [:label "Name" [:maybe "optional"]])

(defn bad-label-with-a-dom-attr-over-a-schema-head []
  [:label {:class "field__label"} [:maybe :string]])
