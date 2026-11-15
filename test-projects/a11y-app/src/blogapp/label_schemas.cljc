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

(defn ok-label-with-unreadable-properties-from-a-reader-conditional []
  [:map
   [:label #?(:cljs {:optional true}) [:maybe :string]]])

(defn ok-label-entry-with-a-nil-literal []
  [:map
   [:label [:= nil]]])

(defn ok-label-around-a-quoted-element []
  [:label '[:span "Name"]])

(defn ok-label-entry-with-vector []
  [:map
   [:label [:vector :string]]])

(defn ok-label-entry-with-set []
  [:map
   [:label [:set :keyword]]])

(defn ok-label-entry-with-a-discarded-string []
  [:map
   [:label [:maybe #_"Name" :string]]])

(defn ok-label-entry-with-metadata []
  [:map
   [:label ^:foo [:maybe :string]]])

(defn ok-label-with-unreadable-properties-built-by-merge [base]
  [:map
   [:label (merge base {:optional true}) [:maybe :string]]])

(defn ok-label-entry-with-called-properties [field-props]
  [:map
   [:label (field-props) [:maybe :string]]])

(defn ok-label-entry-with-called-properties-and-a-discarded-element [field-props]
  [:map
   [:label (field-props) #_[:span "x"] [:maybe :string]]])

(defn ok-label-entry-with-called-properties-and-a-discarded-string [field-props]
  [:map
   [:label (field-props) #_"x" [:maybe :string]]])

(defn ok-label-entry-with-properties-and-metadata []
  [:map
   [:label {:optional true} ^{:key 1} [:maybe :string]]])

(defn ok-label-around-an-image-map-with-symbol-labels-that-reads-as-a-schema [url label]
  [:label [:map {:name "nav"} [:a {:href url} label]]])

(defn ok-label-around-an-svg-set-that-reads-as-a-schema []
  [:label [:set {:attributeName "fill" :to "red"}]])

(defn ok-label-around-a-maybe-with-props-that-reads-as-a-schema [caption]
  [:label [:maybe {:class "x"} caption]])

(defn bad-label-entry-with-a-title-property []
  [:map
   [:label {:title "Name"} :string]])

(defn bad-label-around-a-span [caption]
  [:label [:span caption]])

(defn bad-label-around-a-strong-with-props [caption]
  [:label {:class "field__label"}
   [:strong caption]])

(defn bad-label-around-a-span-with-metadata [caption]
  [:label {:optional true} ^{:key 1} [:span caption]])

(defn bad-label-with-nil-for [caption]
  [:label {:for nil} [:span caption]])

(defn bad-label-around-interop [Comp]
  [:label [:> Comp]])

(defn bad-label-text-beside-a-schema-head []
  [:label "Name" [:maybe "optional"]])

(defn bad-label-with-a-dom-attr-over-a-schema-head []
  [:label {:class "field__label"} [:maybe :string]])

(defn bad-label-around-an-svg-symbol-with-text []
  [:label [:symbol {:id "s"} [:text "Hi"]]])

(defn bad-label-around-an-svg-symbol-beside-a-caption [caption]
  [:label [:symbol {:id "x"}] caption])

(defn bad-label-around-an-empty-svg-symbol []
  [:label [:symbol]])

(defn bad-label-around-an-image-map-beside-a-caption [caption]
  [:label [:map {:name "nav"} [:area {:href "/"}]] caption])

(defn bad-label-around-an-image-map []
  [:label [:map {:name "nav"} [:area {:href "/"}]]])

(defn bad-label-around-a-map-with-text []
  [:label [:map "Map text"]])

(defn bad-label-around-a-maybe-with-text []
  [:label [:maybe "optional"]])

(defn bad-label-with-called-properties-and-a-quoted-element [field-props]
  [:label (field-props)
   '[:span "Name"]])

(defn bad-label-with-called-properties-and-a-quoted-symbol [field-props]
  [:label (field-props)
   'caption])
