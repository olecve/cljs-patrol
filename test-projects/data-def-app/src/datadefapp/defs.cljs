(ns datadefapp.defs
  (:require [datadefapp.other :as other]
            [reagent.core :as r]))

(def ok-props-schema
  [:map {:closed true}
   [:label {:optional true
            :description "The text of the tag."}
    [:maybe [:or :string :int]]]])

(def ok-image-path [:img :src])

(def ok-element-kinds [:img :video :audio])

(def ok-fragment-entry-schema [:map [:<> {} :int]])

(def ok-button-schema
  [:map
   [:button {:aria-disabled true} :boolean]])

(defonce ok-image-path-once [:img :src])

(def ok-documented-schema
  "Describes an image."
  [:map [:img {:optional true} :string]])

(def ok-look-alike-as-element [:vector {:render (other/as-element [:img :src])} :int])

(def ok-unresolved-as-element [:vector {:render (as-element [:img :src])} :int])

(def bad-hiccup-image [:img {:src "a.png"}])

(def bad-toast
  {:message "Saved"
   :description [:img {:src "a.png"}]})

(def bad-routes
  [["/" {:view (fn [match] [:img {:src (:path match)}])}]])

(def bad-actions
  [{:render-item (fn [user] [:img {:src (:avatar user)}])}])

(def bad-as-element-in-schema [:vector {:render (r/as-element [:img :src])} :int])

(def bad-full-as-element-in-schema [:vector {:render (reagent.core/as-element [:img :src])} :int])

(def bad-fn-literal-in-schema [:vector {:render #(do [:img :src])} :int])

(def bad-garden-rule [:div.paragraph [:img {:margin-top "5px"}]])

(def bad-cells [[:img :a] [:img.thumb :b]])

(def bad-hiccup-button [:button {:aria-disabled true} "Submit"])

(def bad-empty-fragment-props [:<> {} [:span "a"]])

(def bad-split-label-schema
  [:map
   [:label {:optional true
            :default "a"} [:maybe :string]]])

(def bad-path-in-meta ^:const [:img :src])

#?(:cljs (def bad-path-in-reader-conditional [:img :src]))

(defn bad-path-in-function [] [:img :src])
