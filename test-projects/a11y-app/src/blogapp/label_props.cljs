(ns blogapp.label-props
  (:require [blogapp.ui :as ui]))

(defn ok-toggle-label [on?]
  [ui/toggle {:checked? on?
              :label "Make public"}])

(defn ok-toggle-second-label-prop [on?]
  [ui/toggle {:checked? on?
              :label-text "Make public"}])

(defn ok-toggle-computed-label [label on?]
  [ui/toggle {:checked? on?
              :label label}])

(defn ok-toggle-nil-label-beside-written-one [on?]
  [ui/toggle {:checked? on?
              :label nil
              :label-text "Make public"}])

(defn bad-toggle-unlabeled [on?]
  [ui/toggle {:checked? on?}])

(defn bad-toggle-blank-label [on?]
  [ui/toggle {:checked? on?
              :label ""}])

(defn ok-toggle-blank-labelledby-beside-label [on?]
  [ui/toggle {:aria-labelledby ""
              :checked? on?
              :label "Make public"}])

(defn bad-switch-label-prop-not-listed [on?]
  [ui/switch {:checked? on?
              :label "Make public"}])

(defn ok-icon-button-label [close!]
  [ui/icon-button {:label "Close"
                   :on-click close!}])

(defn bad-icon-button-unlabeled [close!]
  [ui/icon-button {:on-click close!}])

(defn ok-repeated-alt-in-labelled-button [items remove!]
  [:ul
   (for [item items]
     ^{:key (:id item)}
     [:li [ui/icon-button {:label (:name item)
                           :on-click #(remove! item)}
           [:img {:alt "Remove"
                  :src "x.svg"}]]])])

(defn ok-repeated-label-beside-varying-aria-label [items remove!]
  (for [item items]
    ^{:key (:id item)}
    [ui/icon-button {:aria-label "Remove"
                     :label (str "Remove " (:name item))
                     :on-click #(remove! item)}]))

(defn bad-repeated-literal-label [items remove!]
  (for [item items]
    ^{:key (:id item)}
    [ui/icon-button {:label "Remove"
                     :on-click #(remove! item)}]))

(defn ok-repeated-literal-label-beside-varying-aria-label [items remove!]
  (for [item items]
    ^{:key (:id item)}
    [ui/icon-button {:aria-label (str "Remove " (:name item))
                     :label "Remove"
                     :on-click #(remove! item)}]))

(defn ok-toggle-blank-label-beside-aria-label [on?]
  [ui/toggle {:aria-label "Make public"
              :checked? on?
              :label ""}])

(defn ok-toggle-label-beside-blank-aria-label [on?]
  [ui/toggle {:aria-label ""
              :checked? on?
              :label "Make public"}])

(defn bad-toggle-blank-label-beside-blank-aria-label [on?]
  [ui/toggle {:aria-label ""
              :checked? on?
              :label ""}])

(defn ok-toggle-blank-label-beside-filled-label-text [on?]
  [ui/toggle {:checked? on?
              :label ""
              :label-text "Make public"}])

(defn ok-toggle-blank-label-text-beside-filled-label [on?]
  [ui/toggle {:checked? on?
              :label "Make public"
              :label-text ""}])
