(ns webapp.order-styles
  (:require
   [spade.core :refer [defclass defattrs]]))

;; Correct — position, then box model, then typography, then background.
(defclass card-style []
  {:position :relative
   :display :flex
   :padding "8px"
   :color "#333"
   :background "#fff"})

;; Out of order — :display belongs before :color.
(defclass banner-style []
  {:color "#333"
   :display :block
   :padding "4px"
   :background "#eee"})

;; Out of order — a ranked property after a custom property, which sorts last.
(defclass widget-style []
  {:display :flex
   :--widget-gap "4px"
   :padding "8px"
   :color "#333"})

;; Correct — unranked properties trailing the ranked ones, and unranked among themselves.
(defclass surface-style []
  {:display :flex
   :padding "8px"
   :color "#333"
   :--surface-gap "4px"
   :--surface-inset "2px"})

;; Base map correct; the nested :&:hover block is not.
(defclass link-style []
  {:display :inline-flex
   :padding "2px"
   :color "#06f"
   :cursor :pointer}
  [:&:hover {:color "#04c"
             :background "#eef"
             :padding "3px"
             :text-decoration :underline}])

;; Out of order — defattrs is read the same way as defclass.
(defattrs row-attrs []
  {:background "#fff"
   :display :grid
   :gap "8px"
   :padding "4px"})

;; Correct — three properties is below the threshold, so the order is not judged.
(defclass chip-style []
  {:color "#333"
   :display :block
   :padding "2px"})

;; Base map and the first nested block correct; the block nested inside that one is not.
(defclass panel-style []
  {:display :flex
   :padding "8px"
   :color "#333"
   :background "#fff"}
  [:.section {:display :block
              :padding "4px"
              :color "#666"
              :background "#fafafa"}
   [:&:focus-within {:background "#eef"
                     :color "#04c"
                     :padding "6px"
                     :display :flex}]])

;; Out of order — metadata on the name must not hide the declaration.
(defclass ^:private muted-style []
  {:color "#999"
   :display :block
   :padding "2px"
   :background "#fafafa"})

;; Skipped — a map a call builds is not a literal this can read, however it is ordered.
(defattrs themed-attrs [dark?]
  (merge {:display :flex
          :padding "8px"}
         (if dark?
           {:color "#fff"
            :background "#000"
            :display :block
            :position :absolute}
           {:color "#000"
            :background "#fff"
            :display :block
            :position :absolute})))
