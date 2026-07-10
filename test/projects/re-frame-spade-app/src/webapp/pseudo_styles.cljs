(ns webapp.pseudo-styles
  (:require
   [spade.core :refer [defclass defattrs]]))

;; Broken — :&:hover placed inside the main map.
(defclass menu-item-style []
  {:display :flex
   :padding "6px 12px"
   :&:hover {:background "#eee"}})

;; Broken — two pseudo-selectors nested in the main map.
(defattrs card-section-attrs []
  {:padding "8px 0"
   :&:first-child {:padding-top 0}
   :&:last-child {:padding-bottom 0}})

;; Broken — combinator selector nested in the main map.
(defclass tab-style []
  {:cursor :pointer
   :&:focus-visible>svg {:outline "2px solid #06f"}})

;; Correct — pseudo-selector as its own sibling vector after the main map.
(defclass icon-button-style []
  {:padding "4px"}
  [:&:hover {:opacity 0.8}]
  [:&:focus-visible {:outline "2px solid #06f"}])

;; Broken — two self-selectors in a single vector compile to a descendant
;; selector, not the comma-joined selector the author intended.
(defattrs badge-marker-attrs []
  {:display :inline-block}
  [:&:before
   :&:after
   {:content "''"
    :height "1px"}])

;; Broken — three consecutive self-selectors, same problem.
(defclass callout-style []
  {:border "1px solid #ccc"}
  [:&:hover :&:focus :&:focus-visible {:background "#f5f5f5"}])

;; Correct — descendant selector with a class, not a self-pseudo chain.
(defclass panel-style []
  {:padding "12px"}
  [:&:hover :.child-icon {:color "#06f"}])
