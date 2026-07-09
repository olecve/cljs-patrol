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
