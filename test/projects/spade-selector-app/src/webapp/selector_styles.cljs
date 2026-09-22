(ns webapp.selector-styles
  (:require
   [spade.core :refer [defclass defattrs]]))

;; Broken — Garden substitutes & only at position 0, so this emits a literal &.
(defclass focus-within-style []
  {:opacity 0.4}
  ["li:focus-within &" {:opacity 1}])

;; Broken — a second & after the leading one stays literal.
(defattrs chained-ampersand-attrs []
  {:color "#333"}
  ["&:hover &" {:color "#06f"}])

;; Broken — & in the middle of a descendant selector.
(defclass mid-ampersand-style []
  {:margin 0}
  ["foo & bar" {:margin "4px"}])

;; Correct — & at position 0, the only place Garden substitutes it.
(defclass ampersand-at-front-style []
  {:padding "4px"}
  ["&" {:color "#000"}]
  ["&:hover" {:color "#06f"}]
  ["&[data-x]" {:color "#f60"}])

;; Correct — no & in any selector; the & in a property value is not a selector.
(defclass no-ampersand-style []
  {:display :block}
  [".child" {:color "#333"}]
  ["p" {:content "a & b"}])

;; Broken — :> becomes its own selector in a comma-joined list, not a child combinator.
(defclass child-combinator-style []
  {:display :flex}
  [:> :span {:flex 1}])

;; Broken — adjacent-sibling combinator in keyword form.
(defattrs adjacent-combinator-attrs []
  {:margin 0}
  [:+ :p {:margin-top "8px"}])

;; Broken — a combinator anywhere in the vector splits the selector, not only a leading one.
(defclass mid-combinator-style []
  {:gap "4px"}
  [:.icon :> :img {:opacity 0.5}])

;; Correct — the string form keeps the combinator attached to the next element.
(defclass string-combinator-style []
  {:display :flex}
  ["> span" {:flex 1}])

;; Correct — a compound self-selector and plain descendant selectors.
(defclass descendant-style []
  {:fill "none"}
  [:&:hover {:fill "#06f"}]
  [:svg :path {:stroke "#333"}]
  [:&:hover :span {:color "#06f"}])

;; Broken — both rules fire one level down, inside a nested selector block.
(defclass nested-block-style []
  {:position :relative}
  [:.panel {:padding "8px"}
   [:> :button {:border 0}]
   ["aside:hover &" {:outline "1px solid #06f"}]])
