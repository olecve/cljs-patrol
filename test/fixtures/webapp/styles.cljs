(ns webapp.styles
  (:require
   [spade.core :refer [defclass defattrs]]))

(defclass container-style []
  {:display :flex})

(defclass unused-style []
  {:color :red})

(defattrs unused-attrs []
  {:data-foo "bar"})
