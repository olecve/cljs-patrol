(ns webapp.styles
  (:require
   [spade.core :refer [defclass defattrs]]))

(defclass container-style []
  {:display :flex})

(defclass unused-style []
  {:color :red})

(defattrs unused-attrs []
  {:data-foo "bar"})

(defattrs merged-attrs []
  {:font-size "14px"})

(defclass sole-attr-style []
  {:background :blue})

(defclass vector-sole-attr-style []
  {:margin "10px"})

(defclass vector-multi-class-style []
  {:padding "5px"})
