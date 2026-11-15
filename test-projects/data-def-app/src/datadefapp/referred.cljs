(ns datadefapp.referred
  (:require [reagent.core :refer [as-element]]))

(def bad-referred-as-element-in-schema [:vector {:render (as-element [:img :src])} :int])
