(ns datadefapp.schema
  (:require #?(:cljs [reagent.core :as r])))

(def bad-cljc-as-element-in-schema [:vector {:render #?(:cljs (r/as-element [:img :src]))} :int])

(def bad-reader-conditional-thread-step-in-schema [:vector {:render (-> [:img :src] #?(:cljs r/as-element))} :int])
