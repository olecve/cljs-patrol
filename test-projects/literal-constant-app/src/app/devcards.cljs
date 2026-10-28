(ns app.devcards
  (:require
   [cljs.test :refer [is]]))

(def card-title "Card")

(defn check-title []
  (is (= "Card" card-title)))
