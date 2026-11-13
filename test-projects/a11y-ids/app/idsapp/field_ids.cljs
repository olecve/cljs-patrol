(ns idsapp.field-ids
  (:require [blogapp.ui :as ui]))

(defn textarea-in-labelled-field [notes]
  [ui/field {:label "Notes"}
   [:textarea {:id "field-wrapped-notes"
               :value notes}]])
