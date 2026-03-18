(ns webapp.local-styles
  (:require
   [spade.core :refer [defclass]]))

;; defclass declared and used as sole attr in the same file (no alias)
(defclass local-panel-style []
  {:padding "16px"})

(defn local-panel []
  [:div {:class (local-panel-style)} "content"])
