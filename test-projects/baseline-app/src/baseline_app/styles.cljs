(ns baseline-app.styles
  (:require
   [spade.core :refer [defclass defattrs]]))

(defclass container []
  {:display "flex"})

;; Unused - should appear in baseline
(defclass legacy-panel []
  {:border "1px solid gray"})

(defattrs button-attrs []
  {:padding "8px 16px"})
