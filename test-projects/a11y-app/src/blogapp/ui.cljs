(ns blogapp.ui)

;; Fake component library. Bodies are irrelevant — the fixture only needs
;; these symbols to exist so callers can refer/alias them and the analyzer
;; can map them to native tags via :component-aliases.

(defn textarea [attrs] attrs)

(defn text-input [attrs] attrs)

(defn dialog-root [attrs & children] attrs)

(defn button [attrs & children] attrs)

(defn switch [attrs] attrs)

(defn select [attrs & children] attrs)

(defn search-field [attrs] attrs)

(defn stepper [attrs] attrs)

(defn range-slider [attrs] attrs)

(defn toggle [attrs] attrs)

(defn icon-button [attrs & children] attrs)
