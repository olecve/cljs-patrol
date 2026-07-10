(ns blogapp.ui)

;; Fake component library. Bodies are irrelevant — the fixture only needs
;; these symbols to exist so callers can refer/alias them and the analyzer
;; can map them to native tags via :component-aliases.

(defn textarea [attrs] attrs)

(defn text-input [attrs] attrs)
