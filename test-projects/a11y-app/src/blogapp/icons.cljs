(ns blogapp.icons)

;; Fake icon library, shaped like a real one: callers reach every var through a
;; single `blogapp.icons/*` alias rather than listing each icon by name.

(defn square [attrs] attrs)

(defn check-square [attrs] attrs)

(defn minus-square [attrs] attrs)
