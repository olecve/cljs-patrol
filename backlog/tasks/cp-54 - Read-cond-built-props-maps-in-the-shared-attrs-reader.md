---
id: CP-54
title: "Read cond-> built props maps in the shared attrs reader"
status: Next
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

hiccup/attrs-info reads props built with assoc, assoc-in and merge, but treats (cond-> {…} test (assoc …)) as
unreadable. The rule misses a missing or invalid :type in a cond-> base map when no conditional step can change it. The
a11y name, tabindex and aria rules miss the same shape.

### Why

cond-> is the common way to add optional props in Reagent code; reading it closes a whole class of misses for every
attrs rule at once.

### Flagged

```clojure
;; no step can set :type, so the base map's missing :type stands
(defn save [x] [:button (cond-> {:class "c"} x (assoc :id "i")) "Save"])

;; the base :type is invalid, and no step changes it
(defn save2 [x] [:button (cond-> {:type "btn"} x (assoc :id "i")) "Save"])
```

### Fix

```clojure
(defn save [x] [:button (cond-> {:class "c" :type "button"} x (assoc :id "i")) "Save"])
```

### Left alone

```clojure
;; a step may set :type
(defn save [x] [:button (cond-> {:class "c" :on-click x} x (assoc :type "button")) "Save"])

;; a step the reader cannot follow may set anything
(defn save [x] [:button (cond-> {:class "c"} x (merge opts)) "Save"])
```

### Notes

Semantics: with a literal (or resolvable) base map, a key that no step can touch is settled (present, with its value); a
key a step may assoc, dissoc, update or merge in is unknown, as is everything when a step is a call the reader cannot
follow (merge with a symbol, a fn). cond->> and some-> could follow. Probed 2026-11-18: assoc is flagged today, all
three cond-> shapes are left alone. Related: backlog item 8 (assoc/merge reading).
<!-- SECTION:DESCRIPTION:END -->
