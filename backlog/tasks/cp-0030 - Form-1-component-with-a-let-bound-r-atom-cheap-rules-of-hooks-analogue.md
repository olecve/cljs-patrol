---
id: CP-0030
title: "Form-1 component with a let-bound r/atom (cheap rules-of-hooks analogue)"
status: Idea
labels: ["reagent"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A form-1 component that creates (r/atom …) in its let gets a new atom on every render, so its state resets each time. It
should be form-2 (return a fn) or use use-state.

### Why

A silent state-loss bug with a precise shape.

### Flagged

```clojure
(defn counter []
  (let [n (r/atom 0)]
    [:button {:on-click #(swap! n inc)} @n]))
```

### Fix

```clojure
(defn counter []
  (let [n (r/atom 0)]
    (fn []
      [:button {:on-click #(swap! n inc)} @n])))
```

### Left alone

```clojure
;; r/atom outside the render path, or a form-2 outer let
(defonce state (r/atom 0))
```

### Notes

react-hooks rules-of-hooks has no direct Reagent analogue.
<!-- SECTION:DESCRIPTION:END -->
