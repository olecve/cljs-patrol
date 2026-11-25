---
id: CP-18
title: "Widen inline-component to large let-bound Hiccup with one handler"
status: Idea
labels: ["reagent"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

inline-component flags a let binding whose Hiccup holds two or more elements with event handlers. A big bound element
with only one handler (a whole row with nested conditional parts) slips through, though it is a component in all but
name.

### Why

Large Hiccup bindings can warrant extraction even when they contain only one handler.

### Flagged

```clojure
(let [row [:button {:on-click #(open! item)}
           [:span (icon-style) [icon {:size 16}]]
           [:span (text-style)
            [:span (label-style) label]
            (when secondary-text [:span secondary-text])]]]
  [wrapper {…} row])
```

### Fix

```clojure
(defn- item-button [props] [:button …])
[wrapper {…} [item-button props]]
```

### Left alone

```clojure
(let [icon [icons/check {:size 12}]] …)
```

### Notes

Candidate signal: 1+ handler AND (element count >= N or a nested conditional element). Calibrate N with a corpus scan
before shipping.
<!-- SECTION:DESCRIPTION:END -->
