---
id: CP-16
title: "Flag an inset ring whose High Contrast outline sits outside the box"
status: Idea
labels: ["focus-ring"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

focus-ring-missing-outline accepts any outline, so an inset box-shadow ring with a transparent outline but no negative
outline-offset passes. In forced-colors (High Contrast) mode box shadows are dropped and the outline is drawn outside
the element, where a parent with overflow: hidden clips it, so no focus indicator shows.

### Why

Reproducible in Chrome when an ancestor clips overflow.

### Flagged

```clojure
[:&:focus-visible {:box-shadow example.rings/inset-shadow
                   :outline "2px solid transparent"}]
[:&:focus-visible {:box-shadow "inset 0 0 0 2px #336699"
                   :outline "2px solid transparent"}]
```

### Fix

```clojure
[:&:focus-visible example.rings/inset-outline]
;; or add :outline-offset "-2px"
```

### Left alone

```clojure
;; an outer ring with any outline; an inset ring with a negative literal offset;
;; a style map named by a symbol; a non-literal :outline-offset
```

### Notes

Open: tier (the rule cannot see whether the parent clips; cleanup tier or a hint), new key vs extending
focus-ring-missing-outline, explicit config of inset presets.
<!-- SECTION:DESCRIPTION:END -->
