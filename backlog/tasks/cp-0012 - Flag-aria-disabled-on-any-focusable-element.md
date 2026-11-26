---
id: CP-0012
title: "Flag aria-disabled on any focusable element"
status: Idea
labels: ["forbid-props"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

forbidden-prop only checks :aria-disabled on :button. Moving the attribute to a focusable span or div (tab-index 0 or a
click handler) passes the rule while keeping the same problem: an element that looks disabled but stays in the tab order
and must block its own action.

### Why

Changing a button to a focusable span currently bypasses the check.

### Flagged

```clojure
[:span {:aria-disabled true :role "group" :tab-index 0 :aria-label l} face]
[:div {:aria-disabled true :tab-index 0 :on-click f} …]
```

### Fix

```clojure
;; use native disabled behavior
[:button {:disabled true :type "button"} "Save"]
```

### Left alone

```clojure
;; ARIA composite items where aria-disabled is the pattern
[:div {:role "option" :aria-disabled true} …]
;; non-focusable, no handler
[:div {:aria-disabled true} …]
```

### Notes

Match :aria-disabled (non-false) together with :tab-index 0 or :on-click on any tag; skip roles option, menuitem, tab,
treeitem, gridcell.
<!-- SECTION:DESCRIPTION:END -->
