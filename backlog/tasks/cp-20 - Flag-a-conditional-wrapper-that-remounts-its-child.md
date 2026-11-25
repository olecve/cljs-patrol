---
id: CP-20
title: "Flag a conditional wrapper that remounts its child"
status: Done
labels: ["reagent"]
priority: high
created_date: ""
updated_date: "2026-11-25"
references: ["https://github.com/olecve/cljs-patrol/pull/163"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

When one branch of an if/when/cond wraps a child in a component and the other renders the same child bare, React sees a
different tree when the condition flips. The child unmounts and mounts again, and keyboard focus falls to <body>. The
same happens when a focusable element changes tag between branches (span vs button) around the same content.

### Why

Changing the wrapper changes the React tree and remounts the child, losing its focus and local state.

### Flagged

```clojure
(if highlighted? [example-container {:highlighted? true} child] child)
(if x [:span {:tab-index 0} face] [:button {:on-click f} face])
```

### Fix

```clojure
[example-container {:highlighted? highlighted?} child]
;; or keep one element type in both branches
```

### Left alone

```clojure
;; branches that render unrelated content
(if loading? [spinner] [table rows])
;; same tag in both branches
(if x [:button {:class a} face] [:button {:class b} face])
```

### Notes

Shipped as the `remounting-branch` rule.

Match: branches [W … x] and x for the same symbol or form x; or two focusable elements (button, a, :tab-index) with
different tags holding the same child.

### Links

- [Implementation in PR #163](https://github.com/olecve/cljs-patrol/pull/163)
- [Rule documentation](../../docs/rules/reagent.md#remounting-branch)

<!-- SECTION:DESCRIPTION:END -->
