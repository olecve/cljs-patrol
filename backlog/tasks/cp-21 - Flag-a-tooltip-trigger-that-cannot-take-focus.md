---
id: CP-21
title: "Flag a tooltip trigger that cannot take focus"
status: Done
labels: ["a11y"]
priority: high
created_date: ""
updated_date: "2026-11-25"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Radix Tooltip's as-child mode puts the trigger props, including `aria-describedby` when the tooltip is open, on the
direct child. A plain span or div without a tab stop cannot receive keyboard focus. If a button inside the wrapper
receives focus, the description still belongs to the wrapper.

### Why

Keyboard users need to reach the element that acts as the tooltip trigger.

### Flagged

```clojure
[tooltip {:content "Click to see details"} [:span {:class c} [:button …]]]
[tooltip {:content "Info"} [:div "x"]]
```

### Fix

```clojure
[tooltip {:content "Click to see details"} [:button …]]
;; or give the wrapper a role and :tab-index 0 when it is the control
```

### Left alone

```clojure
[tooltip {:content "Save"} [:button …]]
[tooltip {:content "Docs"} [:a {:href u} "Docs"]]
[tooltip {:content r} [:span {:role "group" :tab-index 0 :aria-label l} …]]
```

### Notes

The `non-focusable-tooltip-trigger` rule checks direct native span and div triggers in configured tooltip wrappers and
in explicit Radix triggers with `:asChild true`. It recognizes `:tab-index`, `:tabIndex`, and `:tabindex`.

### Links

- [Implementation in PR #187](https://github.com/olecve/cljs-patrol/pull/187)
- [Rule documentation](../../docs/rules/a11y.md#non-focusable-tooltip-trigger)

<!-- SECTION:DESCRIPTION:END -->
