---
id: CP-21
title: "Flag a tooltip trigger that cannot take focus"
status: Next
labels: ["a11y"]
priority: high
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Radix Tooltip uses as-child, so the tooltip's first child becomes the trigger and gets aria-describedby. When that child
is a plain span or div (no role, no tab stop), keyboard users never open the tooltip and screen readers never read its
text, even if a button sits inside the span.

### Why

An as-child tooltip trigger must be keyboard-focusable; this rule would check that requirement.

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

Configurable tooltip component names and a list of known focusable components (button, icon-button).
<!-- SECTION:DESCRIPTION:END -->
