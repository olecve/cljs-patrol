---
id: CP-0007
title: "Flag a named group wrapper whose label repeats the control's label"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A role=group wrapper with an :aria-label equal to the :aria-label of the control inside it makes screen readers announce
the same name twice ('Save, group', then 'Save, button').

### Why

A labelled wrapper and its child control can repeat the same accessible name.

### Flagged

```clojure
[:span {:aria-label name :role "group" :tab-index 0}
 [:button {:aria-label name :disabled true} …]]
```

### Fix

```clojure
;; drop the inner label while the wrapper names the group, or
[:span {:role "group" :aria-labelledby id} [:button {:id id} …]]
```

### Left alone

```clojure
;; different labels; a group with no :aria-label; a control named only by its text
```

### Notes

Match the same symbol or string literal on both sides. The component form needs config naming which components put
:aria-label on a group.
<!-- SECTION:DESCRIPTION:END -->
