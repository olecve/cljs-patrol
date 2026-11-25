---
id: CP-46
title: "prefer-tag-over-role and no-noninteractive-tabindex"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A :role that has a native element ([:div {:role "button"}]) should be that element, which brings keyboard handling for
free (jsx-a11y prefer-tag-over-role). A :tab-index 0 on an element with no interactive role adds a tab stop that does
nothing (jsx-a11y no-noninteractive-tabindex).

### Why

Both push toward native elements and a clean tab order.

### Flagged

```clojure
[:div {:on-click f :role "button" :tab-index 0} "Save"]
[:p {:tab-index 0} "Read me"]
```

### Fix

```clojure
[:button {:on-click f :type "button"} "Save"]
[:p "Read me"]
```

### Left alone

```clojure
;; an intentionally focusable group
[:span {:aria-label l :role "group" :tab-index 0} …]
;; a scroll region
[:div {:aria-label "Log" :role "region" :tab-index 0} …]
```

### Notes

Account for intentionally focusable groups and regions.
<!-- SECTION:DESCRIPTION:END -->
