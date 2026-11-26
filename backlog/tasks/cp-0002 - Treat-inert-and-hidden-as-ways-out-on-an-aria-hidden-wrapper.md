---
id: CP-0002
title: "Treat :inert and :hidden as ways out on an aria-hidden wrapper"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

aria-hidden-focusable reports {:aria-hidden true :inert true} and {:aria-hidden true :hidden true}, also on literal
maps, though inert or hidden content takes no focus.

### Flagged

```clojure
;; false positives today
[:div {:aria-hidden true :inert true} [:button "x"]]
[:div {:aria-hidden true :hidden true} [:a {:href u} "x"]]
```

### Fix

```clojure
;; nothing to fix: these should not be reported
```

### Notes

Follow-up from cljs-patrol PR 151 review; older than 151.
<!-- SECTION:DESCRIPTION:END -->
