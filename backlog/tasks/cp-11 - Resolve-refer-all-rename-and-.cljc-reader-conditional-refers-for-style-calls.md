---
id: CP-11
title: "Resolve :refer :all, :rename and .cljc reader-conditional refers for style calls"
status: Idea
labels: ["spade"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A spade style called through :refer :all, :rename, or a :refer inside #?(:cljs …) in a .cljc ns form is not resolved, so
it shows as an unused-styles false positive.

### Flagged

```clojure
;; reported as unused today
(ns app.view (:require [app.styles :refer :all]))
(defn v [] [:div (row-style)])
```

### Notes

Follow-up from cljs-patrol PR 148 review round 2; older than 148.
<!-- SECTION:DESCRIPTION:END -->
