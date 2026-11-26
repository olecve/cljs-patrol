---
id: CP-0005
title: "One declare at the top of a namespace"
status: Idea
labels: ["idioms", "layout"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Some codebases scatter several (declare …) forms through a namespace. A rule could ask for a single declare right after
the ns form, listing every forward-declared name, one per line, sorted.

### Flagged

```clojure
(ns app.views)
(declare row)
(defn table [] …)
(declare cell)
```

### Fix

```clojure
(ns app.views)
(declare cell
         row)
```

### Notes

Open: which group; whether a declare inside a reader conditional counts; whether to also flag a declare whose name is
defined before it (unneeded).
<!-- SECTION:DESCRIPTION:END -->
