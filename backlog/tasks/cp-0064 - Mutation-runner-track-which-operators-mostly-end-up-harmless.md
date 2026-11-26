---
id: CP-0064
title: "Mutation runner: track which operators mostly end up harmless"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Some operators may produce mostly equivalent or skipped mutants, which is noise.

### Why

Google found relational and logical swaps useful most often and dropped one operator (ABS) completely (arXiv
2102.11378).

### Fix

```clojure
;; per operator: planted, caught, gaps, skipped as harmless (CP-0051) -> drop or demote operators that are mostly noise
```

### Notes

Needs CP-0051 reasons first.
<!-- SECTION:DESCRIPTION:END -->
