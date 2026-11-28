---
id: CP-0064
title: "Mutation runner: track which operators mostly end up harmless (moved to mutacio-clj MC-0004)"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: "2026-11-28"
references: []
---

## Description

Cancelled here: the runner moved to [mutacio-clj](https://github.com/olecve/mutacio-clj) (ADR 0016), and this task is
MC-0004 there.

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
