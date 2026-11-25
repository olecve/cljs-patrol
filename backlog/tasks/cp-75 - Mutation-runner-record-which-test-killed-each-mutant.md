---
id: CP-75
title: "Mutation runner: record which test killed each mutant"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/166"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A run stops at the first failing test but does not keep which test that was, so the report cannot show 'killed by'.

### Why

Needed for killedBy in the --html report (#74), and helps when a mutant is killed by an unexpected test.

### Fix

```clojure
;; the stop-at-failure reporter already sees the failing event; keep (:var (first t/*testing-vars*)) with the outcome
```

### Notes

Merged in PR 166 (ff6a4ee): :killed-by on each mutant, :tests on each definition.

### Links

- cljs-patrol PR 166: https://github.com/olecve/cljs-patrol/pull/166

<!-- SECTION:DESCRIPTION:END -->
