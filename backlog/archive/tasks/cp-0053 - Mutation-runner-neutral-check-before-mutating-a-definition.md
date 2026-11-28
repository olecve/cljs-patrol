---
id: CP-0053
title: "Mutation runner: neutral check before mutating a definition (moved to mutacio-clj MC-0001)"
status: Idea
labels: ["mutation-runner"]
milestone: m-0
created_date: ""
updated_date: "2026-11-28"
references: []
---

## Description

Cancelled here: the runner moved to [mutacio-clj](https://github.com/olecve/mutacio-clj) (ADR 0016), and this task is
MC-0001 there.

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A value a def computes when the namespace loads keeps the original code, so mutants only it reaches survive falsely.

### Why

Ruby's mutant runs each subject unchanged through the same path first. Catches false survivors from the
evaluate-and-restore path.

### Fix

```clojure
;; evaluate the unchanged form, run its selected tests; if they fail, mark the definition 'not mutable' instead of reporting survivors
```

### Notes

One extra run per definition; could reuse the test-plan check from PR 165.

### Order

Needs a new design before work starts. The runner already runs the selected tests once without a mutation, and replaying
unchanged code cannot show a caller that kept the original function value; ADR 0005 records that limit.

<!-- SECTION:DESCRIPTION:END -->
