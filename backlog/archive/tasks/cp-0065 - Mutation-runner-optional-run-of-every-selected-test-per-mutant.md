---
id: CP-0065
title: "Mutation runner: optional run of every selected test per mutant (moved to mutacio-clj MC-0005)"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: "2026-11-28"
references: []
---

## Description

Cancelled here: the runner moved to [mutacio-clj](https://github.com/olecve/mutacio-clj) (ADR 0016), and this task is
MC-0005 there.

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Each mutant's run stops at the first failing test, so the report's per-test numbers depend on test order and cannot say
which tests catch nothing.

### Why

The research says a full matrix is rarely wanted (PIT's fullMutationMatrix is academic), so keep it opt-in.

### Fix

```clojure
clojure -M:test:mutate TARGET -t TEST --every-failure  ;; slower: no stop at the first failure
```

### Notes

Off by default. With it, the 'By test' table can show caught counts per test.
<!-- SECTION:DESCRIPTION:END -->
