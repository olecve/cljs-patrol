---
id: CP-0065
title: "Mutation runner: optional run of every selected test per mutant"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

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
