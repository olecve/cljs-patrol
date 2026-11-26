---
id: CP-0052
title: "Mutation runner: number operators (0 <-> 1, n -> n+1)"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The swaps have no number operators, yet the survivors found so far sit on boundaries (>= vs >, inc/dec in focus-ring's
without-negations).

### Why

Infection swaps 0 and 1, mutmut adds 1 to integers. Boundary mistakes are the classic off-by-one bug.

### Fix

```clojure
;; 0 -> 1, 1 -> 0, n -> (inc n) for other integer literals
```

### Notes

Merged in PR 170 (27dd17a): integer literals in function code (defn/defn-/defmethod body, or a def whose value is a fn
or #()); never a definition name, a defmethod dispatch value or a ^:const def. Reasons in ADR 0006 (PR 171).
<!-- SECTION:DESCRIPTION:END -->
