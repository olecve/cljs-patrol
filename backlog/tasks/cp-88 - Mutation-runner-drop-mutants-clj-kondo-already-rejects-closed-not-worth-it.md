---
id: CP-88
title: "Mutation runner: drop mutants clj-kondo already rejects (closed: not worth it)"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Some swaps produce code a linter rejects (wrong arity, unresolved symbol); they only cost a load and show as 'did not
compile'.

### Why

mutmut runs a type check and discards mutants that fail it; cargo-mutants says unviable mutants need no action.

### Fix

```clojure
;; lint the mutated form text with clj-kondo's API before eval; skip on :error findings
```

### Notes

Measured 2026-11-21 and closed without code. 4 of 485 mutants were unviable (all in our own swaps map and case table). A
failed load costs ~0.2 ms; a clj-kondo lint ~7 ms per mutant plus 2.1 s startup, so it would add ~5.5 s to save ~1 ms,
needs clj-kondo on the runner classpath, and a lint false positive could hide a real mutant.
<!-- SECTION:DESCRIPTION:END -->
