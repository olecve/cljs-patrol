---
id: CP-82
title: "Mutation runner: replace a whole function body with nil"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Operator swaps need an operator; a function with none, or one no test checks at all, is easy to miss.

### Why

cargo-mutants replaces function bodies with default values: one mutant per function that finds untested functions fast.

### Fix

```clojure
;; (defn f [x] (body…))  ->  (defn f [x] nil)
```

### Notes

Merged in PR 174: one body-to-nil mutant per arity (defn, defn-, defmethod, def of fn); condition maps kept; --apply
picks by place and operator; -s by any body line. Proposed ADR 0009 (draft on the PR 171 branch).
<!-- SECTION:DESCRIPTION:END -->
