---
id: CP-0051
title: "Mutation runner: skip marker for known equivalent mutants"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Equivalent mutants, such as first -> last on a one-item list, survive every run and crowd the survivor list.

### Why

Keeps the report actionable. mutmut has '# pragma: no mutate', cargo-mutants has #[mutants::skip].

### Fix

```clojure
;; mutate: skip
(first shadows)

;; or on a definition
(defn ^{:mutate/skip true} use-hint [shadows] ...)
```

### Notes

Merged in PR 167 (bc8700d). `; mutate: skip OPERATOR: REASON` (trailing, or own line for the next code in the same form)
and :mutate/skip "REASON" on a definition's name or defn attribute map. Ignored mutants do not run, are not scored, and
are listed with reasons in console, EDN and page; stale markers warn.
<!-- SECTION:DESCRIPTION:END -->
