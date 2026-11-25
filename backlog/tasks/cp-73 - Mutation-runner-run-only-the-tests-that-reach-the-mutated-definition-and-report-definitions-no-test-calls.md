---
id: CP-73
title: "Mutation runner: run only the tests that reach the mutated definition, and report definitions no test calls"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/164", "https://github.com/olecve/cljs-patrol/pull/165"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Each mutant runs the full test namespaces until the first failure. A definition that no test calls still gets one
survivor per operator, so the report lists many survivors where one fact would do: no test calls it.

### Why

Idea borrowed from cljest's design (coverage-guided test selection). Gives a clearer report and faster runs on slow
suites.

### Flagged

```clojure
;; today: 8 survivors in blocks, one per operator
  survived  133:36  some -> every?  in blocks
  survived  137:53  every? -> some  in blocks
  ...
```

### Fix

```clojure
;; after: one coverage pass maps each definition to the deftests that call it
Not covered, no test calls: blocks
;; each other mutant runs only the deftests mapped to its definition
```

### Left alone

```clojure
;; a definition no test reached through its var, such as a fn stored in a map at load time,
;; still runs the full suite, and is reported as not covered
```

### Notes

Merged in cljs-patrol PR 165 (4 review rounds). Coverage run records which tests call each function (all running tests,
across threads); each mutant runs those tests with their own timeout; fallbacks to every test for unrecordable
functions, fixture calls, test-ns-hook, self-failing subsets, --all-tests. Fifth outcome 'no coverage', two scores. ADR
0005 accepted 2026-11-21.

### Links

- cljs-patrol PR 164 (runner): https://github.com/olecve/cljs-patrol/pull/164
- cljs-patrol PR 165 (coverage): https://github.com/olecve/cljs-patrol/pull/165

<!-- SECTION:DESCRIPTION:END -->
