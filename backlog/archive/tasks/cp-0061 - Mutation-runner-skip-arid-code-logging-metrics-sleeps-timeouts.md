---
id: CP-0061
title: "Mutation runner: skip arid code (logging, metrics, sleeps, timeouts) (moved to mutacio-clj MC-0003)"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: "2026-11-28"
references: []
---

## Description

Cancelled here: the runner moved to [mutacio-clj](https://github.com/olecve/mutacio-clj) (ADR 0016), and this task is
MC-0003 there.

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Mutants in logging, metrics, Thread/sleep, timeout values and assert calls survive by design and bury the real gaps.

### Why

Google: with arid-node rules, the share of surfaced mutants developers marked useful went from about 15% to 89%
(Practical Mutation Testing at Scale, arXiv 2102.11378).

### Fix

```clojure
;; a node is arid when it is such a call, or all its children are arid
#{'log/info 'log/debug 'timbre/info 'println 'Thread/sleep 'assert ...}
```

### Notes

List skipped counts under 'not mutated, arid code'. Start with a small fixed set; make it configurable later.
<!-- SECTION:DESCRIPTION:END -->
