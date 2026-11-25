---
id: CP-97
title: "Mutation runner: score every source namespace once"
status: Next
labels: ["mutation-runner"]
created_date: "2026-11-25"
updated_date: "2026-11-25"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Scores exist only for the lines that recent changes touched, because each change runs the runner with `-s`. The one
attempt at a full run stopped after a few namespaces, so nobody knows where the large test gaps are.

### Why

A score per namespace shows where tests are weakest, and it gives a starting point for a CI gate (CP-81).

### Fix

```text
;; one run per namespace under src/, one at a time, each with its own test namespaces
clojure -M:test:mutate cljs-patrol.groups.a11y -t cljs-patrol.groups.a11y-test --html target/mutate/a11y.html
```

Collect each run's EDN report into one table: namespace, score, survivors, no coverage, time. Keep the table in `docs/`
or in a generated report, and open one task per namespace with real gaps.

### Notes

Run namespaces one at a time: the runner uses one JVM per run, and the file guard of ADR 0011 must see each run on its
own. The cache of ADR 0010 makes a second pass cheap.
<!-- SECTION:DESCRIPTION:END -->
