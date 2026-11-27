---
id: CP-0058
title: "Mutation runner: triage the 13 focus-ring survivors"
status: Idea
labels: ["mutation-runner", "tests", "focus-ring"]
created_date: ""
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

focus-ring has 13 surviving mutants (score 92%). None is triaged: each is either a test gap or an equivalent mutant.

### Why

The first real job for the runner; the boundary survivors in without-negations (>= -> >, inc/dec) may hide an
off-by-one.

### Fix

```clojure
;; per survivor: a test that kills it, or a skip marker (CP-0051) with the reason
```

### Notes

Survivors: without-negations 56:22, 60:40, 61:40; blocks 133:36, 137:53; unreadable-part? 174:25, 175:15; entry-value
219:8; outline-holds? 260:26; use-hint 306:18; clojurescript-branch 350:30, 354:12; read-approved-shadows 627:58.
<!-- SECTION:DESCRIPTION:END -->
