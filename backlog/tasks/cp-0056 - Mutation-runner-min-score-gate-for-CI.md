---
id: CP-0056
title: "Mutation runner: --min-score gate for CI"
status: Idea
labels: ["mutation-runner"]
milestone: m-0
created_date: ""
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Exit 1 on any survivor is too strict for a gate on a whole namespace with equivalent mutants.

### Why

Infection has --min-msi and --min-covered-msi. Lets CI gate on a score once the runner is used on PRs.

### Fix

```clojure
;; --min-score 90 --min-covered-score 95  ; exit 1 only below the thresholds
```

### Notes

Only useful once the runner runs in CI.
<!-- SECTION:DESCRIPTION:END -->
