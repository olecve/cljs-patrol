---
id: CP-0059
title: "Mutation runner: tests for the 3 remounting-branch gaps"
status: Idea
labels: ["mutation-runner", "tests", "reagent"]
milestone: m-0
created_date: ""
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The runner found three untested branches in the remounting-branch rule (PR 163).

### Why

Real test gaps the manual mutation check missed.

### Fix

```clojure
;; #_ inside a for body; a for body with several forms; when/if branches that differ in focus
```

### Notes

holds-focus? survivors: remove->filter and = -> not= on #_ handling (490), last -> first on mapping bodies (491), some
-> every? on yielded results (493).
<!-- SECTION:DESCRIPTION:END -->
