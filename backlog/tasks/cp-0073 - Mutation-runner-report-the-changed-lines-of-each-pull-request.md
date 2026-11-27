---
id: CP-0073
title: "Mutation runner: report the changed lines of each pull request"
status: Idea
labels: ["mutation-runner"]
created_date: "2026-11-25"
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`CLAUDE.md` asks that every branch a change adds has a test that fails when the branch breaks. Today the author runs the
runner by hand with `-s`, and a reviewer cannot see the result.

### Why

A report on each pull request shows the survivors on the lines it changed, so the review sees a test gap before merge.

### Fix

```text
;; CI job on pull requests, not a gate at first
clojure -M:test:mutate <each changed src namespace> -t <its test namespaces> -s origin/main --html mutate.html
```

Upload the HTML report as an artifact, and print the survivors in the job summary.

### Notes

Start as a report that never fails the build. Make it a gate later, with CP-0056. Check the job time on the CI runner
first: a namespace with many changed lines can take minutes.

### Order

After CP-0072 and the fresh triage of CP-0058 and CP-0059. CP-0074 shares the work of running several namespaces.

<!-- SECTION:DESCRIPTION:END -->
