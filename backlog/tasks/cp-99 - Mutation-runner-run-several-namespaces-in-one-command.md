---
id: CP-99
title: "Mutation runner: run several namespaces in one command"
status: Idea
labels: ["mutation-runner"]
created_date: "2026-11-25"
updated_date: "2026-11-25"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The runner takes one target namespace per command, and the caller must name its test namespaces. A run over many
namespaces, as in CP-97 or CP-98, needs a script around it.

### Why

One command that finds the test namespaces for each target and prints one summary makes the full run and the CI report
simple.

### Fix

```text
clojure -M:test:mutate --all src -s origin/main
;; each target gets the test namespaces that require it, or <target>-test by name
```

### Notes

Keep one JVM per target, so a stuck mutant in one namespace does not stop the others, and so the file guard checks each
run on its own.
<!-- SECTION:DESCRIPTION:END -->
