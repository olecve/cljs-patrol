---
id: CP-87
title: "Mutation runner: show the top gaps as before/after lines in the terminal"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The console report lists survivors by line only; small runs need a browser to see what changed.

### Why

Infection prints mutation diffs by default (--show-mutations); cargo-mutants prints only missed mutants and timeouts.

### Fix

```clojure
490  (seq (remove #(= ...
  -> (seq (filter #(= ...
…
HTML report: target/mutate.html
```

### Notes

Merged in PR 173 (d388a19): survivors grouped by line, before/after lines for the first 10 lines, long lines cut around
the swap, pointer only to reports not written.
<!-- SECTION:DESCRIPTION:END -->
