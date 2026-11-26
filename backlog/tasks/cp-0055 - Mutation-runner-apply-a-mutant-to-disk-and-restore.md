---
id: CP-0055
title: "Mutation runner: --apply a mutant to disk, and --restore"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

To write the test that kills a survivor, you have to make the swap by hand to see the test fail.

### Why

mutmut apply does this; it shortens the write-a-test loop.

### Fix

```clojure
;; clojure -M:test:mutate TARGET --apply 56:22  ; writes the mutant into the file, git shows the diff
```

### Notes

Merged in PR 168. `--apply 'ROW:COL:OPERATOR'` writes one mutant (atomic write, operator checked), keeps the text in
.mutate-applied.edn; `--restore` puts it back; runs wait while applied; commands printed quoted on console and page.
<!-- SECTION:DESCRIPTION:END -->
