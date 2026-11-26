---
id: CP-0049
title: "Mutation runner: --html report that leads with the test gaps"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/166"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Results are a console table and EDN only. There is no visual view of which lines were mutated, which mutants survived,
and which tests cover each one.

### Why

Asked for after the coverage work. The standard viewer (used by Stryker, also written by PIT and Infection) shows the
source with each mutant marked, the swap, the covering tests, and filters by status.

### Fix

```clojure
;; clojure -M:test:mutate TARGET -t TEST --html report.html
;; one self-contained page: report JSON inline + mutation-testing-elements@3.9.0 from jsDelivr
```

### Notes

Merged in PR 166 (ff6a4ee). Own page in plain words (planted bug, caught, test gap), no network; words and sources in
docs/mutation-testing.md.

### Links

- cljs-patrol PR 166: https://github.com/olecve/cljs-patrol/pull/166

<!-- SECTION:DESCRIPTION:END -->
