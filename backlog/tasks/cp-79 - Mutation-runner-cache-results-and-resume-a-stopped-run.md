---
id: CP-79
title: "Mutation runner: cache results and resume a stopped run"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/175"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A run that stops at a looping mutant starts from scratch, and a second run on an unchanged file repeats every mutant.

### Why

mutmut caches results and re-tests only changed functions; Ruby's mutant keeps session history.

### Fix

```clojure
;; cache each outcome by a hash of the form text + the test sources; skip cached mutants, resume after the stop point
```

### Notes

Merged 2026-11-23 as PR 175 (f36cd29) after 7 review rounds; ADR 0010 accepted. Cache in .mutate-cache/<ns>.edn,
rewritten whole after each outcome. Key: files git sees under the source and classpath folders, the -t namespaces, the
runner and parser source, -a/--timeout. A looping mutant runs last on the next run; --iterate reuses caught outcomes by
place (definition hash + position + swap) after a change; --fresh ignores the cache; progress tags (new / still open / N
closed) against the last reported run. Reagent: killed run resumes to the same 91%; all-cache run 2.4 s.

### Links

- cljs-patrol PR 175: https://github.com/olecve/cljs-patrol/pull/175

<!-- SECTION:DESCRIPTION:END -->
