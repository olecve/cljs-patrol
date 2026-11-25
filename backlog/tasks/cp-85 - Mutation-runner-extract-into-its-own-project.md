---
id: CP-85
title: "Mutation runner: extract into its own project"
status: Idea
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The runner lives in cljs-patrol's dev/ and is usable only there.

### Why

Plan: extract it once it is stable and useful outside cljs-patrol (ADR 0005 Revisit when).

### Fix

```clojure
;; own repo + deps.edn (rewrite-clj, tools.cli); copy the 5 parser helpers
:mutate {:extra-deps {io.github.olecve/mutate {:git/tag "v0.1.0" :git/sha "…"}}
         :main-opts ["-m" "mutate.main"]}
```

### Notes

Good point to do it: after --html (#74) and a few cljs-patrol PRs used it without surprises.
<!-- SECTION:DESCRIPTION:END -->
