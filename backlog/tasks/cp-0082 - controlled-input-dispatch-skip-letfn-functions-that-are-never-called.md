---
id: CP-0082
title: "controlled-input-dispatch: skip letfn functions that are never called"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` walks the whole `:on-change` handler. It enters the bodies of functions a `letfn` defines,
so a `dispatch` inside a local function that the handler never calls is reported.

### Flagged

```clojure
;; reported today, though later! is never called
[:input {:value text
         :on-change (fn [event]
                      (letfn [(later! [] (rf/dispatch [::save]))]
                        (log event)))}]
```

### Fix

Skip `letfn` definitions in the ordinary walk, and read a local function's body only where the handler calls it.

### Left alone

A `letfn` function that the handler calls and that dispatches stays reported.

<!-- SECTION:DESCRIPTION:END -->
