---
id: CP-0085
title: "controlled-input-dispatch: follow a handler bound in with-let"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` follows a handler bound by `let`, but not one bound by Reagent's `with-let`. The input below
is missed, while the same code with `let` is reported.

### Flagged

```clojure
;; missed today
(r/with-let [changed! #(rf/dispatch [::set-text %])]
  [:input {:value text
           :on-change changed!}])
```

### Fix

Resolve the handler's binding with namespace information, so a resolved `reagent.core/with-let` binding is read like a
`let` binding.

### Left alone

A look-alike `x/with-let` keeps today's behavior.

<!-- SECTION:DESCRIPTION:END -->
