---
id: CP-0081
title: "controlled-input-dispatch: read only the helper arity the handler calls"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` follows a same-file helper that the `:on-change` handler calls, and reads every arity of it.
A helper whose called arity uses `dispatch-sync` while another arity uses `dispatch` is reported, although the handler
never reaches the `dispatch`.

### Flagged

```clojure
;; reported today, though the one-argument arity is the one called
(defn save!
  ([value] (rf/dispatch-sync [::save value]))
  ([value options] (rf/dispatch [::save value options])))

[:input {:value text
         :on-change #(save! (.. % -target -value))}]
```

### Fix

Carry the argument count of the call through to the helper, and read only the matching fixed or variadic arity.

### Left alone

A helper whose called arity dispatches stays reported.

<!-- SECTION:DESCRIPTION:END -->
