---
id: CP-50
title: "Controlled text input that dispatches on every change (breaks CJK IME)"
status: Idea
labels: ["reagent"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A controlled :input or :textarea whose :value comes from re-frame state and whose :on-change dispatches loses characters
during CJK IME composition, because the round trip resets the value mid-composition.

### Why

Real user-facing bug for Chinese, Japanese and Korean input; a recurring pattern.

### Flagged

```clojure
[:textarea {:on-change #(dispatch [::set-text (.. % -target -value)])
            :value @(subscribe [::text])}]
```

### Fix

```clojure
;; keep local state and commit on blur, or use a textarea component that handles composition
[textarea {:on-blur commit! :value text}]
```

### Left alone

```clojure
[:input {:default-value v :on-blur commit!}]
```

### Notes

Recognise dispatch through helper functions as well as direct calls.
<!-- SECTION:DESCRIPTION:END -->
