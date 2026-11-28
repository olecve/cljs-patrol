---
id: CP-0083
title: "controlled-input-dispatch: resolve a qualified comment form"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` skips a `(comment ...)` form in the handler only when it is written as `comment`. A form
written as `cljs.core/comment` is walked, so a `dispatch` inside it, which never runs, is reported.

### Flagged

```clojure
;; reported today, though comment discards its body
[:input {:value text
         :on-change (fn [event]
                      (cljs.core/comment (rf/dispatch [::save]))
                      (log event))}]
```

### Fix

Resolve the head through the namespace information, as other heads are resolved, before deciding to skip a comment form.

### Notes

Check whether other rules in the group recognize `comment` by its raw name only, and share one check.

<!-- SECTION:DESCRIPTION:END -->
