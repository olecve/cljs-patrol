---
id: CP-0084
title: "controlled-input-dispatch: read only the ClojureScript branch of a reader conditional"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` walks every branch of a reader conditional in the `:on-change` handler. In a `.cljc` file, a
`dispatch` in the `:clj` branch is reported, although ClojureScript runs only the `:cljs` branch.

### Flagged

```clojure
;; reported today, though ClojureScript runs dispatch-sync
[:input {:value text
         :on-change (fn [event]
                      #?(:clj (rf/dispatch [::set-text event])
                         :cljs (rf/dispatch-sync [::set-text event])))}]
```

### Fix

Select the active ClojureScript or `:default` branch of `#?` and `#?@` before looking for a `dispatch`, with the shared
reader-conditional helpers in the parser.

### Left alone

A `dispatch` in the branch ClojureScript reads stays reported.

<!-- SECTION:DESCRIPTION:END -->
