---
id: CP-0080
title: "aria-hidden-focusable: keep reports inside a modal dialog under an inert ancestor"
status: Idea
labels: ["a11y"]
created_date: "2026-11-27"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`aria-hidden-focusable` leaves alone an element that an inert HTML ancestor renders in place. A `<dialog>` opened with
`.showModal()` escapes the inertness of its ancestors, so a control inside it stays focusable, and the rule misses a
real defect there.

### Flagged

```clojure
;; missed today: the modal dialog escapes the inert ancestor
[:div {:inert true}
 [:dialog {:ref #(when % (.showModal %))}
  [:button {:aria-hidden true} "Close"]]]
```

### Left alone

```clojure
;; a dialog that is not modal stays inert with its ancestor
[:div {:inert true}
 [:dialog {:open true}
  [:button {:aria-hidden true} "Close"]]]
```

### Fix

Stop the inert-ancestor walk at a `:dialog` that may be shown as modal. The tool cannot see `.showModal()` from the
markup alone, so decide what counts: any `:dialog` without a literal `:open`, a `:ref` or other code that calls
`showModal`, or every `:dialog`. Treating every `:dialog` as a boundary is the simplest choice and only keeps reports.

### Notes

Found in the review of the change that taught the rule to read `:inert` and `:hidden`. Check whether a `:hidden`
ancestor needs the same boundary.

<!-- SECTION:DESCRIPTION:END -->
