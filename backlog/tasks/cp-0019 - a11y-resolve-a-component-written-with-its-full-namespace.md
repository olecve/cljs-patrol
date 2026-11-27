---
id: CP-0019
title: "a11y: resolve a component written with its full namespace"
status: Next
labels: ["a11y"]
created_date: ""
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The a11y component checks (:component-aliases, :label-components) resolve a component only through a :require alias. A
component written out with its full namespace is never matched, even when the file requires that namespace. So
[my.ui/select {}] stays unknown under {:component-aliases {my.ui/select :combobox}}, and [:div [my.ui/select {}]] gives
no finding. Found by the PR 156 review round 2 probes.

### Why

Code that writes full names (common in .cljc and in generated code) silently skips every alias-based a11y check, and
docs examples written that way pass for the wrong reason.

### Flagged

```clojure
;; config {:a11y {:component-aliases {my.ui/select :combobox}}}
(ns example.core (:require [my.ui]))

;; today: no finding, the alias is never matched
(defn picker [] [:div [my.ui/select {}]])
```

### Fix

```clojure
;; resolve my.ui/select as written: a fully qualified symbol names its namespace
;; expected: missing-accessible-name on [my.ui/select {}]
```

### Notes

Check how the tests group resolves a qualified symbol: through the aliases, else as a namespace name. Apply the same in
a11y's resolve-component-alias and the label-component lookup. Add a fixture with (:require [my.ui]) and a full name.
<!-- SECTION:DESCRIPTION:END -->
