---
id: CP-0045
title: "Share one reader of top-level forms and reader-conditional branches across groups"
status: Idea
labels: ["spade", "tests", "focus-ring"]
created_date: ""
updated_date: "2026-11-25"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Three groups unwrap top-level forms and pick reader-conditional branches in their own way, and the three disagree:

- spade.clj declared-style-names reads every branch.
- tests.clj reader-conditional-branch leaves out other platforms (other-platforms).
- focus_ring.clj top-level-forms reads the first :cljs or :default branch and looks under metadata. focus_ring.clj
  style-declaration? also repeats style-blocks/style-decl?, which lacks defglobal.

### Why

A fix to one copy does not reach the others. Found in round 5 of the review of the custom-property focus-ring change (PR
161).

### Fix

```clojure
;; one shared helper, for example in parser or style-blocks
(defn top-level-forms [root] …)       ; unwraps metadata and reader conditionals
(defn clojurescript-branch [loc] …)    ; the branch a ClojureScript build reads
;; style-blocks/style-decl? covers defglobal; focus-ring uses it
```

### Notes

Decide one branch policy per use: style declarations care about the branch ClojureScript reads; the tests group cares
about which branches may compile. Moving spade.clj onto the shared helper changes which style names it sees, so run it
against a real codebase before and after.

PR #185 added a shared branch selector in `cljs-patrol.parser` for the colors and focus-ring groups. PR #186 lets
callers pass a set of active features and uses it for color findings in `.cljc` files. Sharing the top-level form
traversal and updating the tests and spade groups are still open.

### Links

- [Shared branch selector](https://github.com/olecve/cljs-patrol/pull/185)
- [Reader-conditional follow-up](https://github.com/olecve/cljs-patrol/pull/186)

<!-- SECTION:DESCRIPTION:END -->
