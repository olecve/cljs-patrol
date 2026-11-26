---
id: CP-0015
title: "One shared accessible-text reader for all a11y text checks"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/153"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

PR 153 taught visible-content? (the empty-interactive-element check) to skip text inside aria-hidden elements and to
read text that same-file components render. Other text readers in the a11y group, such as announces-something? for table
captions and renders-something?, still count hidden text and do not look into local components. The rules now disagree
about the same markup.

### Why

A hidden span names nothing in a button but still names a table through its caption. One reader would keep every rule
consistent and get future fixes once.

### Flagged

```clojure
;; counts as named today, though the caption text is hidden
[:table {:role "grid"}
 [:caption [:span {:aria-hidden true} "Stats"]]
 …]
```

### Fix

```clojure
;; extract one accessible-text reader (hidden subtrees, local components,
;; caller arguments) and use it from every rule that asks "does this have text?"
```

### Left alone

```clojure
[:table {:role "grid"} [:caption "Stats"] …]
```

### Blocked by

CP-0004 (PR 153)

### Notes

Found in the PR 153 review, round 2. Do after PR 153 merges, so the reader starts from its visible-content? logic.

### Links

- cljs-patrol PR 153: https://github.com/olecve/cljs-patrol/pull/153

<!-- SECTION:DESCRIPTION:END -->
