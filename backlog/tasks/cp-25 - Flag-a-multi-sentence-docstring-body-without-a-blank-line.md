---
id: CP-25
title: "Flag a multi-sentence docstring body without a blank line"
status: Idea
labels: ["docstrings"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A docstring whose summary line is followed directly (no blank line) by two or more sentences makes the summary hard to
read as a summary. cljs-patrol's own CLAUDE.md has this rule, but nothing checks it.

### Why

A review of PR 153 caught it by hand in cljs-patrol's own source.

### Flagged

```clojure
(defn f
  "Reads the name of a control.
  A vector calling a local component is also read. Hidden text carries nothing."
  [x] …)
```

### Fix

```clojure
  "Reads the name of a control.

  A vector calling a local component is also read. Hidden text carries nothing."
```

### Left alone

```clojure
;; summary alone; summary plus exactly one continuation sentence; any body after a blank line
```

### Notes

Sentence split: '. ' followed by an uppercase letter or backtick, ignoring e.g./i.e. and code spans. Dogfood on
cljs-patrol's own sources.
<!-- SECTION:DESCRIPTION:END -->
