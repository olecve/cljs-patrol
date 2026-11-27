---
id: CP-0077
title: "Clojure mode: decide which rules apply to .clj files"
status: Next
labels: ["core", "docs"]
milestone: m-1
created_date: "2026-11-27"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`--experimental-clj` lets five groups read `.clj` files: `docstrings`, `tests`, `layout`, `idioms` and `comments`. The
rules in them were written against ClojureScript code. Some do not hold on the JVM as written: `js-hint-in-test` is
about `^js` externs inference, and `var-deref-in-test` flags `@#'private-fn`, which is a common idiom in Clojure tests.

### Why

A supported Clojure mode needs a clear list of what it checks. Without one, users cannot tell which findings on `.clj`
files are meant and which are a side effect of rules written for ClojureScript.

### Fix

Go through every rule of the five groups and decide for `.clj`: applies as is, applies with other wording, or is off.
Record the scope and the reasons in an ADR, and put the per-rule result in each group's doc page.

### Notes

Check each rule on an open-source Clojure project before deciding, and keep the examples invented. Leave the groups that
read Hiccup, re-frame or Spade on `.cljs`/`.cljc` only.

<!-- SECTION:DESCRIPTION:END -->
