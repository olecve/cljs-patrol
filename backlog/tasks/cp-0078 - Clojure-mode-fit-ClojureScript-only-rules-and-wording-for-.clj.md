---
id: CP-0078
title: "Clojure mode: fit ClojureScript-only rules and wording for .clj"
status: Idea
labels: ["core", "tests"]
milestone: m-1
created_date: "2026-11-27"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Rules that only make sense in ClojureScript still run on `.clj` files, and some messages assume ClojureScript.

### Why

A finding that means nothing on the JVM costs the user time and makes the mode look unfinished.

### Fix

Apply the decisions from CP-0077: turn off the rules that do not apply to `.clj`, such as `js-hint-in-test`, and adjust
the messages and hints that name ClojureScript where the rule also holds for Clojure. Add a `.clj` example to each rule
doc that applies, as an executable example.

### Order

After CP-0077.

<!-- SECTION:DESCRIPTION:END -->
