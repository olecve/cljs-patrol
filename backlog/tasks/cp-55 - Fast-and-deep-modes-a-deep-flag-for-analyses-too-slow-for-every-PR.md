---
id: CP-55
title: "Fast and deep modes: a --deep flag for analyses too slow for every PR"
status: Idea
labels: ["core"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The per-file, syntax-level reading keeps missing the same shapes: props maps, defs and aliases from another namespace;
data flow inside a function (cond->, a path kept in a let, values passed through if/let, alias chains); whether a ^js
hint matters, which only the build's externs and :infer-warnings know; and pass-through wrapper components that render a
native element with the caller's props. Closing them per file is either impossible or too slow for the PR gate.

### Why

Keep the default analysis fast enough for pull-request checks, including on resource-constrained runners, while giving
rules like js-hint-in-test and button-missing-type the facts they now guess at.

### Notes

Shape: a --deep flag on the same rules, not separate rules: a rule keeps its key, meaning and fix, and deep mode reads
more, so it finds a superset. Gate PRs on fast only; run deep nightly on main or on demand, as a report or tickets.
Baselines must not mix modes: keep one per mode, or mark deep-only findings and never gate on them. Build one
project-wide index per run (namespaces, defs, components and the props they pass through), shared by every rule. Cost:
rule docs and the docs-example tests cover both modes. Start with an ADR (flag, gating, baseline policy, options,
revisit trigger), then pilot one analysis: cross-file props resolution, the most common miss and useful to several
rules. Related: backlog item 39 (^js hints), 54 (cond->).
<!-- SECTION:DESCRIPTION:END -->
