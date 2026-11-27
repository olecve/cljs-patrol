---
id: CP-0024
title: "aria-role: a literal :role that is not a WAI-ARIA role"
status: Done
labels: ["a11y"]
created_date: ""
updated_date: "2026-11-27"
references: ["https://github.com/olecve/cljs-patrol/pull/191"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A :role value that is not a WAI-ARIA role, or an abstract one such as "widget", is ignored by assistive tech, so the
element is announced as if it had no role. Ports jsx-a11y's aria-role.

### Why

Small rule; catches typos like "buton" and made-up roles.

### Flagged

```clojure
[:div {:role "buton"} …]
[:div {:role "widget"} …]
```

### Fix

```clojure
[:div {:role "button" :tab-index 0} …]
```

### Left alone

```clojure
[:div {:role "group" :aria-label "Filters"} …]
[:div {:role "presentation"} …]
```

### Notes

Needs the ARIA 1.2 role table; the same table later serves role-has-required-aria-props and role-supports-aria-props.

### Done

`invalid-role` flags unknown, abstract and non-lowercase role tokens, and checks each token of a fallback list. The role
table is `cljs-patrol.groups.a11y.roles`. The same change adds an opt-in `{:a11y {:role-value :string}}` setting, with
the values `:string`, `:keyword` and `:mixed`, and a `role-value-spelling` rule that reports the other spelling of a
literal `:role`, and a namespaced keyword in every mode.
<!-- SECTION:DESCRIPTION:END -->
