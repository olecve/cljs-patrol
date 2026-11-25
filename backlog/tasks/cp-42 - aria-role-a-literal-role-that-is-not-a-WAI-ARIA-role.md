---
id: CP-42
title: "aria-role: a literal :role that is not a WAI-ARIA role"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
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
<!-- SECTION:DESCRIPTION:END -->
