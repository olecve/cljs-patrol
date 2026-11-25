---
id: CP-96
title: "Recognize SVG tags in the tab-index rule"
status: Idea
labels: ["reagent", "a11y"]
created_date: "2026-11-25"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The CP-95 spelling rule can miss SVG elements when an earlier segment of a Reagent tag chain establishes SVG context.
The shared tag set also omits some elements that can be recognized inside an SVG, such as `switch` and
`feComponentTransfer`.

### Flagged

```clojure
[:svg>image {:href "/chart.svg" :tabIndex 0}]
["svg>g" [:image {:tabindex 0}]]
[:svg [:switch {:tabIndex 0}]]
[:svg [:filter [:feComponentTransfer {:tabindex 0}]]]
```

### Fix

Keep the final segment as the props target and let earlier SVG segments establish context for it and its children.
Recognize the relevant SVG-only tags within that context. The suggested spelling is `:tab-index`.

### Left alone

SVG-shaped vectors outside SVG markup may be data, including event IDs such as `:image`.

### Links

- [CP-95](cp-95%20-%20Prefer-tab-index-in-Reagent-DOM-Hiccup.md)

<!-- SECTION:DESCRIPTION:END -->
