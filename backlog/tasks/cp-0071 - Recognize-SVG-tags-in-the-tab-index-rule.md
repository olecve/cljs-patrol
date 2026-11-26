---
id: CP-0071
title: "Recognize SVG tags in the tab-index rule"
status: Done
labels: ["reagent", "a11y"]
created_date: "2026-11-25"
updated_date: "2026-11-26"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Closed

Superseded. [ADR 0015](../../docs/adr/0015-replace-prefer-tab-index-with-forbid-props-config.md) removed the CP-0070
rule before a release. The `:native` target of `forbid-props` now checks the spelling, and
[CP-0075](cp-0075%20-%20forbid-props-know-every-SVG-tag-for-native-and-disallowed-for.md) covers the SVG tag gap. The
examples below moved to CP-0075.

### Problem

The CP-0070 spelling rule can miss SVG elements when an earlier segment of a Reagent tag chain establishes SVG context.
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

- [CP-0070](cp-0070%20-%20Prefer-tab-index-in-Reagent-DOM-Hiccup.md)

<!-- SECTION:DESCRIPTION:END -->
