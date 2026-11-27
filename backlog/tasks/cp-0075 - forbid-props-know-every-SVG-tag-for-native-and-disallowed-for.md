---
id: CP-0075
title: "forbid-props: know every SVG tag for :native and :disallowed-for"
status: Next
labels: ["forbid-props"]
created_date: "2026-11-26"
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The group's tag list omits SVG tags such as `feTurbulence`, `feImage`, `switch`, `view` and `mpath`. The `:native`
target does not match these tags. The group refuses a `:disallowed-for` collection that names one.

### Why

A prop policy that uses `:native` can miss props on valid SVG elements. Users cannot select these elements with an
explicit `:disallowed-for` collection either.

### Fix

Add all SVG tags to the tag list that `forbid-props` uses for matching and validation. Add tests for the `:native`
target and explicit `:disallowed-for` collections. Update the rule documentation to describe the supported tags.

### Notes

Check ambiguous tag names that can also identify data. Keep the element reader and configuration validation consistent.

Let an earlier SVG segment of a tag chain, or an SVG parent, give SVG context to the tags after it. Examples that the
`:native` target should match:

```clojure
[:svg>image {:href "/chart.svg" :tabIndex 0}]
["svg>g" [:image {:tabindex 0}]]
[:svg [:switch {:tabIndex 0}]]
[:svg [:filter [:feComponentTransfer {:tabindex 0}]]]
```

Outside SVG markup, the same vectors can be data, for example an event ID such as `:image`.

<!-- SECTION:DESCRIPTION:END -->
