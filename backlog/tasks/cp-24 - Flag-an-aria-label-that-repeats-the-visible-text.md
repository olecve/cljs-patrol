---
id: CP-24
title: "Flag an aria-label that repeats the visible text"
status: Next
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

An :aria-label equal to the element's only visible text is redundant. It overrides the content-derived name and drifts
from the visible text when one of them changes later.

### Why

Needs #14 so the rule can see text inside same-file components; without it the rule would push authors the wrong way.

### Flagged

```clojure
[:button {:aria-label "Save" :on-click f} "Save"]
[:button {:aria-label label} [item-caption {:label label}]]
```

### Fix

```clojure
[:button {:on-click f} "Save"]
```

### Left alone

```clojure
[:button {:aria-label "Save draft"} "Save"]   ; adds words
[:button {:aria-label "Close"} [icons/x]]     ; icon-only content
```

### Notes

Also detect aria-label values that duplicate a configured field component's label, as in [field {:label "Notes"}
[textarea {:aria-label "Notes"}]]. With :label-components the rule can see the field label, so it could flag this case
too.
<!-- SECTION:DESCRIPTION:END -->
