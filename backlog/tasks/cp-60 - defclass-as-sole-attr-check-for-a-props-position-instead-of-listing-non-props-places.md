---
id: CP-60
title: "defclass-as-sole-attr: check for a props position instead of listing non-props places"
status: Idea
labels: ["reagent"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The rule decides that {:class (x)} is the whole props map by excluding places that are not props: merge, merge-props,
read-as-data forms, and (PR 159) tagged maps. Every new non-props consumer needs one more special case, such as js-obj,
interop calls or threading.

### Why

Checking that the map sits in a props slot (the second element of a Hiccup tag vector, or a component's props argument)
would cover the unknown cases at once, and miss rather than wrongly flag.

### Notes

Raised by the round 3 review of cljs-patrol PR 159. Needs a corpus run first: how many current findings sit outside a
props slot. Related gap (PR 159 round 4): heads/site treats any reader macro around a list or vector as a reader
conditional, so #ui/props [{:class (x)}] is still flagged while #ui/props {:class (x)} is not; heads/site should climb
only through #? and #?@ (check the ? token), which touches other rules.
<!-- SECTION:DESCRIPTION:END -->
