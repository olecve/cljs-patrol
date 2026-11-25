---
id: CP-59
title: "defclass-as-sole-attr: a map under a tag (#js {:class (x)}) is not a props map"
status: Done
labels: ["reagent"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/159"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

defclass-as-sole-attr counts {:class (x)} under a #js tag as a whole-props-map use, so a defclass whose only use builds
a JS attrs object (a ProseMirror Decoration.inline) is flagged. A defattrs there would need #js {:class (:class (x))}.

### Why

Tagged maps may require literal JavaScript object construction, so the rule must not suggest a worse representation.

### Flagged

```clojure
;; flagged today, wrongly
(defclass mark-style []
  {:background :yellow})

(.inline Decoration from to #js {:class (mark-style)})
```

### Fix

```clojure
;; left alone: a #js map is a JS object, not Hiccup props
```

### Notes

Merged 2026-11-19 as PR 159 (merge commit 990a699) after four review rounds. A {:class (x)} map directly under any tag
(also with metadata, #js ^:m {…}) is left alone: a tag reader may need the literal map. #?(:cljs {…}) and a clj->js
argument are still flagged; clj->js converts a defattrs result too. Next: release. Follow-ups on item 60.

### Links

- cljs-patrol PR 159 (merged): https://github.com/olecve/cljs-patrol/pull/159

<!-- SECTION:DESCRIPTION:END -->
