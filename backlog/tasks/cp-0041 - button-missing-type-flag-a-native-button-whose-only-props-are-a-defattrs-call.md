---
id: CP-0041
title: "button-missing-type: flag a native button whose only props are a defattrs call"
status: Done
labels: ["a11y"]
created_date: ""
updated_date: "2026-11-27"
references: ["https://github.com/olecve/cljs-patrol/pull/192"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

button-missing-type reads only literal props maps. [:button (example-style) "Save"] passes a defattrs call as the whole
props map, so the rule treats the props as unknown and stays silent. A defattrs returns only {:class …}, so such a
button has no type for certain.

### Why

The spade group already knows which calls are defattrs, so this closes a false negative without guessing; the same
reading would help every attrs rule that skips an unreadable props map.

### Flagged

```clojure
;; flagged: a defattrs result is only {:class …}
(defattrs example-style []
  {:color :gray})

[:button (example-style)
 "Save"]
```

### Fix

```clojure
;; fix
(defclass example-style []
  {:color :gray})

[:button {:class (example-style)
          :type :button}
 "Save"]
```

### Left alone

```clojure
;; left alone: a plain function may return any props
[:button (button-props)
 "Save"]

;; left alone: the call is merged with props that may hold :type
[:button (merge (example-style) props)
 "Save"]
```

### Notes

Resolve the call through :require aliases and same-file declarations to a defattrs (the spade group's declaration
index). Count it as the attrs map {:class …} only when the call is the whole props position; anything that changes or
merges it stays unknown. Check whether other attrs rules (empty-interactive-element, missing-accessible-name) should
read it the same way.
<!-- SECTION:DESCRIPTION:END -->
