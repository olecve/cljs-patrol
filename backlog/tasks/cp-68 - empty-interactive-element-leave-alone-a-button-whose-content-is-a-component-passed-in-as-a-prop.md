---
id: CP-68
title: "empty-interactive-element: leave alone a button whose content is a component passed in as a prop"
status: Done
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A button may render text through a component supplied as a parameter. empty-interactive-element reports the button as
empty because it cannot distinguish that component from an icon renderer.

### Why

Adding an :aria-label to silence it would hide the real content from screen readers.

### Flagged

```clojure
;; flagged today, wrongly: caption-component is a prop
(defn item-buttons [{:keys [caption-component items]}]
  (for [item items]
    [:button {:type :button}
     [caption-component {:item item}]]))
```

### Fix

```clojure
;; left alone: the content is a component the rule cannot see into
```

### Left alone

```clojure
;; still flagged: nothing inside the button
[:button {:type :button}]
[:button {:type :button} [:span]]
```

### Notes

Won't do (2026-11-20). The rule page decides this on purpose: "Any other child component, from another namespace or
bound in the function, such as [icon] with icon from the props, counts as icon markup. It may be a render function that
shows text, but a rule cannot tell it from an icon, and an icon-only button is the common bug. Such a button goes in the
baseline when its child shows text." Treating a local-headed child as content would also stop the rule from catching
[:button [icon]] with icon from the props.
<!-- SECTION:DESCRIPTION:END -->
