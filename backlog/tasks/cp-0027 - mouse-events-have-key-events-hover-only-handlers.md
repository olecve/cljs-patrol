---
id: CP-0027
title: "mouse-events-have-key-events: hover-only handlers"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

An :on-mouse-over or :on-mouse-out with no :on-focus or :on-blur shows content to mouse users only. Ports jsx-a11y's
mouse-events-have-key-events. Decorative hover behavior may produce false positives.

### Why

Keyboard users miss hover-only previews and menus.

### Flagged

```clojure
[:div {:on-mouse-over show!} …]
```

### Fix

```clojure
[:div {:on-blur hide! :on-focus show! :on-mouse-over show!} …]
```

### Left alone

```clojure
;; purely decorative hover
[:div {:class (hover-style)} …]
```

### Notes

Overlaps with backlog CP-0010 (tooltip trigger focus).
<!-- SECTION:DESCRIPTION:END -->
