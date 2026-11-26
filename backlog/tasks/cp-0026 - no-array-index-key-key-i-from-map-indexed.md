---
id: CP-0026
title: "no-array-index-key: ^{:key i} from map-indexed"
status: Idea
labels: ["reagent"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A key taken from the item's index makes React reuse the wrong component when items are inserted, removed or reordered,
so state and focus jump between rows. Ports react's no-array-index-key.

### Why

A real bug class in editable lists; medium false positives on static lists that never reorder.

### Flagged

```clojure
(map-indexed (fn [i item] ^{:key i} [row item]) items)
```

### Fix

```clojure
(for [item items] ^{:key (:id item)} [row item])
```

### Left alone

```clojure
;; a static list that never changes order
(map-indexed (fn [i label] ^{:key label} [:li label]) labels)
```

### Notes

Pair with jsx-key (a missing :key in a seq of Hiccup). Consider cleanup tier because of static lists.
<!-- SECTION:DESCRIPTION:END -->
