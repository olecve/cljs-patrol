---
id: CP-13
title: "Prefer ->> over a multi-step transducer into []"
status: Idea
labels: ["idioms"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

(into [] (comp (mapcat f) (filter p)) xs) reads worse than the threaded form. into-empty deliberately leaves the
transducer form alone today.

### Flagged

```clojure
(into [] (comp (mapcat f) (filter p)) xs)
```

### Fix

```clojure
(->> xs (mapcat f) (filterv p))
```

### Left alone

```clojure
;; single-step or perf-sensitive transducers
(into [] (map f) xs)
```

### Notes

Check perf-sensitive cases and decide the step threshold before designing.
<!-- SECTION:DESCRIPTION:END -->
