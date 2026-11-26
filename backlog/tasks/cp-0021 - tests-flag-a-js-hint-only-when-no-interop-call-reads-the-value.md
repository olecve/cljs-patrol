---
id: CP-0021
title: "tests: flag a ^js hint only when no interop call reads the value"
status: Done
labels: ["tests"]
priority: high
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/157"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

js-hint-in-test flagged every ^js hint in test code, saying nothing reads it outside :advanced. The compiled output does
not change, but shadow-cljs reads the hint in every build to decide whether to print an :infer-warning, so removing
hints on values that interop reads filled test build logs with warnings.

### Why

Keeps the hints that silence real :infer-warnings and still removes the ones that do nothing.

### Flagged

```clojure
;; nothing reads el through interop
(let [^js el (render-chart)]
  (is (some? el)))
```

### Fix

```clojure
(let [el (render-chart)]
  (is (some? el)))
```

### Left alone

```clojure
;; .-width reads el
(let [^js el (render-chart)]
  (is (= 10 (.-width el))))

;; inline on the target
(.-textContent ^js (render-chart))

;; threaded into a property read
(-> el .-dataset .-pendingOp)

;; return hint
(defn- ^js load-canvas [] (render-chart))
```

### Notes

Merged 2026-11-17 as PR 157 (merge commit bc0892a) after five code review rounds. The rule flags a ^js hint only when it
is sure nothing reads it: a name a known form binds whose every use is a plain call argument or literal element, or such
a value itself. It leaves a hint alone wherever the value can be handed on (core macros, are/async/testing, def-/with-
heads, macros the ns form names, tails, def values, unknown bindings like p/let). Still reports unread hints while
preserving hints used by interop. Next: release.

### Links

- PR 157: https://github.com/olecve/cljs-patrol/pull/157

<!-- SECTION:DESCRIPTION:END -->
