---
id: CP-0020
title: "idioms: merge consecutive assoc (and dissoc, disj, conj) calls into one"
status: Idea
labels: ["idioms"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Code chains single-pair calls where one call takes them all: (-> m (assoc :a "a") (assoc :b "b")) or (assoc (assoc m
:a 1) :b 2). assoc applies its pairs left to right, so (assoc m :a "a" :b "b") returns the same map, even when a key
repeats. The same holds for dissoc, disj and conj, which all take several arguments.

### Why

One call says the same with less noise, and the rewrite is always safe, so false positives stay near zero. Tier:
cleanup.

### Flagged

```clojure
;; consecutive assoc steps in a thread
(-> m (assoc :a "a") (assoc :b "b"))

;; nested assoc
(assoc (assoc m :a 1) :b 2)

;; the same family
(-> s (disj :a) (disj :b))

;; some-> too: assoc never returns nil
(some-> m (assoc :a 1) (assoc :b 2))
```

### Fix

```clojure
(-> m (assoc :a "a" :b "b"))

(assoc m :a 1 :b 2)

(disj s :a :b)

(some-> m (assoc :a 1 :b 2))
```

### Left alone

```clojure
;; another step sits between them
(-> m (assoc :a 1) (update :n inc) (assoc :b 2))

;; cond-> steps carry their own conditions
(cond-> m a? (assoc :a 1) b? (assoc :b 2))

;; assoc-in takes one path
(-> m (assoc-in [:a :x] 1) (assoc-in [:b :y] 2))

;; ->> puts the map last, so assoc does not apply
(->> m (map f))

;; merge is a different shape
(-> m (merge a) (merge b))
```

### Notes

Before naming the rule and writing the hint, check prior art: Kibit may have a nested-assoc rule and Splint something
close; match their wording if so. Decide whether one rule key covers assoc, dissoc, disj and conj, or one key per
function. Suggested 2026-11-17.
<!-- SECTION:DESCRIPTION:END -->
