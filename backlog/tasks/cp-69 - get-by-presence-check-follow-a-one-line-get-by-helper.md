---
id: CP-69
title: "get-by-presence-check: follow a one-line get-by helper"
status: Done
labels: ["react-testing-library-cljs"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/162"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

get-by-presence-check flags (is (some? (s/get-by-role ...))) but not (is (some? (get-item-1))) where get-item-1 is a
same-file helper that only calls s/get-by-role. The assertion can never fail either way.

### Why

A presence assertion through a lookup helper can currently bypass the rule.

### Flagged

```clojure
(defn- get-item-1 [] (s/get-by-role "radio" {:name "Item 1"}))
(is (some? (get-item-1)))
```

### Fix

```clojure
(is (some? (s/query-by-role "radio" {:name "Item 1"})))
```

### Left alone

```clojure
;; acting on the node: the throw is the assertion
(fire-event/click (get-item-1))
```

### Notes

Merged in cljs-patrol PR 162 (6 review rounds). get-by-presence-check follows a top-level defn/defn- of the same file
whose one arity returns a get-by-_/get-all-by-_ lookup. Not followed: helpers inside another form, helpers of another
namespace (#72), and a name the file defines more than once. Hint: "get-item-1 returns get-by-role, … Use query-item-1,
with query-item-1 returning query-by-role." Release pending.

### Links

- cljs-patrol PR 162: https://github.com/olecve/cljs-patrol/pull/162

<!-- SECTION:DESCRIPTION:END -->
