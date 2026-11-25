---
id: CP-72
title: "get-by-presence-check: follow get-by helpers of other namespaces"
status: Idea
labels: ["react-testing-library-cljs"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

get-by-presence-check follows only helpers of the same file (cljs-patrol PR 162). Shared test helpers usually live in
another namespace, such as a test-utils ns required by an alias, so (is (some? (helpers/get-dialog))) is not reported.

### Why

Found in round 2 of the review of PR 162. A presence check through a shared helper can never fail either.

### Flagged

```clojure
;; webapp/helpers.cljs
(defn get-dialog [] (s/get-by-role "dialog"))

;; webapp/dialog_test.cljs
(is (some? (helpers/get-dialog)))
```

### Fix

```clojure
;; webapp/helpers.cljs
(defn query-dialog [] (s/query-by-role "dialog"))

;; webapp/dialog_test.cljs
(is (some? (helpers/query-dialog)))
```

### Left alone

```clojure
;; acted on
(.click (helpers/get-dialog))
```

### Notes

Reviewer's idea: record lookup helpers as declarations keyed by ns and name, record presence-checked calls (resolved
through aliases and refers) as usages, and join them in analyze*. That also replaces the per-file scan and handles
redefinition order in one place.
<!-- SECTION:DESCRIPTION:END -->
