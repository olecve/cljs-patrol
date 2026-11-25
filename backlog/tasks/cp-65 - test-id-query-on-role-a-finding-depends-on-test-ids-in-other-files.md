---
id: CP-65
title: "test-id-query-on-role: a finding depends on test ids in other files"
status: Done
labels: ["react-testing-library-cljs"]
priority: high
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/160"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

In a two-file reproduction, first_test.cljs and second_test.cljs both render [:button {:data-testid "first"} "First"]
and [:button {:data-testid "last"} "Last"] and query both ids. 0.0.58 flagged only the "last" lookups, never "first".
After first_test.cljs dropped its test ids, the untouched second_test.cljs lookup of "last" was no longer reported
either.

### Why

A finding that changes when another file changes makes baselines move for no local reason, and the missing "first"
findings are false negatives.

### Flagged

```clojure
;; file A and file B render the same ids and query both
[:button {:data-testid "first"
          :type :button}
 "First"]
[:button {:data-testid "last"
          :type :button}
 "Last"]
(s/get-by-test-id "first") ;; never flagged (false negative)
(s/get-by-test-id "last")  ;; flagged, until file A drops its ids
```

### Fix

```clojure
;; each lookup flagged on its own, whatever other files do
(s/get-by-role "button" {:name "First"})
(s/get-by-role "button" {:name "Last"})
```

### Notes

Merged 2026-11-19 as PR 160 (merge commit ab3461a) after nine review rounds. A lookup whose test runs render! before it
reads its own test body plus everything outside the bodies of tests that render with render!; any other lookup reads the
whole run. Known limit: a test mixing render! with another mount. Follow-up: render! through a file's helper (item 67).
Released in v0.0.59.

### Links

- cljs-patrol PR 160 (merged): https://github.com/olecve/cljs-patrol/pull/160

<!-- SECTION:DESCRIPTION:END -->
