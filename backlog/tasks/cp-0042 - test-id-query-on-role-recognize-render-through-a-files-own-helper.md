---
id: CP-0042
title: "test-id-query-on-role: recognize render! through a file's own helper"
status: Next
labels: ["react-testing-library-cljs"]
created_date: ""
updated_date: "2026-11-27"
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

PR 160 narrows a lookup's evidence to its own test only when react-testing-library-cljs's render! runs in the test body
before it. Most real test files render through a file-local helper, such as (defn- render-panel! [] (render! [panel])),
or a fixture, so those tests read the whole run and keep the old behavior.

### Why

Most of the narrowing's value sits in tests that render through helpers.

### Flagged

```clojure
;; read on its own body today only when render! is written in the test
(defn- render-save! []
  (render! [:button {:data-testid "save"} "Save"]))

(deftest saves
  (render-save!)
  (s/get-by-test-id "save"))
```

### Notes

Resolve a call in the test body to a same-file defn whose body runs render! unconditionally (same statement rules as in
the test), and count it as a render at that call's position. Fixtures with :each and a render! are a further step.
Raised by the round 8 review of PR 160. Related known limit (documented in the rule page): a test that calls render! and
also mounts another way still drops out of other tests' evidence.
<!-- SECTION:DESCRIPTION:END -->
