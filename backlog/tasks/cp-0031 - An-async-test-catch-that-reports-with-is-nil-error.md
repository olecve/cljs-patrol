---
id: CP-0031
title: "An async test catch that reports with (is (nil? error))"
status: Idea
labels: ["tests"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

In an ^:async deftest, a catch that reports a rejection with (is (nil? error)) or (is false …) records a :fail with the
error flattened, instead of an :error that keeps the error object.

### Why

Report unexpected rejections with :type :error so the test report retains the error object and stack.

### Flagged

```clojure
(catch :default error
  (is (nil? error)))
```

### Fix

```clojure
(catch :default error
  (cljs.test/do-report {:type :error :message "rejected" :expected nil :actual error}))
```

### Left alone

```clojure
;; a test that expects the rejection
(catch :default error
  (is (= "boom" (ex-message error))))
```

### Notes

Only inside a deftest marked ^:async, or a try around await.
<!-- SECTION:DESCRIPTION:END -->
