---
id: CP-91
title: "Mutation runner: keep a mutant's tests from writing into the repository"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/176"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A mutant can break a test's temp-folder helper so the test writes its fixtures into the working directory. A baseline
run on cljs-patrol.fs on 2026-11-21 left src/kept.clj, src/kept.cljs, src/skipped.txt, .hidden/, node_modules/thing,
target/classes/app and out/compiled.cljs in the repo root. A worse mutant could overwrite or delete real files.

### Why

A mutation run must not change the code it tests; today only the target form is restored, not files the tests touch.

### Fix

```clojure
;; run each mutant's tests with a scratch working directory (user.dir) or in a copy of the repo, or at least compare `git status --porcelain` before and after a run and warn about new or changed files
```

### Notes

Merged 2026-11-23 as PR 176 (5e91c32), ADR 0011 accepted. Cause: the body-to-nil mutant on fs/tmp-file-path made
fs_test's root nil, and (File. nil "src") wrote into the repo. Fix: tests take temp folders from cljs-patrol.temp-dir
(Files/createTempDirectory); the runner takes a snapshot (path, size, mtime, folders too) after every mutant whose tests
ran or that failed at load, and stops with exit 2 on any new, changed or deleted file, naming the mutant and dropping
its cache entry. Skips .git, the cache folder, tool caches and editor files. A Flight Recorder design was tried and
rejected (missed renames and copies, needed clock correction). Reagent cost about 1.8x on a busy machine. fs/tmp-dir and
fs/tmp-file-path removed.

### Links

- cljs-patrol PR 176: https://github.com/olecve/cljs-patrol/pull/176

<!-- SECTION:DESCRIPTION:END -->
