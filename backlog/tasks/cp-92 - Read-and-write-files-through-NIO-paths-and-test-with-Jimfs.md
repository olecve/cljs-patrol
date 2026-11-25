---
id: CP-92
title: "Read and write files through NIO paths, and test with Jimfs"
status: Review
labels: ["core"]
created_date: ""
updated_date: ""
references:
  [
    "https://github.com/olecve/cljs-patrol/pull/179",
    "https://github.com/olecve/cljs-patrol/pull/180",
    "https://github.com/olecve/cljs-patrol/releases/tag/v0.0.62",
  ]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

cljs-patrol.fs wraps java.io.File and takes path strings. (File. nil "src") quietly resolves against the working
directory, which let a mutant write fixtures into the repo (#91). File.delete and .mkdirs return false instead of saying
why.

### Why

NIO throws on nil and gives errors with a reason; Jimfs keeps file tests off the real disk. Decided 2026-11-23: Jimfs
over memoryfilesystem and Commons VFS. ADR 0013 (proposed) records the migration steps and the final goal.

### Fix

```clojure
;; enforcement, agreed 2026-11-24
;; now: ^{:deprecated "0.0.63"} on the fs string adapters (clj-kondo :deprecated-var)
;; with step 4: clj-kondo :discouraged-var on clojure.java.io/file, slurp, spit, io/reader, io/writer
;;   (exempt: classpath resources, dev/, build.clj)
;; and the ADR 0013 Confirmation grep for java.io.File as a CI step
```

### Notes

Step 1 merged 2026-11-24 as PR 179 (d00633a), released in v0.0.62. Step 2 merged 2026-11-24 as PR 180 (cc52e29): core,
baseline, parser and the reporters work on Paths from the default FileSystem made in -main; typed text kept for printed
folders and the configured baseline path; fs/text-writer replaces bad characters as io/writer did; fs/read-text is the
lenient UTF-8 read; discovered names come back in NFC when that names the same file, as File.listFiles gave before
v0.0.62; no fs/absolute-path caller left in src. Output equals main on all 17 test-projects and the a11y-app baseline.
ADR 0013 accepted 2026-11-24. Next: step 3 (file tests on Jimfs), step 4 (remove the string adapters, then the
Confirmation grep finds no java.io in src).

### Links

- cljs-patrol PR 179: https://github.com/olecve/cljs-patrol/pull/179
- cljs-patrol PR 180: https://github.com/olecve/cljs-patrol/pull/180
- v0.0.62: https://github.com/olecve/cljs-patrol/releases/tag/v0.0.62

<!-- SECTION:DESCRIPTION:END -->
