---
id: CP-0067
title: "Read and write files through NIO paths, and test with Jimfs"
status: Done
labels: ["core"]
created_date: ""
updated_date: "2026-11-25"
references:
  [
    "https://github.com/olecve/cljs-patrol/pull/179",
    "https://github.com/olecve/cljs-patrol/pull/180",
    "https://github.com/olecve/cljs-patrol/pull/181",
    "https://github.com/olecve/cljs-patrol/pull/182",
    "https://github.com/olecve/cljs-patrol/pull/183",
    "https://github.com/olecve/cljs-patrol/releases/tag/v0.0.62",
  ]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`cljs-patrol.fs` used to wrap `java.io.File` and take path strings. (File. nil "src") quietly resolves against the
working directory, which let a mutant write fixtures into the repo (CP-0066). File.delete and .mkdirs return false
instead of saying why.

### Why

NIO throws on nil and gives errors with a reason; Jimfs keeps file tests off the real disk. Decided 2026-11-23: Jimfs
over memoryfilesystem and Commons VFS. ADR 0013 records the migration steps and the final goal.

### Fix

Use `java.nio.file.Path` for file access and Jimfs for file unit tests. CI checks Clojure file I/O vars and Java file
interop in `src/`.

### Notes

Step 1 merged 2026-11-24 as PR 179 (d00633a), released in v0.0.62. Step 2 merged 2026-11-24 as PR 180 (cc52e29): core,
baseline, parser and the reporters work on Paths from the default FileSystem made in -main; typed text kept for printed
folders and the configured baseline path; fs/text-writer replaces bad characters as io/writer did; fs/read-text is the
lenient UTF-8 read; discovered names come back in NFC when that names the same file, as File.listFiles gave before
v0.0.62; no fs/absolute-path caller left in src. Output equals main on all 17 test-projects and the a11y-app baseline.
PR #181 deprecated the string adapters. PR #182 moved the file unit tests to Jimfs. PR #183 removed the adapters and
added the CI checks. ADR 0013 records the completed migration and verification.

### Links

- cljs-patrol PR 179: https://github.com/olecve/cljs-patrol/pull/179
- cljs-patrol PR 180: https://github.com/olecve/cljs-patrol/pull/180
- [PR #181](https://github.com/olecve/cljs-patrol/pull/181)
- [PR #182](https://github.com/olecve/cljs-patrol/pull/182)
- [PR #183](https://github.com/olecve/cljs-patrol/pull/183)
- [ADR 0013](../../docs/adr/0013-nio-paths-and-in-memory-test-file-system.md)
- v0.0.62: https://github.com/olecve/cljs-patrol/releases/tag/v0.0.62

<!-- SECTION:DESCRIPTION:END -->
