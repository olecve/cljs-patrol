---
id: CP-63
title: "focus-ring-raw-shadow: accept a box-shadow that reads a CSS variable filled from a preset"
status: Done
labels: ["focus-ring"]
priority: high
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/161"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A style may set :box-shadow "var(--example-ring)" in the focus block and fill the variable from a focus-ring preset
elsewhere. focus-ring-raw-shadow reads the var(...) string as a hand-written shadow, producing a false positive.

### Why

A variable filled from a preset is already valid; :approved-shadows maps presets to tokens and cannot express a
variable.

### Flagged

```clojure
;; flagged today, wrongly
[:&:focus-visible {:box-shadow "var(--example-ring)"}]
```

### Fix

```clojure
;; left alone: the variable is filled from a preset in the same file
[:&:focus-visible {:box-shadow "var(--example-ring)"}]
;; elsewhere in the file
{"--example-ring" example.rings/outline-shadow}
```

### Left alone

```clojure
;; still flagged: the variable is filled with a raw shadow
{"--x-shadow" "0 0 0 2px #00f"}
[:&:focus {:box-shadow "var(--x-shadow)"}]
```

### Notes

Released in cljs-patrol 0.0.60 (PR 161, 6 review rounds). A :box-shadow that is "var(--name)" and nothing else is judged
by every value the same file sets --name to, in the style maps of its top-level defclass, defattrs and defglobal forms
(also under metadata, in at-media/at-supports calls, and in the branch ClojureScript reads of a reader conditional).
Values that read another property are followed. A property no style map of the file sets, or whose values only read each
other, is reported with "--x is not set in a style map of this file."; a value off the preset names the declarations:
"--x is not set from the preset in a, b." Variables assigned unapproved values remain reportable.

### Links

- cljs-patrol PR 161: https://github.com/olecve/cljs-patrol/pull/161

<!-- SECTION:DESCRIPTION:END -->
