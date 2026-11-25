---
id: CP-64
title: "Config: let a module's config include a shared file"
status: Next
labels: ["core"]
priority: high
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Separate modules may duplicate component aliases. An optional include can share selected settings while preserving local
overrides. An alias added to only one config quietly weakens checks in the other module.

### Why

One source for shared aliases removes silent drift.

### Flagged

```clojure
;; today: the same aliases kept by hand in two files
;; module-a/.cljs-patrol/config.edn
{:a11y {:component-aliases {example.controls/toggle {:tag :switch …}}}}
;; module-b/.cljs-patrol/config.edn
{:a11y {:component-aliases {example.controls/toggle {:tag :switch …}}}}
```

### Fix

```clojure
;; module-b/.cljs-patrol/aliases.edn holds the shared aliases
;; module-a/.cljs-patrol/config.edn
{:include ["../../module-b/.cljs-patrol/aliases.edn"]
 :forbid-props […]} ;; module keys stay local
```

### Notes

The include is opt-in: a module may share only the keys that describe the same components (:component-aliases,
:test-id-elements) and keep the rest its own. Shape: an :include key with paths relative to the config file, merged in
order before the module's own keys; the module's keys win; an unreadable include is a config error, not a silent skip.
<!-- SECTION:DESCRIPTION:END -->
