---
id: CP-71
title: "focus-ring: approve a shadow var without exempting its whole namespace"
status: Idea
labels: ["focus-ring"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Every key of :approved-shadows adds its namespace to :preset-namespaces, and focus-ring-raw-shadow skips every style in
a preset namespace. Approving a few token vars, such as example.tokens/outline-shadow, therefore exempts every focus
ring declared in example.tokens, not only those vars.

### Why

Approving individual variables should not exempt other styles declared in their namespace.

### Flagged

```clojure
;; config approves one token var
{:focus-ring {:approved-shadows {example.tokens/outline-shadow #{}}}}

;; should still be flagged: a hand-built ring in the same namespace
(ns example.tokens)
(defclass ring [] [:&:focus-visible {:box-shadow "0 0 0 2px #123456"}])
```

### Fix

```clojure
;; one option: list the preset namespaces explicitly
{:focus-ring {:approved-shadows {…}
              :preset-namespaces #{example.rings}}}
```

### Left alone

```clojure
;; the preset namespace itself still builds its rings by hand
(ns example.rings)
(def outline-shadow (str "0 0 0 2px " example.tokens/accent))
```

### Notes

Default :preset-namespaces could stay the namespaces of the approved shadows, so existing configs keep working.
<!-- SECTION:DESCRIPTION:END -->
