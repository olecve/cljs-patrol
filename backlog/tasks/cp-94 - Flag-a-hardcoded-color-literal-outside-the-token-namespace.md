---
id: CP-94
title: "Flag a hardcoded color literal outside the token namespace"
status: Next
labels: ["colors"]
priority: high
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Code should use configured design tokens, such as tokens/accent with the invented value "#336699" or
"var(--example-accent)", instead of color literals. Literals cannot adapt to runtime themes. The rule should detect
literals outside configured token namespaces, including positions a style-macro hook cannot inspect: plain defs, Hiccup
attributes, canvas interop (set! (.-fillStyle ctx) ...), chart options, nested selectors and colors inside larger CSS
values (box-shadow rgba).

### Why

Using tokens instead of literals lets colors adapt to runtime themes.

### Flagged

```clojure
(def ink "#000000")
[:path {:stroke "#000000"}]
(set! (.-fillStyle ctx) "#ffffff")
{:itemStyle {:color "#336699"}}
(defclass card [] {} [:&:hover {:color "#fff"}])
{:box-shadow "0 1px 2px rgba(0,0,0,0.05)"}
```

### Fix

```clojure
(def ink tokens/ink)
[:path {:stroke tokens/ink}]
;; hint when the token namespace has a hex value: "#336699 is tokens/accent"
```

### Left alone

```clojure
"#/settings"   ; route fragment, not hex digits
"#section"       ; anchor
;; literals inside the configured token namespaces
;; namespaces in :allow-namespaces, test files, docstrings, comments, ns forms
```

### Notes

One setting only, {:colors {:allowed-namespaces #{example.tokens}}}, to identify where color literals are allowed.
Literals are fine anywhere in those namespaces, and the token hint reads their (def name "#hex") forms. No
allowed-colors list for now; exceptions use the usual ignore comment. A new opt-in group `colors`, cleanup tier. Color =
hex strings of exactly 3/4/6/8 hex digits after #, and rgb(/rgba(/hsl(/hsla( anywhere in a string; named colors left out
of v1. Looks at every string literal in code, skips docstrings, comments, ns forms and test files. Hint: hex->token map
from the allowed namespaces; a var(--...) token gives no hex, so the hint falls back to "use a token". Adoption through
the existing baseline flow. Plan: ADR 0014 draft for review, then a PR with the rule, rule doc examples, config and
hint. Related: focus-ring (token vs hand-written), typography (token namespaces).
<!-- SECTION:DESCRIPTION:END -->
