---
id: CP-94
title: "Flag a hardcoded color literal outside the token namespace"
status: Done
labels: ["colors"]
priority: high
created_date: ""
updated_date: "2026-11-25"
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
;; test files, docstrings, comments, and namespace declarations
```

### Notes

Use `{:colors {:allowed-namespaces #{example.tokens}}}` to enable the cleanup rule. It recognizes hex strings of 3, 4,
6, or 8 digits and strings containing `rgb(`, `rgba(`, `hsl(`, or `hsla(`. Exact hex values can match a token. Use the
baseline to keep findings that must stay.

The rule merged in PR #185. PR #186 covers additional reader-conditional documentation cases and keeps baseline keys
distinct when a definition name is conditional.

### Links

- [ADR 0014](../../docs/adr/0014-flag-color-literals-outside-token-namespaces.md)
- [Rule implementation](https://github.com/olecve/cljs-patrol/pull/185)
- [Reader-conditional follow-up](https://github.com/olecve/cljs-patrol/pull/186)

<!-- SECTION:DESCRIPTION:END -->
