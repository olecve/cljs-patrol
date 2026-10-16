# Spade rules

The rules of the `spade` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `defclass` and `defattrs`. A usage is a direct call, qualified (`styles/container-style`) or
unqualified within the same namespace (`container-style`).

- **Unused styles** (`unused-styles`, cleanup) — declared with `defclass`/`defattrs` but never called
- **defattrs in merge** (`defattrs-in-merge`, deprecations) — `defattrs` used inside `merge`; should be `defclass` so
  callers can pass it via `:class` without merge
- **Pseudo-selector in Spade main map** (`pseudo-in-main-map`, bugs) — `defclass`/`defattrs` with a `:&`-prefixed key
  (e.g. `:&:hover`) inside a style map; Spade emits it as an invalid CSS property and silently drops the rule. Move the
  selector into its own sibling vector `[:&:hover {…}]`. Checked in the base map and in the map of every nested selector
  block, at any depth
- **Consecutive self-selectors** (`consecutive-self-selectors`, bugs) — a Spade selector vector begins with 2+
  `:&`-prefixed keywords (e.g. `[:&:before :&:after {…}]`); Garden compiles this as a descendant selector
  (`elem:before elem:after`), not the comma-joined selector the author intended. Checked at every nesting depth, not
  only on the declaration's own siblings
- **Ampersand not at start** (`spade-ampersand-not-at-start`, bugs) — a string selector inside `defclass`/`defattrs`
  carries `&` anywhere but position 0 (e.g. `["li:focus-within &" {…}]`). Garden substitutes the parent-class reference
  only at the front of a string selector; elsewhere the `&` survives into the stylesheet as a literal character, and the
  rule matches nothing. Checked at every nesting depth; `"&"`, `"&:hover"` and `"&[data-x]"` are correct
- **Keyword combinator selector** (`spade-keyword-combinator-selector`, bugs) — a Spade selector vector holds a
  combinator keyword (`:>`, `:+`) alongside another selector, e.g. `[:> :span {…}]`. Garden reads a selector vector as a
  comma-separated list, so this compiles to `.parent >, .parent span {…}` — the first half invalid, the second matching
  every descendant. Use the string form, `["> span" {…}]`. Single-element vectors (`[:&:hover {…}]`), string selectors
  and plain descendant chains (`[:svg :path {…}]`) are left alone
