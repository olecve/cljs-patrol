# Spade rules

The rules of the `spade` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `defclass` and `defattrs`. A usage is a direct call, qualified (`styles/container-style`) or
unqualified within the same namespace (`container-style`).

### Unused styles

`unused-styles` · cleanup — A `defclass` or `defattrs` nothing calls.

```clojure
;; flagged
(defclass legacy-panel-style []
  {:color :red})

;; fix
(defclass panel-style []
  {:color :red})
(defn panel [] [:div {:class (panel-style) :id "panel"}])
```

<details>
<summary>Details</summary>

Declared with `defclass`/`defattrs` but never called

</details>

### defattrs in merge

`defattrs-in-merge` · deprecations — A `defattrs` merged into props; a `defclass` passed as `:class` says the same.

```clojure
;; flagged
(defattrs card-attrs []
  {:padding "8px"})
(defn card [props] [:div (merge (card-attrs) props)])

;; fix
(defclass card-style []
  {:padding "8px"})
(defn card [props] [:div (merge {:class (card-style)} props)])
```

<details>
<summary>Details</summary>

`defattrs` used inside `merge`; should be `defclass` so callers can pass it via `:class` without merge

</details>

### Pseudo-selector in Spade main map

`pseudo-in-main-map` · bugs — A `:&:hover`-style key inside a style map, which Spade drops.

```clojure
;; flagged
(defclass link-style []
  {:color :blue
   :&:hover {:color :red}})

;; fix
(defclass link-style []
  {:color :blue}
  [:&:hover {:color :red}])
```

<details>
<summary>Details</summary>

`defclass`/`defattrs` with a `:&`-prefixed key (e.g. `:&:hover`) inside a style map; Spade emits it as an invalid CSS
property and silently drops the rule. Move the selector into its own sibling vector `[:&:hover {…}]`. Checked in the
base map and in the map of every nested selector block, at any depth

</details>

### Consecutive self-selectors

`consecutive-self-selectors` · bugs — Two `:&` selectors in one vector, which compile to a descendant selector.

```clojure
;; flagged
(defclass quote-style []
  [:&:before :&:after {:content "\"\""}])

;; fix
(defclass quote-style []
  [:&:before {:content "\"\""}]
  [:&:after {:content "\"\""}])
```

<details>
<summary>Details</summary>

A Spade selector vector begins with 2+ `:&`-prefixed keywords (e.g. `[:&:before :&:after {…}]`); Garden compiles this as
a descendant selector (`elem:before elem:after`), not the comma-joined selector the author intended. Checked at every
nesting depth, not only on the declaration's own siblings

</details>

### Ampersand not at start

`spade-ampersand-not-at-start` · bugs — A `&` anywhere but the front of a string selector, which Garden leaves in as
text.

```clojure
;; flagged
(defclass row-style []
  ["li:focus-within &" {:outline "none"}])

;; fix
(defclass row-style []
  ["&:focus-within" {:outline "none"}])
```

<details>
<summary>Details</summary>

A string selector inside `defclass`/`defattrs` carries `&` anywhere but position 0 (e.g. `["li:focus-within &" {…}]`).
Garden substitutes the parent-class reference only at the front of a string selector; elsewhere the `&` survives into
the stylesheet as a literal character, and the rule matches nothing. Checked at every nesting depth; `"&"`, `"&:hover"`
and `"&[data-x]"` are correct

</details>

### Keyword combinator selector

`spade-keyword-combinator-selector` · bugs — A combinator written as a keyword, which Garden reads as a selector of its
own.

```clojure
;; flagged
(defclass list-style []
  [:> :span {:margin 0}])

;; fix
(defclass list-style []
  ["> span" {:margin 0}])
```

**Left alone:**

```clojure
(defclass icon-style []
  [:svg :path {:fill :none}]) ; a descendant chain
```

<details>
<summary>Details</summary>

A Spade selector vector holds a combinator keyword (`:>`, `:+`) alongside another selector, e.g. `[:> :span {…}]`.
Garden reads a selector vector as a comma-separated list, so this compiles to `.parent >, .parent span {…}` — the first
half invalid, the second matching every descendant. Use the string form, `["> span" {…}]`. Single-element vectors
(`[:&:hover {…}]`), string selectors and plain descendant chains (`[:svg :path {…}]`) are left alone

</details>
