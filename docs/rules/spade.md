# Spade rules

The rules of the `spade` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `defclass` and `defattrs`. A usage is a direct call, qualified (`styles/container-style`) or
unqualified within the same namespace (`container-style`).

Run this group alone with `--only spade`.

## Rules

### Unused styles

`unused-styles` · cleanup — A `defclass` or `defattrs` nothing calls.

```clojure
;; flagged
(defclass legacy-panel-style []
  {:color :red})

;; fix
(defclass legacy-panel-style []
  {:color :red})

(defn panel [] [:div {:class (legacy-panel-style) :id "panel"}])
```

### `defattrs` as data

`defattrs-as-data` · deprecations — A `defattrs` whose result is changed or read as a map, which hides the other
attributes.

```clojure
;; flagged
(defattrs card-attrs []
  {:padding "8px"})

(defn card [props] [:div (merge (card-attrs) props)])

;; assoc onto it
(defattrs row-attrs []
  {:display :flex})

(defn row [id] [:div (assoc (row-attrs) :id id)])

;; one key read out of it
(defattrs badge-attrs []
  {:color :red})

(defn badge [] [:span {:class (:class (badge-attrs))}])

;; a key threaded out of it
(defattrs tab-attrs []
  {:margin 0})

(defn tab [] [:li {:class (-> (tab-attrs) :class)}])

;; fix
(defclass card-style []
  {:padding "8px"})

(defn card [props] [:div (merge {:class (card-style)} props)])

;; assoc onto it
(defclass row-style []
  {:display :flex})

(defn row [id] [:div {:class (row-style) :id id}])

;; one key read out of it
(defn badge [] [:span (badge-attrs)])
(defn tab [] [:li (tab-attrs)])
```

**Left alone:**

```clojure
;; the whole props map of a tag or a component
(defattrs card-attrs []
  {:padding "8px"})

(defn card [] [:div (card-attrs)])

(defn panel [] [card (card-attrs)])

;; passed to another function, or as a prop
(defattrs row-attrs []
  {:display :flex})

(defn row [] [:div (with-focus (row-attrs))])

(defn table [] [card {:row-props (row-attrs)}])

;; a local merge
(defattrs tab-attrs []
  {:margin 0})

(defn tab [merge props] [:li (merge (tab-attrs) props)])
```

<details>
<summary>Details</summary>

A `defattrs` returns the whole props map. When its call result is changed or read as a map, the attributes the call
holds are hidden from the rules that read literal props maps, for example the [a11y](a11y.md) rules. The rule reports
the `defattrs` at its definition, one time for all of its calls. A call is read as data when it is:

- an argument of `merge` or `into`;
- the first argument of `assoc`, `assoc-in`, `update`, `dissoc`, `conj`, `get` or `get-in`;
- the first argument of a keyword call, for example `(:class (card-attrs))`;
- in a `->`, and the step after it is one of the forms above, for example `(-> (card-attrs) :class)` or
  `(-> (card-attrs) (assoc :id id))`.

A `#_` form is not an argument or a step. A call inside a `#_` form is left alone. A local that shadows the name of the
`defattrs` is not a call of it. A `defattrs` called through an alias or a `:refer` counts.

A function name counts only when it is the `clojure.core` / `cljs.core` function, written plain, through an alias, or
through `:refer`. A local of the same name, or a function of another namespace with the same name, does not count. In a
step of `->`, `some->` or `cond->`, the threaded value is the first argument, so a call there is the second argument:
`(-> cache (get (card-attrs)))` uses the call as a key, and is left alone. The test of a `cond->` is not a step, so
`(cond-> props (:hidden (card-attrs)) …)` is reported. A `#?@` splice hides the position of what it holds. So a call is
left alone when a `#?@` splices it into a thread-first form or into the arguments of a function that reads one position,
such as `assoc`, `assoc-in`, `update`, `dissoc`, `conj`, `get`, `get-in` or a key lookup. It is left alone too when its
reading call is a step that a `#?@` splices into a thread-first form, as in
`(-> cache #?@(:cljs [(get (card-attrs))]))`. A call that a `#?@` splices into a `merge` or an `into`, or into a step
that is one, is reported, because those read every argument.

These calls are left alone: the whole props map of a tag or a component (`[:div (card-attrs)]`), an argument of any
other function, and a prop of a component (`{:attrs (card-attrs)}`). The rule cannot read what happens to the map there,
and the use can be correct. A `defclass` is never reported.

To fix it, declare the style with `defclass` and pass it as `:class` in a literal props map at every call, also where
the call was the whole props map. When no call needs other props beside `:class`, keep the `defattrs` and pass its call
whole, as `badge` and `tab` show. The mirror rule, [`defclass-as-sole-attr`](reagent.md#defclass-as-sole-attr), reports
a `defclass` that is only the whole props map. A `{:class …}` map that is merged or changed, as in the `card` fix, is
not the whole props map. So a style that one call uses whole and another call uses with other props is a `defclass`, and
neither rule reports it.

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

A `defclass`/`defattrs` with a `:&`-prefixed key (e.g. `:&:hover`) inside a style map. Spade emits it as an invalid CSS
property and silently drops the rule. Move the selector into its own sibling vector `[:&:hover {…}]`. Checked in the
base map and in the map of every nested selector block, at any depth.

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
a descendant selector (`elem:before elem:after`), not the comma-joined selector the author intended. Split it into
sibling vectors. Checked at every nesting depth, not only on the declaration's own siblings.

</details>

### Ampersand not at start

`spade-ampersand-not-at-start` · bugs — A `&` past the front of a string selector, which Garden leaves in as text.

```clojure
;; flagged
(defclass row-style []
  [":not(.disabled)&:hover" {:background "#eee"}])

;; fix
(defclass row-style []
  ["&:not(.disabled):hover" {:background "#eee"}])
```

<details>
<summary>Details</summary>

A string selector inside `defclass`/`defattrs` carries `&` anywhere but position 0 (e.g. `["li:focus-within &" {…}]`).
Garden substitutes the parent-class reference only at the front of a string selector; elsewhere the `&` survives into
the stylesheet as a literal character, and the rule matches nothing. Checked at every nesting depth; `"&"`, `"&:hover"`
and `"&[data-x]"` are correct.

</details>

### Keyword combinator selector

`spade-keyword-combinator-selector` · bugs — A combinator written as a keyword, which Garden reads as a selector.

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
;; a descendant chain
(defclass icon-style []
  [:svg :path {:fill :none}])
```

<details>
<summary>Details</summary>

A Spade selector vector holds a combinator keyword (`:>`, `:+`) alongside another selector, e.g. `[:> :span {…}]`.
Garden reads a selector vector as a comma-separated list, so this compiles to `.parent >, .parent span {…}` — the first
half invalid, the second matching every descendant. Use the string form, `["> span" {…}]`. Single-element vectors
(`[:&:hover {…}]`), string selectors and plain descendant chains (`[:svg :path {…}]`) are left alone.

</details>
