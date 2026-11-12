# Focus ring rules

The rules of the `focus-ring` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only focus-ring`. It does nothing until `:focus-ring` is set in `.cljs-patrol/config.edn`;
see [Focus ring options](#focus-ring-options). The examples below run under the config shown there.

## Rules

### Focus ring without the forced-colors companion

`focus-ring-missing-companion` · bugs · config [`:required-companion`](#focus-ring-options) — A focus ring painted with
`box-shadow` and no transparent outline, so it disappears in forced-colors mode.

```clojure
;; flagged
(defclass trigger-style []
  ["&:focus-visible::after" {:box-shadow focus-rings/primary-shadow}])

;; fix
(defclass trigger-style []
  ["&:focus-visible::after" {:box-shadow focus-rings/primary-shadow
                             :outline "2px solid transparent"}])
```

**Left alone:**

```clojure
(defclass input-style []
  [:textarea:focus {:box-shadow :none}])

(defclass row-style []
  ["&:not(:focus-visible)" {:box-shadow "0 1px 2px rgba(0, 0, 0, 0.2)"}])

(defclass card-style []
  [:&:focus-visible (merge base-ring {:border-radius "4px"})])

(defclass chip-style []
  [:&:focus-visible {:outline "2px solid transparent"}
   [:&::after {:box-shadow focus-rings/primary-shadow}]])
```

<details>
<summary>Details</summary>

Windows forced-colors mode (High Contrast) drops `box-shadow` and keeps `outline`. A ring painted only with a shadow
looks right in every browser and is gone in High Contrast, so a keyboard user there cannot see where focus is. A
transparent outline costs nothing in the normal mode, and forced-colors mode paints it in the system color.

The rule reads every selector block of a `defclass`, `defattrs` or `defglobal` whose selector, or a selector above it,
holds a configured focus pseudo-class outside `:not(…)`: `:&:focus-visible`, `".ProseMirror:focus-visible"` and
`":&:focus-within:not(:has(button:focus))"` all count, `"&:not(:focus-visible)"` does not. When the block sets
`:box-shadow`, every property of `:required-companion` must be set to its configured value, in one of the block's
literal maps or in a focus block around it. A missing property and a wrong literal value, such as `:outline :none`, are
both reported. A value the rule cannot read, a symbol or a call, is taken as set.

A reset (`:box-shadow :none`, or `"none"`, `"unset"` and the other CSS-wide keywords) paints no ring and is left alone.
So is a block holding anything besides literal maps and nested blocks, such as `(merge …)`, which may set the outline,
and a `:box-shadow` built by a call other than `str`, such as `(if highlighted? x y)`.

</details>

### Focus ring not from the preset

`focus-ring-raw-shadow` · deprecations · config [`:approved-shadows`](#focus-ring-options) — A focus ring whose
`box-shadow` is built by hand instead of taken from the design system's preset.

```clojure
;; flagged
(defclass toggle-style []
  [:&:focus-visible {:box-shadow "0 0 0 2px var(--ring-color)"
                     :outline "2px solid transparent"}])

;; fix
(defclass toggle-style []
  [:&:focus-visible {:box-shadow focus-rings/primary-shadow
                     :outline "2px solid transparent"}])
```

**Left alone:**

```clojure
(defclass editor-style [highlight-ring]
  [:&:focus-visible {:box-shadow (str highlight-ring ", " focus-rings/primary-shadow)
                     :outline "2px solid transparent"}])
```

<details>
<summary>Details</summary>

A ring built by hand drifts: the next change to the ring's color, width or forced-colors contract updates the preset and
misses this call site. The `:box-shadow` of a focus block must be one of `:approved-shadows`, written as is. Under a
focus state in `:allow-stacking-under`, a `(str …)` that holds an approved shadow is accepted too, for a stacked outer
and inner ring. Everywhere else a `str`, a string, a Garden vector or another var is reported.

A local, such as a parameter of the `defclass`, is not something the rule can read and is left alone. So is a string
that is only a CSS custom property, `"var(--ring)"`, whose value is set somewhere else. One with a fallback,
`"var(--ring, 0 0 0 2px #516bff)"`, is read as any other string. A value that a more specific rule reports,
[a forbidden token](#focus-ring-from-a-forbidden-token) or [a forbidden color](#focus-ring-with-a-forbidden-color), is
not reported here as well. Files in `:exempt-paths` are not checked, the preset's own namespace for one. Off while
`:approved-shadows` is empty.

</details>

### Focus ring from a forbidden token

`focus-ring-forbidden-token` · deprecations · config [`:forbidden-tokens`](#focus-ring-options) — A focus ring painted
from a raw theme token that should go through the preset.

```clojure
;; flagged
(defclass link-style []
  [:&:focus-visible {:box-shadow button-theme/shadow-brand-focused
                     :outline "2px solid transparent"}])

;; fix
(defclass link-style []
  [:&:focus-visible {:box-shadow focus-rings/primary-shadow
                     :outline "2px solid transparent"}])
```

<details>
<summary>Details</summary>

A token anywhere in the `:box-shadow` value counts, inside a `str` or a Garden vector too. `:forbidden-tokens` maps each
token to the approved shadow that replaces it, and the finding names that shadow. Files in `:exempt-paths` are not
checked.

</details>

### Focus ring with a forbidden color

`focus-ring-forbidden-color` · deprecations · config [`:forbidden-color-literals`](#focus-ring-options) — A focus ring
painted with a raw brand color instead of the preset.

```clojure
;; flagged
(defclass tab-style []
  [:&:focus-visible {:box-shadow "0 0 0 2px #516BFF"
                     :outline "2px solid transparent"}])

;; fix
(defclass tab-style []
  [:&:focus-visible {:box-shadow focus-rings/primary-shadow
                     :outline "2px solid transparent"}])
```

<details>
<summary>Details</summary>

A color matches anywhere in a string of the `:box-shadow` value, inside a `str` or a Garden vector too, without regard
to case: `#516BFF` is the same color as `#516bff`. Files in `:exempt-paths`, such as the preset that defines the colors,
are not checked.

</details>

## Focus ring options

The group reads `:focus-ring` from `.cljs-patrol/config.edn`, and does nothing until it is set:

```clojure
{:focus-ring
 {:selectors #{":focus" ":focus-visible" ":focus-within"}
  :approved-shadows #{app.styles.focus-rings/primary-shadow
                      app.styles.focus-rings/danger-shadow}
  :allow-stacking-under #{":focus-visible"}
  :required-companion {:outline "2px solid transparent"}
  :forbidden-tokens {app.theme.button/shadow-brand-focused app.styles.focus-rings/primary-shadow}
  :forbidden-color-literals #{"#516bff"}
  :exempt-paths #{"src/app/styles/focus_rings.cljs"}}}
```

- `:selectors` — the focus pseudo-classes. A selector that holds one outside `:not(…)` is a focus selector. Defaults to
  `:focus`, `:focus-visible` and `:focus-within`.
- `:approved-shadows` — the vars whose value is an approved ring, fully qualified. Empty turns off
  [Focus ring not from the preset](#focus-ring-not-from-the-preset).
- `:allow-stacking-under` — the focus states under which a `(str …)` holding an approved shadow is accepted.
- `:required-companion` — the properties a focus block with a `box-shadow` must also set, and their values. Empty turns
  off [Focus ring without the forced-colors companion](#focus-ring-without-the-forced-colors-companion).
- `:forbidden-tokens` — raw theme tokens, each mapped to the approved shadow that replaces it. A set works too, when
  there is nothing to suggest.
- `:forbidden-color-literals` — colors, hex or `rgb(…)`, that no focus `box-shadow` may hold.
- `:exempt-paths` — files, relative to the project, that may build a ring by hand. Only the companion is checked there.

Every key but `:selectors` may be empty, and an empty key turns off its part of the check. A symbol in the config is
matched against what a file's aliases and refers resolve a symbol to, so `focus-rings/primary-shadow` matches
`app.styles.focus-rings/primary-shadow` in a file that requires `[app.styles.focus-rings :as focus-rings]`.
