# Focus ring rules

The rules of the `focus-ring` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only focus-ring`. It does nothing until `:focus-ring` is set in `.cljs-patrol/config.edn`;
see [Focus ring options](#focus-ring-options). The examples below run under the config shown there.

## Rules

### Focus ring without an outline

`focus-ring-missing-outline` · bugs · config [`:outline`](#focus-ring-options) — A focus ring painted with `box-shadow`
and no outline, so it disappears in forced-colors mode.

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

(defclass tile-style []
  [:&:focus-visible {:outline "1px solid transparent"}]
  ["&:focus-visible::after" {:box-shadow focus-rings/primary-shadow}])

(defclass option-style []
  ["&:focus:not(:focus-visible)" {:box-shadow "0 1px 2px rgba(0, 0, 0, 0.2)"}])
```

<details>
<summary>Details</summary>

Windows forced-colors mode (High Contrast) drops `box-shadow` and keeps `outline`. A ring painted only with a shadow
looks right in every browser and is gone in High Contrast, so a keyboard user there cannot see where focus is. A
transparent outline costs nothing in the normal mode, and forced-colors mode paints it in the system color.

The rule reads every selector block of a `defclass`, `defattrs` or `defglobal` whose selector, or a selector above it,
holds a configured focus pseudo-class outside `:not(…)`: `:&:focus-visible`, `".ProseMirror:focus-visible"` and
`":&:focus-within:not(:has(button:focus))"` all count, `"&:not(:focus-visible)"` does not. Neither does a `:focus` that
a selector holding `:not(:focus-visible)` narrows, written as one selector or nested under `[:&:focus …]`: it styles
focus from a pointer, so that no ring shows.

When the block sets `:box-shadow`, it must also set an outline that draws: any value but one with a `none` or `hidden`
style or a zero width outside parentheses, so the `0` of `rgb(0 0 0)` is no width. Forced-colors mode paints any such
outline in the system color, whatever color it was written with. With `:outline` configured, the outline must be that
value instead, compared without regard to case, spacing or `!important`, so a codebase can keep one outline everywhere.

The outline may sit in the block's literal maps or in a map around it that styles the same element. A block whose
selectors all start with `&` is the element around it in a narrower state, or one of its pseudo-elements, so every map
around it counts: the declaration's base map and its `[:& …]` blocks, and blocks such as `[:&:hover …]`,
`["&:not(:disabled)" …]` or `[:.btn …]` that it sits in. A descendant, such as `[:input:focus …]`, is another element:
only the focus blocks around it count, whose outline marks the element that holds focus. For a ring on `::after` or
`::before`, the element's own block in the same declaration counts too, `[:&:focus-visible {:outline …}]` beside
`["&:focus-visible::after" {:box-shadow …}]`, because forced-colors mode paints the element's outline. The
pseudo-element matches in any case, and in a selector list written as one string. A ring on the pseudo-elements of
several elements needs the outline on every one of them. The block and the element hold each on its own, so an
`:outline :none` on the element does not undo the outline on its `::after`.

A value the rule cannot read, a symbol or a call, is taken as set, and so is an outline written as its longhands,
`:outline-width`, `:outline-style` or `:outline-color`, which the rule does not read further. So `:outline-style "none"`
passes, and so does a width with no style, `:outline "2px"`, which draws nothing. The rule leaves these to review rather
than risk reporting an outline that works.

A reset (`:box-shadow :none`, or `"none"`, `"unset"` and the other CSS-wide keywords, with or without `!important`)
paints no ring and is left alone. So is a block holding anything besides literal maps and nested blocks, such as
`(merge …)`, which may set the outline, and a `:box-shadow` built by a call other than `str`, such as
`(if highlighted? x y)`.

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
misses this call site. The `:box-shadow` of a focus block must be one of `:approved-shadows`, written as is, or alone in
a `(str …)` or a Garden vector. Under a focus state in `:allow-stacking-under`, a `str` or a Garden vector that holds an
approved shadow beside other parts is accepted too, for a stacked outer and inner ring. Everywhere else a `str`, a
string, a Garden vector or another var is reported.

A local, such as a parameter of the `defclass`, is not something the rule can read and is left alone, and so is a `str`
or a Garden vector that holds no approved shadow and holds a local or a call other than `str`. A string is always read,
a CSS custom property such as `"var(--ring)"` too: the preset is what a focus ring should name. A value that a more
specific rule reports, [a forbidden token](#focus-ring-from-a-forbidden-token) or
[a forbidden color](#focus-ring-with-a-forbidden-color), is not reported here as well. Files in `:exempt-paths` are not
checked, the preset's own namespace for one. Off while `:approved-shadows` is empty.

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

A color matches anywhere in a string of the `:box-shadow` value, inside a `str` or a Garden vector too, and inside a
call within them, without regard to case: `#516BFF` is the same color as `#516bff`. Spaces around parentheses and commas
do not count either, so `rgb(81, 107, 255)` matches `rgb(81,107,255)`. A color followed by another hex digit is a
different one, so `#fff` does not match `#fffabc`. A value built by a call alone, such as `(if highlighted? x y)`, is
not read. Files in `:exempt-paths`, such as the preset that defines the colors, are not checked.

</details>

## Focus ring options

The group reads `:focus-ring` from `.cljs-patrol/config.edn`, and does nothing until it is set:

```clojure
{:focus-ring
 {:selectors #{":focus" ":focus-visible" ":focus-within"}
  :approved-shadows #{app.styles.focus-rings/primary-shadow
                      app.styles.focus-rings/danger-shadow}
  :allow-stacking-under #{":focus-visible"}
  :outline "2px solid transparent"
  :forbidden-tokens {app.theme.button/shadow-brand-focused app.styles.focus-rings/primary-shadow}
  :forbidden-color-literals #{"#516bff"}
  :exempt-paths #{"src/app/styles/focus_rings.cljs"}}}
```

- `:selectors` — the focus pseudo-classes. A selector that holds one outside `:not(…)` is a focus selector. Defaults to
  `:focus`, `:focus-visible` and `:focus-within`.
- `:approved-shadows` — the vars whose value is an approved ring, fully qualified. Empty turns off
  [Focus ring not from the preset](#focus-ring-not-from-the-preset).
- `:allow-stacking-under` — the focus states under which a `(str …)` or a Garden vector holding an approved shadow
  beside other parts is accepted.
- `:outline` — the outline a focus block with a `box-shadow` must set. Left out, any outline that draws holds. `false`
  turns off [Focus ring without an outline](#focus-ring-without-an-outline).
- `:forbidden-tokens` — raw theme tokens, each mapped to the approved shadow that replaces it. A set works too, when
  there is nothing to suggest.
- `:forbidden-color-literals` — colors, hex or `rgb(…)`, that no focus `box-shadow` may hold.
- `:exempt-paths` — files, relative to the project, that may build a ring by hand. Only the outline is checked there. A
  path matches the end of a file's path on a directory boundary, so `src/app/styles/focus_rings.cljs` holds however the
  source directory was named on the command line. A bare file name matches only a file at the root, never every file of
  that name.

Every key may be left out. An empty `:approved-shadows`, `:forbidden-tokens` or `:forbidden-color-literals` turns off
its check, and `{:focus-ring {}}` checks only the outline. A symbol in the config is matched against what a file's
aliases and refers resolve a symbol to, so `focus-rings/primary-shadow` matches `app.styles.focus-rings/primary-shadow`
in a file that requires `[app.styles.focus-rings :as focus-rings]`.
