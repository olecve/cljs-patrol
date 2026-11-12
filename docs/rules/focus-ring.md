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

(defclass link-style []
  [:&:focus-visible {:box-shadow button-theme/shadow-brand-focused
                     :outline "2px solid transparent"}])

(defclass tab-style []
  [:&:focus-visible {:box-shadow (str "inset " focus-rings/primary-shadow ", 0 0 0 1px #516BFF")
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

(defclass card-style [ring]
  [:&:focus {:box-shadow ring
             :outline "2px solid transparent"}])
```

<details>
<summary>Details</summary>

A ring built by hand drifts: the next change to the ring's color, width or forced-colors contract updates the preset and
misses this call site. The `:box-shadow` of a focus block must be one of `:approved-shadows`, written as is, or held in
a `(str …)` or a Garden vector beside other parts, for a stacked outer and inner ring. A string, another var, and a
`str` or a Garden vector without an approved shadow are reported, a CSS custom property such as `"var(--ring)"` too: the
preset is what a focus ring should name.

When `:approved-shadows` maps an approved shadow to the raw values it replaces, a value holding one of them is reported
even inside a stacked ring, and the finding names the shadow to use: `button-theme/shadow-brand-focused` or the color
`#516BFF` above. A color matches without regard to case, to spaces around parentheses and commas, so `rgb(81, 107, 255)`
matches `rgb(81,107,255)`, and up to the next hex digit, so `#fff` does not match `#fffabc`.

A local, such as a parameter of the `defclass`, is not something the rule can read and is left alone, and so is a `str`
or a Garden vector that holds a local or a call other than `str`, unless it holds a replaced value. The namespaces of
the approved shadows, the preset, may build rings by hand and are not checked. A one-off elsewhere goes in the
[baseline](../baseline.md). Off while `:approved-shadows` is empty.

</details>

## Focus ring options

The group reads `:focus-ring` from `.cljs-patrol/config.edn`, and does nothing until it is set:

```clojure
{:focus-ring {:approved-shadows #{app.styles.focus-rings/primary-shadow
                                  app.styles.focus-rings/danger-shadow}}}
```

- `:approved-shadows` — the vars whose value is an approved ring, fully qualified. A map from each to the raw tokens and
  colors it replaces works too, and a finding then names the shadow to use:
  `{app.styles.focus-rings/primary-shadow #{app.theme.button/shadow-brand-focused "#516bff"}}`. Empty turns off
  [Focus ring not from the preset](#focus-ring-not-from-the-preset).
- `:outline` — the outline a focus block with a `box-shadow` must set, such as `"2px solid transparent"`. Left out, any
  outline that draws holds. `false` turns off [Focus ring without an outline](#focus-ring-without-an-outline).
- `:selectors` — the focus pseudo-classes. A selector that holds one outside `:not(…)` is a focus selector. Defaults to
  `:focus`, `:focus-visible` and `:focus-within`.

Every key may be left out: `{:focus-ring {}}` checks only the outline. A symbol in the config is matched against what a
file's aliases and refers resolve a symbol to, so `focus-rings/primary-shadow` matches
`app.styles.focus-rings/primary-shadow` in a file that requires `[app.styles.focus-rings :as focus-rings]`.
