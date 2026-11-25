# ADR 0014: Flag Color Literals Outside The Token Namespaces

## Status

Accepted, 2026-11-25 (proposed 2026-11-25)

## Outcome

2026-11-25: [PR #185](https://github.com/olecve/cljs-patrol/pull/185) implements the opt-in rule. The test and native
verification jobs pass. Reader conditionals used as form heads or names and some conditional docs are covered by a
separate follow-up.

## Context

Some apps keep all their colors in design-token namespaces, such as `(def accent "#336699")`. The rule of such an app is
that other code never writes a color literal. It uses a token instead.

Tokens can become CSS custom properties, such as `"var(--example-accent)"`, so that the app can change its theme at
runtime. A token then follows the theme. A literal does not. It stays the same in every theme, and it breaks a theme
such as dark mode.

cljs-patrol is a static analysis tool for ClojureScript. It has these parts that this record uses:

- **Groups.** Each rule belongs to a rule group, such as `spade` or `focus-ring`. `--only` and `--disable` select
  groups. A group that needs config does nothing until its key is set in `.cljs-patrol/config.edn`. Such a group is
  opt-in.
- **Tiers.** Each rule has a tier: `bugs`, `deprecations` or `cleanup`. In a plain run, a `cleanup` finding is reported
  and does not fail the run. `--fail-on` names the tiers or rules that fail it.
- **Baseline.** `--baseline-write` writes all current findings to `.cljs-patrol/baseline.edn`. A run with `--baseline`
  then fails only on a finding that is not in that file, whatever its tier. A second write that adds a finding is
  refused, unless the command also has `--allow-new`. So the reviewer sees each accepted finding.
- **Ignoring one finding.** cljs-patrol has no inline marker that turns off one finding. A finding that must stay goes
  in the baseline with `--baseline-write --allow-new`. The `focus-ring` docs give the same advice for a one-off ring.

A style-map hook can check the first style map of a Spade `defclass` or `defattrs`. Color literals can also appear
outside that map. A rule that checks strings throughout a file can read these positions:

| Position                    | Rule reads | First-style-map hook sees |
| --------------------------- | ---------- | ------------------------- |
| Spade first style map       | yes        | yes                       |
| Spade nested selector       | yes        | no                        |
| Hiccup attribute            | yes        | no                        |
| Interop                     | yes        | no                        |
| Plain `def`, vectors too    | yes        | no                        |
| Data map                    | yes        | no                        |
| Garden style vector, `.clj` | no         | no                        |
| Other expression            | yes        | no                        |

The rule reads these positions in `.cljs` and `.cljc` files, except in the skipped places listed below.

An exact hex string, such as `"#fff"`, rarely means anything except a color. The same applies to `rgb(`, `rgba(`, `hsl(`
and `hsla(` in strings. A bounded hex inside a larger CSS value is also a color in practice.

Named colors are common words, such as `"red"`, `"white"` and `"transparent"`. These words can also be labels, enum
keywords or test data. A rule that reads every string cannot reliably tell these uses apart.

A hint can name a token only when the token holds the same hex value. A literal without a matching token needs a generic
hint.

## Decision

Add an opt-in rule group, `colors`, with one rule, `color-literal`, in the `cleanup` tier. The rule reports a color
literal in a string outside the namespaces that the config allows.

- **One config key.** `{:colors {:allowed-namespaces #{app.tokens}}}`. The group does nothing until it is set. Any
  literal in an allowed namespace is accepted. There is no list of allowed colors. A config of another shape, such as a
  string where a symbol belongs, stops the run with a message that names it.
- **The hex shape.** A `#` and 3, 4, 6 or 8 hex digits, in any case. The character before the `#` is the start of the
  string, a space, `(` or `,`. The character after the digits is the end of the string, a space, `)`, `,` or `;`. So the
  rule reports `"#fff"` and also the `#ddd` of `"1px solid #ddd"` and each color of a gradient.
- **The function shape.** `rgb(`, `rgba(`, `hsl(` or `hsla(` anywhere in a string, in any case.
- **Named colors are not reported in v1.**
- **Skipped places.** Test files, docstrings, `;` comments, `(comment …)` forms, `#_` forms and the `ns` form. A test
  file is a file that the `tests` group counts as test code: a file under `:tests :paths` when those paths are
  non-empty, or else a namespace whose name ends in `-test`.
- **Tests are not checked in v1.** Test code uses arbitrary colors on purpose. Examples include fixture data, computed
  styles from a browser, and inputs to a color picker or a chart. Reports for these values would add baseline entries
  that need no fix. The rule first gets stable on app code.
- **Files.** The group reads `.cljs` and `.cljc` files. It does not read `.clj` files, also not with
  `--experimental-clj`.
- **The hint.** The group builds a map from value to token. It reads each `(def name "#hex")` of an allowed namespace.
  It compares the values in lower case, with a 3-digit or 4-digit hex expanded to 6 or 8 digits. Alpha stays
  significant, so `#ffff` is not `#fff`. A finding names the token, fully qualified, when one matches. When several
  tokens match, it names them all, sorted. When no token matches, or the literal is a function shape, the hint says only
  to use a token from an allowed namespace. A token that holds `var(--…)` gives no hex, so it matches nothing.
- **Baseline identity.** A finding is identified by its file, the name of the top-level form that holds it, and the
  literal text. The line is not part of it, so a reformat does not make old findings new.
- **Adoption.** To adopt the rule, enable the group. Run `--baseline-write`. Then CI with `--baseline` fails only on a
  new literal. A literal that must stay goes in the baseline with `--allow-new`.
- **The clj-kondo hook.** Keep an existing hook until the group uses a baseline. Then remove the hook so that each
  literal is reported once.

## Options

### A new `colors` group (chosen)

- Good, because the group reads strings throughout `.cljs` and `.cljc` files, including positions outside style maps.
- Good, because the group is opt-in. Without configured token namespaces, it gives no findings.
- Good, because `cleanup` does not fail a plain run. A run with a baseline fails only on new literals.
- Bad, because cljs-patrol gets one more group and one more config key to document and test.

### Extend the `spade` or `focus-ring` group (rejected)

Rejected because both groups read only Spade style maps. Colors in plain defs, data maps, Hiccup attributes and interop
need a rule that reads outside style maps.

### Keep only the clj-kondo hook (rejected)

Rejected because a hook that reads only the first style map misses plain defs, data maps, Hiccup attributes, interop and
nested selectors. A hook that checks only whole color values also misses colors inside larger CSS values.

### A list of allowed colors (deferred)

- Good, because a list could allow a color that has no token, such as the white of a canvas export.
- Bad, because the rule of the app is that one place holds all colors. A second list is a second place.
- Bad, because the baseline already keeps a literal that must stay, and the reviewer sees it.

### An inline marker to ignore one finding (rejected)

Rejected because cljs-patrol has no such marker in any group. The baseline is how cljs-patrol ignores one finding. A
marker for one group would be a second mechanism.

### Open point 1: a hex color inside a larger string

The exact-string shape misses `"1px solid #ddd"`, gradients and shadows. These values can contain colors that must
follow the theme, just as an exact hex string must.

Strings such as `"#section"` and `"#/settings"` do not match the hex shape. They contain characters that are not hex
digits. An anchor made only of hex digits can match, so the shape cannot prevent every false positive.

A pattern with no boundaries also matches the HTML entity `&#160;`, because `160` is three hex digits. Boundaries
exclude this case.

#### Exact strings only (rejected)

- Good, because the shape is the simplest to read and to test.
- Bad, because `"1px solid #ddd"` is the easy way around the rule. After the tokens become `var(--…)`, a composite value
  with a hex color breaks a theme in the same way as an exact one.
- Bad, because the function shape already matches anywhere in a string. The two shapes would differ for no reason.

#### Bounded hex anywhere in a string (chosen)

- Good, because it reports colors in borders, gradients and shadows, which must follow the theme too.
- Good, because an exact string is one case of the same pattern, so the rule has one hex shape.
- Bad, because an anchor that is only hex digits, such as `[:a {:href "#add"}]`, is reported. The exact shape has the
  same cost.
- Bad, because `url(#beef)` would match, since `(` is an allowed character before the `#`. Such references need a
  baseline entry.

#### Hex anywhere, with no boundaries (rejected)

Rejected because it matches the HTML entity `&#160;`, which is not a color.

### Open point 2: Garden style vectors in `.clj` files

cljs-patrol reads `.clj` files only with `--experimental-clj`. The flag is for groups that are not about ClojureScript:
`docstrings`, `tests`, `layout`, `idioms` and `comments`. Every group that reads Hiccup, re-frame or Spade keeps to
`.cljs` and `.cljc` files.

#### Read `.cljs` and `.cljc` only (chosen)

- Good, because `colors` is about a theme in the browser, like the groups that keep to `.cljs` and `.cljc` files.
- Good, because `.clj` files can contain server output, such as HTML email, that does not use the browser theme. A color
  literal there can be correct.
- Bad, because Garden `.clj` files compile to CSS. Their exact hex strings, embedded hex colors and function literals
  can break a theme, but the rule does not report them.

#### Read `.clj` files under `--experimental-clj` (deferred)

- Good, because the Garden files would be checked.
- Bad, because `--experimental-clj` applies to every path of the run. A run on a full tree would report the server code.
- Bad, because the flag would change its meaning: today it widens discovery only for groups that are not about
  ClojureScript.

### Open point 3: named colors (deferred)

Deferred because named colors are also common words. Strings such as `"red"`, `"white"` and `"transparent"` can be
labels or test data. A keyword such as `:gray` can be an enum value. The rule cannot distinguish these uses without
reading their position. Also, `transparent` does not change with a theme.

### Open point 4: the hint

#### Match by value, with a generic hint as the fallback (chosen)

- Good, because a finding can name a token when its hex value matches the literal.
- Good, because the map comes from the allowed namespaces, so it needs no second config.
- Bad, because a token that holds `var(--…)` gives no hex. When all tokens become `var(--…)`, no finding names a token.
- Bad, because the map holds only the defs of the files that the run reads. When the token namespaces are outside the
  source paths of the run, every hint is the generic one.

#### No hint (rejected)

Rejected because the author must then search the token namespaces by value. The tool already reads them.

## Consequences

- With configured token namespaces, the rule can block a new color literal in any checked position of a `.cljs` or
  `.cljc` file.
- The baseline holds existing findings at adoption. These can include exact hex strings, hex colors inside larger
  strings and function literals.
- An anchor that is only hex digits, such as `"#add"`, is reported. The author puts it in the baseline.
- Named colors, such as `"white"`, still pass. Review must find them.
- Garden `.clj` files are not checked.
- The baseline identity collapses two equal literals in one top-level form into one entry. A fix of one leaves the other
  in the baseline. File and literal alone would also combine equal literals from different top-level forms.
- After adoption, remove an existing clj-kondo hook so that each literal is reported once.

## Confirmation

- The rule doc `docs/rules/colors.md` leads with the examples of the Appendix. `docs_examples_test.clj` runs them, so
  each flagged example is reported and each fix and left-alone example is not.
- A test for each shape: an exact hex of 3, 4, 6 and 8 digits, a hex in a border, in a gradient and in a shadow, and
  each of `rgb(`, `rgba(`, `hsl(` and `hsla(`, in lower and upper case.
- A test for each boundary that is not a color: `&#160;`, `#/settings`, a hex of 5 or 7 digits, and a hex followed by a
  letter.
- A test for each skipped place: a test file found by `:tests :paths`, a test file found by a `-test` namespace, a
  docstring, a `(comment …)` form, a `#_` form, the `ns` form and an allowed namespace.
- A test for each position: a plain `def`, a Hiccup attribute, interop, a data map, a first style map and a nested
  selector.
- A test for the hint: a match on `#fff` against a `#FFFFFF` token, two tokens with the same value, no match, an alpha
  that differs, and a token that holds `var(--…)`.
- A test that a config of another shape stops the run with a message, and that no config gives no finding.
- `baseline_test.clj` passes, so the rule has a baseline identity.
- In a test fixture under `test-projects/`: `--baseline-write` writes the current literals, `--baseline` exits `0`, a
  new literal makes it exit `1`, and a second `--baseline-write` refuses the new literal without `--allow-new`.

## Revisit when

- A run reports more than 5% false positives among the findings of `color-literal`.
- All configured tokens hold `var(--…)`. Then the value map is empty, and the hint must come from another source, such
  as the CSS file that sets the custom properties.
- A request requires checks of Garden `.clj` files, or `--experimental-clj` gets a way to select groups per path.
- A position-aware reading of named colors, such as one that reads only CSS property values, gives fewer false positives
  than real colors on a run.
- An allowed color with no token is needed in more than one place, and the baseline is not enough.
- cljs-patrol adds an inline marker to ignore one finding for all groups.
- The rule runs on app code for a release with no false positive reported. Then decide whether test code is checked too,
  for example only a test that checks a literal where the code under test uses a token.

## More Information

- Groups and the experimental `.clj` reading: [`docs/rules.md`](../rules.md)
- Baseline: [`docs/baseline.md`](../baseline.md)
- Tiers: [`docs/severity.md`](../severity.md)
- How the `focus-ring` group allows the namespaces of the preset and matches colors by value:
  [`docs/rules/focus-ring.md`](../rules/focus-ring.md),
  [`src/cljs_patrol/groups/focus_ring.clj`](../../src/cljs_patrol/groups/focus_ring.clj)
- How the `typography` group reads design tokens: [`docs/rules/typography.md`](../rules/typography.md),
  [`src/cljs_patrol/groups/typography.clj`](../../src/cljs_patrol/groups/typography.clj)
- Rule docs examples run as tests: [ADR 0004](0004-rule-docs-examples-are-tests.md)

## Appendix: rule doc examples

`color-literal` · cleanup · config [`:allowed-namespaces`](#colors-options): a color written as a literal outside the
design-token namespaces.

```clojure
;; config {:colors {:allowed-namespaces #{app.tokens}}}

;; flagged
(def warning-color "#336699")

;; a Hiccup attribute
[:circle {:fill "#669933" :r 4}]

;; interop
(set! (.-fillStyle ctx) "#fff")

;; a data map
(def chart-options {:series [{:color "rgba(51, 102, 153, 0.4)"}]})

;; a nested selector
(defclass row-style []
  [:&:hover {:background "#cccccc"}])

;; a color inside a larger value
(defclass card-style []
  {:border "1px solid #ddd"})

;; fix
(def warning-color tokens/accent)
```

**Left alone:**

```clojure
;; config {:colors {:allowed-namespaces #{app.tokens}}}

;; left alone: an allowed namespace
(ns app.tokens)
(def accent "#336699")

;; a docstring
(defn tint
  "Mixes a color such as #336699 with white."
  [color amount]
  (mix color tokens/paper amount))

;; an anchor that is not only hex digits
[:a {:href "#/settings"} "Settings"]

;; an HTML entity
(str/replace markup "&nbsp;" "&#160;")

;; a named color, not read in v1
(defclass overlay-style []
  {:background "transparent"})

;; code in a comment form
(comment
  (draw! ctx "#993366"))
```

A finding names the token when one has the same value, such as "Use app.tokens/paper." for `"#FFF"`. When no token has
the value, it says "Use a token from app.tokens."
