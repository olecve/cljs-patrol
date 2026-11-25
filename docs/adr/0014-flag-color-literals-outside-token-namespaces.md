# ADR 0014: Flag Color Literals Outside The Token Namespaces

## Status

Accepted, 2026-11-25 (proposed 2026-11-25)

## Outcome

2026-11-25: [PR #185](https://github.com/olecve/cljs-patrol/pull/185) implements the opt-in rule. The test and native
verification jobs pass. Reader conditionals used as form heads or names and some conditional docs are covered by a
separate follow-up.

## Context

The examples in this record are invented; the survey counts describe a real codebase.

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

Today, the only check that such apps have is a clj-kondo hook. It reads the first style map of a Spade `defclass` or
`defattrs`. It does not see:

- a plain `def`, such as `(def warning-color "#336699")`
- a Hiccup attribute, such as `[:circle {:fill "#669933"}]`
- interop, such as `(set! (.-fillStyle ctx) "#fff")`
- a data map, such as the options of a chart
- a nested selector block of a Spade style
- a color inside a larger CSS value, such as `"1px solid #ddd"`

Two plain `def`s with a color passed code review. A person found them later.

A survey of a real codebase of about 2,250 Clojure files counted the color literals in it. The token namespaces of that
codebase hold 390 color defs, and 249 of them are an exact hex string. Outside the token namespaces, test code excluded,
the survey found these literals. The last column shows what the clj-kondo hook sees.

| Position                    | Hex | `rgb(` and others | Named, real of candidates | Hook sees |
| --------------------------- | --: | ----------------: | ------------------------: | --------- |
| Spade first style map       |   2 |                13 |                     31/31 | yes       |
| Spade nested selector       |   7 |                 9 |                     12/12 | no        |
| Hiccup attribute            |   3 |                 0 |                       3/4 | no        |
| Interop                     |   3 |                 0 |                       1/1 | no        |
| Plain `def`, vectors too    |  43 |                 8 |                       0/5 | no        |
| Data map                    |  81 |                 2 |                     24/89 | no        |
| Garden style vector, `.clj` |   8 |                 2 |                       4/4 | no        |
| Other expression            |   3 |                 2 |                      3/11 | no        |
| **Total**                   | 150 |                36 |                    78/157 | 2 hex     |

Hex counts only an exact string, such as `"#fff"`. `rgb(` counts `rgb(`, `rgba(`, `hsl(` and `hsla(` anywhere in a
string. Test code held 277 more hex literals and 70 more `rgb(` literals. Over the whole codebase, tests included, the
survey found no false positive for hex and none for `rgb(`. For named colors, such as `"red"` or `:gray`, it found 97
real colors and 127 false positives. A false positive is a word that is also a color name, such as a label `"Cyan"` or a
keyword `:gray` in an enum.

Of the 150 hex literals outside the tokens and tests, 29 have the same value as a token, and 121 do not. The survey
compared the values in lower case, with a 3-digit hex expanded to 6 digits, such as `#fff` to `#ffffff`.

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
- **Tests are not checked in v1.** Test code holds more color literals than the app code: 277 hex literals and 70 `rgb(`
  literals in the survey. Most of them have a reason: fixture data, a check of a computed style that the browser gives
  back as a literal, or an input to a color picker or a chart. Reports in tests would fill the baseline with entries
  that nobody must fix. The rule first gets stable on app code.
- **Files.** The group reads `.cljs` and `.cljc` files. It does not read `.clj` files, also not with
  `--experimental-clj`.
- **The hint.** The group builds a map from value to token. It reads each `(def name "#hex")` of an allowed namespace.
  It compares the values in lower case, with a 3-digit or 4-digit hex expanded to 6 or 8 digits. Alpha stays
  significant, so `#ffff` is not `#fff`. A finding names the token, fully qualified, when one matches. When several
  tokens match, it names them all, sorted. When no token matches, or the literal is a function shape, the hint says only
  to use a token from an allowed namespace. A token that holds `var(--…)` gives no hex, so it matches nothing.
- **Baseline identity.** A finding is identified by its file, the name of the top-level form that holds it, and the
  literal text. The line is not part of it, so a reformat does not make old findings new.
- **Adoption.** A project turns the group on and runs `--baseline-write`. Then CI with `--baseline` fails only on a new
  literal. A literal that must stay goes in the baseline with `--allow-new`.
- **The clj-kondo hook.** A project keeps its hook until it adopts the group with a baseline. Then it removes the hook,
  so that the same literal is not reported twice.

## Options

### A new `colors` group (chosen)

- Good, because one group reads every string of a file, so it sees all positions in the table, not only style maps.
- Good, because the group is opt-in. A project with no token namespaces gets no findings.
- Good, because `cleanup` does not fail a plain run. A project that adopts with a baseline fails only on new literals.
- Bad, because cljs-patrol gets one more group and one more config key to document and test.

### Extend the `spade` or `focus-ring` group (rejected)

Rejected because both groups read only Spade style maps. In the survey, 9 of the 150 hex literals outside the tokens
were in a style map: 2 in a first map and 7 in a nested selector. The other 141 were in `def`s, data maps, Hiccup,
interop and Garden vectors.

### Keep only the clj-kondo hook (rejected)

Rejected because the hook sees 2 of the 150 hex literals outside the tokens. It sees 13 of the 36 function literals. The
two plain `def`s that passed review are of the kind it does not see.

### A list of allowed colors (deferred)

- Good, because a project could allow a color that has no token, such as the white of a canvas export.
- Bad, because the rule of the app is that one place holds all colors. A second list is a second place.
- Bad, because the baseline already keeps a literal that must stay, and the reviewer sees it.

### An inline marker to ignore one finding (rejected)

Rejected because cljs-patrol has no such marker in any group. The baseline is how cljs-patrol ignores one finding. A
marker for one group would be a second mechanism.

### Open point 1: a hex color inside a larger string

The exact-string shape misses `"1px solid #ddd"`, a gradient and a shadow. A read-only `rg` run on the surveyed codebase
measured it. It read the non-test source directories, 1,524 Clojure files, and left out the token namespaces. The
bounded pattern of the Decision found 20 hex colors that are not an exact string:

- 11 in code strings. 8 of them are in 7 lines of Garden `.clj` files, such as `"1px solid #ddd"` and
  `"linear-gradient(to right, #336699, #669933)"`. 3 are in `.cljs` files: 2 in one gradient and 1 in a `box-shadow`.
- 9 in `;` comments, which the rule skips.

The token namespaces hold 85 more, in composite borders, gradients and shadows. So a composite value is a usual way to
write a color in that codebase.

The bounded pattern gave no false positive. The strings of the codebase hold 4 anchors. Invented equivalents are
`#example-top`, `#example-content`, `#example-nav` and `#example-view`. None is only hex digits, so none matches. All 22
exact strings that are only the letters `a` to `f` after the `#` are colors, such as `"#ddd"` and `"#FFFFFF"`. No inline
SVG with an escaped hex color was found outside docstrings.

A pattern with no boundaries also matches the HTML entity `&#160;`, because `160` is three hex digits. It found 5 such
lines: 4 in code and 1 in a docstring.

#### Exact strings only (rejected)

- Good, because the shape is the simplest to read and to test.
- Bad, because `"1px solid #ddd"` is the easy way around the rule. After the tokens become `var(--…)`, a composite value
  with a hex color breaks a theme in the same way as an exact one.
- Bad, because the function shape already matches anywhere in a string. The two shapes would differ for no reason.

#### Bounded hex anywhere in a string (chosen)

- Good, because it finds 3 more literals in the `.cljs` files of the surveyed codebase, with no false positive.
- Good, because an exact string is one case of the same pattern, so the rule has one hex shape.
- Bad, because an anchor that is only hex digits, such as `[:a {:href "#add"}]`, is reported. The exact shape has the
  same cost. The surveyed codebase has no such anchor.
- Bad, because `url(#beef)` would match, since `(` is an allowed character before the `#`. The surveyed codebase has no
  such reference.

#### Hex anywhere, with no boundaries (rejected)

Rejected because it matches the HTML entity `&#160;`. The surveyed codebase has 4 such code lines.

### Open point 2: Garden style vectors in `.clj` files

cljs-patrol reads `.clj` files only with `--experimental-clj`. The flag is for groups that are not about ClojureScript:
`docstrings`, `tests`, `layout`, `idioms` and `comments`. Every group that reads Hiccup, re-frame or Spade keeps to
`.cljs` and `.cljc` files.

#### Read `.cljs` and `.cljc` only (chosen)

- Good, because `colors` is about a theme in the browser, like the groups that keep to `.cljs` and `.cljc` files.
- Good, because a `.clj` file is often server code, where a literal is correct. CSS custom properties do not reach an
  HTML email, and many email clients do not support them. The server code of the surveyed codebase holds 14 hex colors
  in `.clj` files: 10 in email templates and 4 in data, such as the color of a tag.
- Bad, because the 8 exact hex literals, 8 embedded hex colors and 2 function literals in the Garden `.clj` files of the
  surveyed codebase are not reported. Those files compile to CSS, so they break a theme too.

#### Read `.clj` files under `--experimental-clj` (deferred)

- Good, because the Garden files would be checked.
- Bad, because `--experimental-clj` applies to every path of the run. A run on a full tree would report the server code.
- Bad, because the flag would change its meaning: today it widens discovery only for groups that are not about
  ClojureScript.

### Open point 3: named colors (deferred)

Deferred because the survey found more false positives (127) than real colors (97). Of the real ones, 50 were
`transparent`, which does not change with a theme. A named color in a string, such as `"red"`, and a keyword, such as
`:gray` in an enum of a component, look the same to a rule that does not know the position.

### Open point 4: the hint

#### Match by value, with a generic hint as the fallback (chosen)

- Good, because a finding can name the token to use. In the survey, 29 of the 150 hex literals outside the tokens match
  a token.
- Good, because the map comes from the allowed namespaces, so it needs no second config.
- Bad, because a token that holds `var(--…)` gives no hex. When all tokens become `var(--…)`, no finding names a token.
- Bad, because the map holds only the defs of the files that the run reads. When the token namespaces are outside the
  source paths of the run, every hint is the generic one.

#### No hint (rejected)

Rejected because the author must then search the token namespaces by value. The tool already reads them.

## Consequences

- A project with token namespaces can block a new color literal in any position of a `.cljs` or `.cljc` file.
- On the surveyed codebase, the rule would report about 180 literals outside the tokens and tests, in `.cljs` and
  `.cljc` files: 142 exact hex strings, 3 hex colors inside a larger string, and 34 function literals. The baseline
  holds them at adoption.
- An anchor that is only hex digits, such as `"#add"`, is reported. The author puts it in the baseline.
- Named colors, such as `"white"`, still pass. Review must find them.
- The Garden `.clj` files of a project are not checked.
- The baseline identity collapses two equal literals in one top-level form into one entry. Then a fix of one leaves the
  other in the baseline. File and literal alone would collapse more. On the surveyed codebase, an `rg` count found 140
  exact hex strings in `.cljs` and `.cljc` files outside the tokens. They give 130 identities by file and literal, so 10
  collapse. The survey, which read the forms, counted 142.
- A project that adopts the group removes its clj-kondo hook, so each literal is reported once.

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
- On a test project under `test-projects/`: `--baseline-write` writes the current literals, `--baseline` exits `0`, a
  new literal makes it exit `1`, and a second `--baseline-write` refuses the new literal without `--allow-new`.

## Revisit when

- A run on a real codebase reports more than 5% false positives among the findings of `color-literal`.
- All tokens of a project that uses the group hold `var(--…)`. Then the value map is empty, and the hint must come from
  another source, such as the CSS file that sets the custom properties.
- A project asks to check its Garden `.clj` files, or `--experimental-clj` gets a way to select groups per path.
- A position-aware reading of named colors, such as one that reads only CSS property values, gives fewer false positives
  than real colors on the surveyed codebase.
- A project needs an allowed color that has no token, in more than one place, and the baseline is not enough.
- cljs-patrol adds an inline marker to ignore one finding for all groups.
- The rule is stable on app code: it has run on a real codebase for a release with no false positive reported. Then
  decide whether test code is checked too, for example only a test that checks a literal where the code under test uses
  a token.

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
