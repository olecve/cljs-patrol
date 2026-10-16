# CSS order rules

The rules of the `css-order` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

- **CSS property order (outside-in)** (`css-property-order-outside-in`, cleanup) — a Spade style map writes its
  properties out of the order the chosen table defines. Each style map is judged on its own — the base map and every
  nested selector block, at any depth — and only maps written as literals in the declaration body are read, so a map
  that `(merge …)` or `(case …)` builds is left alone. A property the table does not name, a custom property (`--*`)
  included, is unordered; it only reads as a problem when a ranked property follows it. Maps under four ranked
  properties are not judged, and only the first property out of place in each block is reported — alongside the whole
  block's target order, so the fix needs no guessing. See [Property-order tables](#property-order-tables) for the choice
  of table

## Property-order tables

The `css-order` group ranks properties with one of five [stylelint](https://stylelint.io) property-order configs, each
embedded verbatim from its npm package. These are the same lists
[`stylelint-order`](https://github.com/hudochenkov/stylelint-order) applies to CSS, so a project already running one of
them in stylelint gets the matching order here.

| Table              | Package                                                                                                                  | Properties | Character                                                                             |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------ | ---------- | ------------------------------------------------------------------------------------- |
| `recess` (default) | [stylelint-config-recess-order](https://github.com/stormwarning/stylelint-config-recess-order)                           | 496        | Positioning, box model, **typography, then background and border**, effects           |
| `clean`            | [stylelint-config-clean-order](https://github.com/kutsan/stylelint-config-clean-order)                                   | 468        | Interaction, positioning, layout, box model (border included), typography, appearance |
| `concentric`       | [stylelint-config-concentric-order](https://github.com/ream88/stylelint-config-concentric-order)                         | 330        | Outside-in: **border and background, then the text inside**                           |
| `smacss`           | [stylelint-config-property-sort-order-smacss](https://github.com/cahamilton/stylelint-config-property-sort-order-smacss) | 225        | Grouped by purpose: content, box, animation, border, background, text                 |
| `idiomatic`        | [stylelint-config-idiomatic-order](https://github.com/ream88/stylelint-config-idiomatic-order)                           | 64         | Structural properties only; the rest left unranked                                    |

They disagree most about where layout ends and looks begin: `recess` puts typography ahead of background and border,
`concentric` does the opposite. Read a finding against the table in use.

Pick one on the command line:

```bash
clojure -M:run --css-order smacss <src-dir>
```

or in `.cljs-patrol/config.edn`, where it persists for the project and for CI:

```clojure
{:css-order {:order :smacss}}
```

An unrecognized name warns on stderr and falls back to `recess` rather than stopping the run.

Every finding carries the target order for its own block, so neither a reader nor another tool has to work out what the
table wants:

```
styles.cljs:16  :app.ui/banner-style {:color "#333" :display :block :padding "4px" :background "#eee"}
      → Move :display before :color. Order for this block: :display :padding :color :background
```

The console and HTML reports print that line; the EDN report carries the full sequence as `:expected-order`, which stays
complete even where the printed hint trails off.

Two deviations from `stylelint-order` worth knowing. A property the chosen table does not name is left unordered here;
`idiomatic` in particular asks stylelint to sort its unlisted properties alphabetically at the bottom, which this does
not do. And nothing is auto-fixed — property order is a write-time review decision.
