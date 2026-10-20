# Severity tiers

Which rules block CI and which only report, and how to choose per tier or per rule with `--fail-on`.

[← back to the README](../README.md)

Without `--fail-on`, what fails depends on the run. A plain run fails when a group's own default check trips:
`re-frame`, `spade`, `typography` and `a11y` block on most of their findings, unused registrations and styles included,
while `reagent`, `docstrings`, `tests`, `css-order`, `layout` and `idioms` only report. With `--baseline`, every new
finding fails, whatever its tier. `--fail-on` replaces both with the rules you name. Every issue is still reported
regardless; only the exit code is gated.

## Tiers

**`bugs`** — silent runtime breakage: a registration overwritten, app-db clobbered, a control that a keyboard or
screen-reader user cannot reach, name or tell apart, and a Spade selector that matches nothing.

- `duplicate-subs`, `duplicate-events`
- `reg-event-fx-empty`, `reg-event-db-empty`, `reg-event-db-returning-effects`
- `img-alt-missing`, `invalid-tabindex`, `on-click-on-non-interactive`, `empty-interactive-element`,
  `missing-accessible-name`, `repeated-accessible-name`, `aria-live-contradicts-role`, `aria-hidden-focusable`,
  `nested-interactive-element`, `label-not-associated`
- `pseudo-in-main-map`, `consecutive-self-selectors`, `spade-ampersand-not-at-start`,
  `spade-keyword-combinator-selector`

**`deprecations`** — deprecated APIs and idiomatic violations that may break later.

- `deprecated-effects`, `defclass-as-sole-attr`, `defattrs-in-merge`, `mixed-token-groups`

**`cleanup`** — dead code, style noise, and suspicious references with no runtime impact.

- `unused-subs`, `unused-events`, `unused-styles`, `phantom-subs`, `phantom-events`
- `reg-sub-=>-1-arity`, `reg-event-fx-db-only`, `redundant-into-hiccup`
- `docstring-summary`, `docstring-indentation`, `docstring-leading-trailing-whitespace`
- `css-property-order-outside-in`, `deftest-leading-article`, `assertion-message-inline`, `conditional-assertion`,
  `var-deref-in-test`, `js-hint-in-test`
- `form-after-split-map`, `split-map-after-sibling`, `nested-call-chain`, `cond-else-nil`, `negated-test`, `into-empty`,
  `plus-minus-one`

`dynamic-sites` is info-only — it never affects the exit code.

## Usage

```bash
clojure -M:run --fail-on bugs src/cljs/myapp
clojure -M:run --fail-on bugs,deprecations src/cljs/myapp
clojure -M:run --fail-on phantom-subs,duplicate-subs src/cljs/myapp
clojure -M:run --fail-on all src/cljs/myapp     # every classified rule blocks
```

Tier names, individual rule keys, and the meta value `all` can be mixed. Unknown tokens error with a hint.

## Output with --fail-on

Console output adds a `[BLOCKING]` marker to section headers for rules in the failing set, and a summary line shows the
breakdown:

```
=== Duplicate subs (1) [BLOCKING] ===
  :app.subs/users   src/app/subs.cljs:5

=== Unused subs (3) ===
  :app.subs/old     src/app/subs.cljs:12
  ...

1 blocking, 3 warnings.
```

EDN output adds `:blocking-count`, `:warning-count`, and (for baseline mode) `:tier` on each issue. HTML output shows
the same blocking badge and a tier-summary panel at the top.

## Listing rules

To see every rule and its tier (handy when picking what to put in `--fail-on`):

```bash
clojure -M:run --list-rules
clojure -M:run --only re-frame --list-rules    # scope to one group
```

## Composing with --baseline

This is the headline combo. With both `--baseline` and `--fail-on`, an issue causes exit 1 only if it is **both new (not
in baseline) and in a failing tier**:

```bash
# Adopting on a messy codebase: snapshot once, then block only new bugs in CI.
clojure -M:run --baseline-write src/cljs/myapp
clojure -M:run --baseline --fail-on bugs src/cljs/myapp
```

What this means for CI:

- Old issues already in the baseline: never block, regardless of tier.
- New issues in failing tiers (here, `bugs`): block CI immediately.
- New issues in non-failing tiers (deprecations, cleanup): printed with `[NEW]` but don't block.

`--strict-baseline` still applies on top: fixed baseline issues always block when set, regardless of tier (forces
baseline regeneration).

## Greenfield project

For a project starting fresh, no baseline is needed:

```bash
clojure -M:run --fail-on bugs,deprecations src/cljs/myapp
```

This blocks on real problems while leaving cleanup items as visible warnings.

`--fail-on` can also be set in `.cljs-patrol/config.edn` — see [Configuration file](../README.md#configuration-file). A
CLI flag overrides what the file says.
