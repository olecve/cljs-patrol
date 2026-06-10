# cljs-patrol

Static analysis tool for ClojureScript UI codebases. Detects unused and phantom re-frame subscriptions and events, unused Spade CSS styles, and bbatsov docstring style-guide violations.

## Usage

```bash
clojure -M:run [options] <source-dir> [<source-dir> ...]
clojure -M:run --help
```

Example:

```bash
clojure -M:run src/cljs/myapp
```

By default, exits with code `1` when any blocking issue is found, making it suitable for CI pipelines. The set of blocking issues can be narrowed with [`--fail-on`](#severity-tiers) and existing issues can be ignored with [`--baseline`](#baseline).

### Standalone jar

Download a pre-built jar from [GitHub Releases](https://github.com/olecve/cljs-patrol/releases):

```bash
curl -sL https://github.com/olecve/cljs-patrol/releases/download/v0.1.0/cljs-patrol-0.1.0.jar -o cljs-patrol.jar
java -jar cljs-patrol.jar <source-dir>
```

## Rule groups

Analysis is split into independent rule groups. By default all groups run.

| Group         | Detects                                                                  |
| ------------- | ------------------------------------------------------------------------ |
| `re-frame`    | Unused/phantom re-frame subscriptions and events                         |
| `spade`       | Unused Spade style declarations, defattrs in merge                       |
| `reagent`     | defclass used as sole attr (should be defattrs)                          |
| `typography`  | Mixed Figma typography token groups in a single style                    |
| `docstrings`  | Bbatsov style-guide violations on every def (summary, indent, whitespace) |

Run only specific groups:

```bash
clojure -M:run --only re-frame src/cljs/myapp
clojure -M:run --only re-frame,spade src/cljs/myapp
```

Disable specific groups:

```bash
clojure -M:run --disable spade src/cljs/myapp
```

## HTML report

Generate a self-contained HTML report instead of console output:

```bash
clojure -M:run --output html src/cljs/myapp
```

Writes `report.html` in the current directory and prints the summary counts to stdout.
File entries in the report are clickable VS Code links (`vscode://file/...`) that open the file at the exact line.
Combinable with other flags:

```bash
clojure -M:run --only re-frame --output html src/cljs/myapp
```

## What it detects

- **Unused subscriptions** — registered with `reg-sub` but never subscribed to
- **Unused events** — registered with `reg-event-*` but never dispatched
- **Unused styles** — declared with `defclass`/`defattrs` but never called
- **Phantom subscriptions** — subscribed to but never declared
- **Phantom events** — dispatched but never declared
- **Duplicate registrations** — two `reg-sub` or `reg-event-*` calls with the same keyword (second silently overwrites the first)
- **reg-event-db returning effects** — `reg-event-db` handler returns an effects-style `{:db ... :dispatch ...}` map; the whole map silently replaces app-db and extra effects are dropped (use `reg-event-fx` instead)
- **Deprecated effects** — use of `:dispatch-n` (replaced by `:fx`)
- **defclass as sole attr** — `defclass` where every usage is `{:class (style-fn)}` with no other props; should be `defattrs` instead
- **defattrs in merge** — `defattrs` used inside `merge`; should be `defclass` so callers can pass it via `:class` without merge
- **Mixed typography token groups** — typography tokens from different Figma token groups mixed in a single style definition
- **Docstring summary** — first line of a multi-line docstring is not a self-contained sentence ending in `.`, `!`, `?`, or `:`
- **Docstring indentation** — continuation lines of a multi-line docstring are indented less than the opening-quote column
- **Docstring leading/trailing whitespace** — docstring starts or ends with whitespace
- **Dynamic dispatch/subscribe sites** — dispatch or subscribe calls with a non-literal keyword (manual review needed)

### Example: reg-event-db returning effects

```clojure
;; BAD — reg-event-db handler returns a {:db ... :dispatch ...} map.
;; The whole map silently becomes the new app-db; :dispatch never fires.
(reg-event-db
 :cart/add-item-success
 (fn [{:keys [db]} [_ item]]
   {:db (-> db
            (update :cart-items conj item)
            (assoc :loading? false))
    :dispatch [:analytics/track :item-added]}))

;; GOOD — switch to reg-event-fx, which expects exactly this shape.
(reg-event-fx
 :cart/add-item-success
 (fn [{:keys [db]} [_ item]]
   {:db (-> db
            (update :cart-items conj item)
            (assoc :loading? false))
    :dispatch [:analytics/track :item-added]}))
```

## Supported patterns

Re-frame declarations: `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx`, `reg-cofx`

Re-frame usages: `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples
(`:dispatch`, `:dispatch-n`, `:dispatch-later`), `:on-success` / `:on-failure` / `:on-error` http callbacks

Spade declarations: `defclass`, `defattrs`

Spade usages: direct function calls, both qualified (`styles/container-style`) and unqualified (`container-style`) within the same namespace

## EDN output

Print structured EDN to stdout for programmatic or AI-assisted analysis:

```bash
clojure -M:run --output edn src/cljs/myapp
```

File paths in the output are absolute, making it easy to read files directly.
The output includes a `:suggestions` map with fix guidance for each issue type, useful for AI-assisted remediation.
Combinable with other flags:

```bash
clojure -M:run --only re-frame --output edn src/cljs/myapp
```

## Filtering results to specific files

Limit results to a subset of files while still analyzing the full codebase for cross-reference context:

```bash
clojure -M:run --files src/app/subs.cljs,src/app/events.cljs src/cljs/myapp
```

`--files` takes a **single comma-separated string** of file paths. The positional arguments after it are always the source directories to analyze. Do not pass file paths as positional source-dir arguments.

This is useful in CI to surface only issues in files changed by a pull request, while phantom/duplicate detection still considers the whole codebase.
Combinable with other flags:

```bash
clojure -M:run --output edn --files src/app/subs.cljs src/cljs/myapp
```

## Baseline

Existing codebases often have many issues. The baseline feature lets you snapshot current issues so CI only fails on **new** ones.

### Setup

1. Run analysis and write the baseline:

```bash
clojure -M:run --baseline-write src/cljs/myapp
```

This creates `src/cljs/myapp/.cljs-patrol/baseline.edn` with all current issues. The baseline file is placed inside the source directory by default. Commit this file.

2. Use `--baseline` in CI:

```bash
clojure -M:run --baseline src/cljs/myapp
```

Exits `0` if every found issue is in the baseline. Exits `1` only on **new** issues.

### Why keyword-based baselines survive refactors

Re-frame issues are keyed by their fully-qualified keyword (e.g. `:app.subs/users`), not by file path or line number. This means baselines survive file moves and renames without regeneration - a real advantage over line-based baselines in JS/TS tools.

### Output with baseline

Console output tags each issue as `[NEW]` or `[BASE]`. A summary shows counts:

```
Found 12 issues: 2 new, 8 in baseline, 3 fixed.
3 baseline issues no longer present - consider running --baseline-write to refresh.
```

All output formats work with `--baseline`:

```bash
clojure -M:run --baseline --output edn src/cljs/myapp   # {:new-issues [...] :baseline-issues [...] :fixed-issues [...] :exit-code 0}
clojure -M:run --baseline --output html src/cljs/myapp  # report.html with visual new/baseline distinction
```

### Strict mode

By default, fixed issues (present in baseline but no longer found) don't cause CI failure. Use `--strict-baseline` to require baseline regeneration when issues disappear:

```bash
clojure -M:run --baseline --strict-baseline src/cljs/myapp
```

This prevents quiet drift where issues get "fixed by ignoring."

### Quiet mode

Suppress baseline issues from output, showing only new ones. Useful for PR comment bots:

```bash
clojure -M:run --baseline --quiet-baseline src/cljs/myapp
```

### Changed-files PR pattern

Combine `--files` with `--baseline` to check only files changed in a PR:

```bash
changed=$(git diff --name-only origin/main...HEAD | grep -E '\.clj[sc]?$' | paste -sd, -)
clojure -M:run --baseline --files "$changed" src/cljs/myapp
```

Note: `--baseline-write` cannot be combined with `--files` (would write a partial baseline).

### Configuration file

Baseline settings can be configured in `.cljs-patrol/config.edn`:

```edn
{:baseline {:path ".cljs-patrol/baseline.edn"
            :strict false
            :quiet false}}
```

CLI flags override config file settings.

## Severity tiers

By default, any issue causes CI to fail. For incremental adoption — or just to focus signal on what matters most — `--fail-on` controls which rules block CI. Every issue is still reported regardless; only the exit code is gated.

### Tiers

| Tier | Rules | Why |
| ---- | ----- | --- |
| `bugs` | `duplicate-subs`, `duplicate-events`, `reg-event-fx-empty`, `reg-event-db-empty`, `reg-event-db-returning-effects` | Silent runtime breakage — duplicate registrations overwrite, empty-effect handlers clobber app-db, and effects-style `reg-event-db` returns replace app-db with the effects map. |
| `deprecations` | `deprecated-effects`, `defclass-as-sole-attr`, `defattrs-in-merge`, `mixed-token-groups` | Deprecated APIs and idiomatic violations that may break later. |
| `cleanup` | `unused-subs`, `unused-events`, `unused-styles`, `phantom-subs`, `phantom-events`, `reg-sub-=>-1-arity`, `reg-event-fx-db-only`, `docstring-summary`, `docstring-indentation`, `docstring-leading-trailing-whitespace` | Dead code, style noise, and suspicious references with no runtime impact. |

`dynamic-sites` is info-only — it never affects the exit code.

### Usage

```bash
clojure -M:run --fail-on bugs src/cljs/myapp
clojure -M:run --fail-on bugs,deprecations src/cljs/myapp
clojure -M:run --fail-on phantom-subs,duplicate-subs src/cljs/myapp
clojure -M:run --fail-on all src/cljs/myapp     # every classified rule blocks
```

Tier names, individual rule keys, and the meta value `all` can be mixed. Unknown tokens error with a hint.

### Output with --fail-on

Console output adds a `[BLOCKING]` marker to section headers for rules in the failing set, and a summary line shows the breakdown:

```
=== Duplicate subs (1) [BLOCKING] ===
  :app.subs/users   src/app/subs.cljs:5

=== Unused subs (3) ===
  :app.subs/old     src/app/subs.cljs:12
  ...

1 blocking, 3 warnings.
```

EDN output adds `:blocking-count`, `:warning-count`, and (for baseline mode) `:tier` on each issue. HTML output shows the same blocking badge and a tier-summary panel at the top.

### Listing rules

To see every rule and its tier (handy when picking what to put in `--fail-on`):

```bash
clojure -M:run --list-rules
clojure -M:run --only re-frame --list-rules    # scope to one group
```

### Composing with --baseline

This is the headline combo. With both `--baseline` and `--fail-on`, an issue causes exit 1 only if it is **both new (not in baseline) and in a failing tier**:

```bash
# Adopting on a messy codebase: snapshot once, then block only new bugs in CI.
clojure -M:run --baseline-write src/cljs/myapp
clojure -M:run --baseline --fail-on bugs src/cljs/myapp
```

What this means for CI:
- Old issues already in the baseline: never block, regardless of tier.
- New issues in failing tiers (here, `bugs`): block CI immediately.
- New issues in non-failing tiers (deprecations, cleanup): printed with `[NEW]` but don't block.

`--strict-baseline` still applies on top: fixed baseline issues always block when set, regardless of tier (forces baseline regeneration).

### Greenfield project

For a project starting fresh, no baseline is needed:

```bash
clojure -M:run --fail-on bugs,deprecations src/cljs/myapp
```

This blocks on real problems while leaving cleanup items as visible warnings.

### Configuration file

`--fail-on` can be set in `.cljs-patrol/config.edn` as a vector of keywords:

```edn
{:fail-on [:bugs :deprecated-effects]
 :baseline {:path ".cljs-patrol/baseline.edn"
            :strict false
            :quiet false}}
```

CLI flag overrides config file setting.

## Build

Build a standalone uberjar:

```bash
clojure -T:build uber
```

This produces `target/cljs-patrol-dev.jar`. To set a specific version:

```bash
clojure -J-Dcljs-patrol.version=0.2.0 -T:build uber
```

This produces `target/cljs-patrol-0.2.0.jar`.

### Releasing

Releases are automated via GitHub Actions. Push a version tag to build and publish:

```bash
git tag v0.2.0
git push origin v0.2.0
```

This runs tests, builds the jar as `cljs-patrol-0.2.0.jar` (version derived from the tag), and creates a [GitHub Release](https://github.com/olecve/cljs-patrol/releases) with the jar attached.

## Formatting

```bash
clojure -M:cljfmt fix src/ test/
```
