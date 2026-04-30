# cljs-patrol

Static analysis tool for ClojureScript UI codebases. Detects unused and phantom re-frame subscriptions and events, and unused Spade CSS styles.

## Usage

```bash
clojure -M:run [options] <source-dir> [<source-dir> ...]
clojure -M:run --help
```

Example:

```bash
clojure -M:run src/cljs/myapp
```

Exits with code `1` when issues are found, making it suitable for CI pipelines.

### Standalone jar

Download a pre-built jar from [GitHub Releases](https://github.com/olecve/cljs-patrol/releases):

```bash
curl -sL https://github.com/olecve/cljs-patrol/releases/download/v0.1.0/cljs-patrol-0.1.0.jar -o cljs-patrol.jar
java -jar cljs-patrol.jar <source-dir>
```

## Rule groups

Analysis is split into independent rule groups. By default all groups run.

| Group        | Detects                                              |
| ------------ | ---------------------------------------------------- |
| `re-frame`   | Unused/phantom re-frame subscriptions and events     |
| `spade`      | Unused Spade style declarations, defattrs in merge   |
| `reagent`    | defclass used as sole attr (should be defattrs)      |
| `typography` | Mixed Figma typography token groups in a single style |

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
- **Deprecated effects** — use of `:dispatch-n` (replaced by `:fx`)
- **defclass as sole attr** — `defclass` where every usage is `{:class (style-fn)}` with no other props; should be `defattrs` instead
- **defattrs in merge** — `defattrs` used inside `merge`; should be `defclass` so callers can pass it via `:class` without merge
- **Mixed typography token groups** — typography tokens from different Figma token groups mixed in a single style definition
- **Dynamic dispatch/subscribe sites** — dispatch or subscribe calls with a non-literal keyword (manual review needed)

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

This creates `.cljs-patrol/baseline.edn` with all current issues. Commit this file.

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
clojure -M:cljstyle fix src/
```
