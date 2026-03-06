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

## Rule groups

Analysis is split into independent rule groups. By default all groups run.

| Group     | Detects                                         |
| --------- | ----------------------------------------------- |
| `re-frame` | Unused/phantom re-frame subscriptions and events |
| `spade`   | Unused Spade style declarations                 |

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

Limit results to a subset of files while still using the full codebase for context:

```bash
clojure -M:run --files src/app/subs.cljs src/cljs/myapp
clojure -M:run --files src/app/subs.cljs,src/app/events.cljs src/cljs/myapp
```

This is useful in CI to surface only issues in files changed by a pull request, while phantom/duplicate detection still considers the whole codebase.
Combinable with other flags:

```bash
clojure -M:run --output edn --files src/app/subs.cljs src/cljs/myapp
```

## Build

Build a standalone uberjar:

```bash
clojure -T:build uber
java -jar target/cljs-patrol-0.1.0.jar <source-dir>
```

## Formatting

```bash
clojure -M:cljstyle fix src/
```
