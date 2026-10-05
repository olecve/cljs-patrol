# cljs-patrol

Static analysis tool for ClojureScript UI codebases. Detects unused and phantom re-frame subscriptions and events,
silently-broken Spade CSS (pseudo-selectors misplaced inside the main map, comma-vs-descendant selector confusion),
accessibility issues in Hiccup (missing image alt text, invalid tabindex, click handlers on non-interactive tags, empty
interactive elements, form controls without an accessible name), unused Spade CSS styles, and bbatsov docstring
style-guide violations.

## Usage

```bash
clojure -M:run [options] <source-dir> [<source-dir> ...]
clojure -M:run --help
```

Example:

```bash
clojure -M:run src/cljs/myapp
```

Scans skip `target/`, `node_modules/`, `out/` and any dot-directory, so a build that copies sources into its output does
not get reported twice.

By default, exits with code `1` when any blocking issue is found, making it suitable for CI pipelines. The set of
blocking issues can be narrowed with [`--fail-on`](docs/severity.md) and existing issues can be ignored with
[`--baseline`](docs/baseline.md).

### Standalone jar

Download a pre-built jar from [GitHub Releases](https://github.com/olecve/cljs-patrol/releases):

```bash
curl -sL https://github.com/olecve/cljs-patrol/releases/download/v0.0.13/cljs-patrol-0.0.13.jar -o cljs-patrol.jar
java -jar cljs-patrol.jar <source-dir>
```

Native binaries (`cljs-patrol-<version>-linux-x86_64`, `cljs-patrol-<version>-macos-aarch64`) are attached to each
release too — no JVM required.

## Rule groups

Analysis is split into independent rule groups. By default all groups run.

- **`re-frame`** — unused/phantom re-frame subscriptions and events
- **`spade`** — unused Spade style declarations, defattrs in merge, pseudo-selector keys inside the main style map,
  consecutive self-selectors that compile to descendant selectors, `&` away from the front of a string selector,
  combinators written as keywords
- **`reagent`** — defclass used as sole attr (should be defattrs); a redundant `(into [:tag …] (map …))` around a
  mapping body that already keys its elements
- **`typography`** — mixed Figma typography token groups in a single style
- **`a11y`** — accessibility issues in Hiccup: `:img` missing `:alt`, invalid `:tabIndex`, `:on-click` on
  non-interactive tags, empty interactive elements without an accessible name, form controls and name-required roles
  missing an accessible name, one constant name shared by every item of a repeated list, `aria-hidden` on something that
  can still take focus, one interactive element nested inside another, a `<label>` that labels nothing
- **`tests`** — conventions on test code: a `deftest` whose name opens with `a-`, `an-` or `the-`, an assertion message
  sharing a line with its expression, a test that decides what to assert while it runs, a test reading a var through its
  var quote, a `^js` type hint in a test
- **`docstrings`** — bbatsov style-guide violations on every def (summary, indent, whitespace)
- **`css-order`** — Spade style maps whose properties run out of the
  [property order](docs/rules.md#property-order-tables) the chosen stylelint config defines

Run only specific groups:

```bash
clojure -M:run --only re-frame src/cljs/myapp
clojure -M:run --only re-frame,spade src/cljs/myapp
```

Disable specific groups:

```bash
clojure -M:run --disable spade src/cljs/myapp
```

The `docstrings` and `tests` groups are about Clojure rather than ClojureScript, and `--experimental-clj` lets them read
`.clj` files too — see [Reading .clj files](docs/rules.md#experimental-reading-clj-files).

## HTML report

Generate a self-contained HTML report instead of console output:

```bash
clojure -M:run --output html src/cljs/myapp
```

Writes `report.html` in the current directory and prints the summary counts to stdout. File entries in the report are
clickable VS Code links (`vscode://file/...`) that open the file at the exact line. Combinable with other flags:

```bash
clojure -M:run --only re-frame --output html src/cljs/myapp
```

## Documentation

- **[What it detects](docs/rules.md)** — every rule, what it flags, and what it deliberately leaves alone
- **[Baseline](docs/baseline.md)** — adopting on a codebase that already has findings: snapshot what is there, then
  block only what is new
- **[Severity tiers](docs/severity.md)** — which rules block CI, and choosing per tier or per rule with `--fail-on`

## Configuration file

Every setting can live in `.cljs-patrol/config.edn`, where it persists for the project and for CI:

```edn
{:fail-on [:bugs :deprecated-effects]
 :baseline {:path ".cljs-patrol/baseline.edn"
            :strict false
            :quiet false}
 :css-order {:order :recess}
 :a11y {:component-aliases {my.ui/drawer :dialog}}
 :tests {:paths ["test"]}
 :experimental-clj true}
```

A CLI flag overrides the matching setting. See [Severity tiers](docs/severity.md) for `:fail-on`,
[Baseline](docs/baseline.md) for `:baseline`, [Property-order tables](docs/rules.md#property-order-tables) for
`:css-order`, [A11y component aliases](docs/rules.md#a11y-component-aliases) for `:a11y`,
[Test paths](docs/rules.md#test-paths) for `:tests`, and
[Reading .clj files](docs/rules.md#experimental-reading-clj-files) for `:experimental-clj`. A flag written
`--no-experimental-clj` turns that last one off for one run.

## Supported patterns

Re-frame declarations: `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx`, `reg-cofx`

Re-frame usages: `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples (`:dispatch`,
`:dispatch-n`, `:dispatch-later`), `:on-success` / `:on-failure` / `:on-error` http callbacks

Spade declarations: `defclass`, `defattrs`

Spade usages: direct function calls, both qualified (`styles/container-style`) and unqualified (`container-style`)
within the same namespace

## EDN output

Print structured EDN to stdout for programmatic or AI-assisted analysis:

```bash
clojure -M:run --output edn src/cljs/myapp
```

File paths in the output are absolute, making it easy to read files directly. The output includes a `:suggestions` map
with fix guidance for each issue type, useful for AI-assisted remediation. Combinable with other flags:

```bash
clojure -M:run --only re-frame --output edn src/cljs/myapp
```

## Filtering results to specific files

Limit results to a subset of files while still analyzing the full codebase for cross-reference context:

```bash
clojure -M:run --files src/app/subs.cljs,src/app/events.cljs src/cljs/myapp
```

`--files` takes a **single comma-separated string** of file paths. The positional arguments after it are always the
source directories to analyze. Do not pass file paths as positional source-dir arguments.

This is useful in CI to surface only issues in files changed by a pull request, while phantom/duplicate detection still
considers the whole codebase. Combinable with other flags:

```bash
clojure -M:run --output edn --files src/app/subs.cljs src/cljs/myapp
```

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

This runs tests, builds the jar as `cljs-patrol-0.2.0.jar` (version derived from the tag), and creates a
[GitHub Release](https://github.com/olecve/cljs-patrol/releases) with the jar attached.

## Formatting

```bash
clojure -M:cljfmt fix src/ test/
```
