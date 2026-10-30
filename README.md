# cljs-patrol

Static analysis for ClojureScript UI codebases: dead and missing re-frame registrations, Spade CSS that silently matches
nothing, accessibility mistakes in Hiccup, and conventions on docstrings, tests and layout. See
[Rule groups](#rule-groups) for the full list.

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

Download a pre-built jar from [GitHub Releases](https://github.com/olecve/cljs-patrol/releases), with `VERSION` set to
the latest one:

```bash
VERSION=0.0.34
curl -sL "https://github.com/olecve/cljs-patrol/releases/download/v$VERSION/cljs-patrol-$VERSION.jar" -o cljs-patrol.jar
java -jar cljs-patrol.jar <source-dir>
```

Native binaries (`cljs-patrol-<version>-linux-x86_64`, `cljs-patrol-<version>-macos-aarch64`) are attached to each
release too — no JVM required.

## Rule groups

Analysis is split into independent rule groups. By default all groups run.

- **[`re-frame`](docs/rules/re-frame.md)** — unused, phantom and duplicate subscriptions and events, handlers that
  clobber app-db, deprecated effects
- **[`spade`](docs/rules/spade.md)** — unused styles, and selectors that compile to CSS matching nothing
- **[`reagent`](docs/rules/reagent.md)** — a `defclass` that should be a `defattrs`, and a redundant `into` around keyed
  Hiccup
- **[`typography`](docs/rules/typography.md)** — mixed Figma typography token groups in a single style
- **[`a11y`](docs/rules/a11y.md)** — accessibility in Hiccup: missing alt text and accessible names, controls a keyboard
  cannot reach, nested controls, labels that label nothing
- **[`tests`](docs/rules/tests.md)** — conventions on test code: test names, assertion messages, conditional assertions,
  var derefs, `^js` hints
- **[`react-testing-library-cljs`](docs/rules/react-testing-library-cljs.md)** — react-testing-library-cljs queries used
  where they cannot fail, such as a presence check on a `get-by-*` lookup, and helpers whose name lacks the prefix of
  the lookup they return
- **[`docstrings`](docs/rules/docstrings.md)** — bbatsov style-guide violations on every def (summary, indent,
  whitespace)
- **[`layout`](docs/rules/layout.md)** — the staircase cljfmt's `:split-keypairs-over-multiple-lines?` leaves behind and
  `cljfmt check` accepts
- **[`idioms`](docs/rules/idioms.md)** — code a core idiom says more plainly: three or more calls nested one inside
  another that a `->` or `->>` thread would read top to bottom, a `cond` closing on an `:else nil` it returns anyway, an
  `if` or `when` whose test is a `not`, an `into` from a collection into an empty vector or set, a `+` or `-` of a
  literal `1` that `inc` or `dec` names, a `not` of an `=` that `not=` names, and a `cond->` or `cond->>` clause whose
  test is `true`
- **[`css-order`](docs/rules/css-order.md)** — Spade style properties out of the
  [order](docs/rules/css-order.md#property-order-tables) a stylelint config defines

Run only specific groups:

```bash
clojure -M:run --only re-frame src/cljs/myapp
clojure -M:run --only re-frame,spade src/cljs/myapp
```

Disable specific groups:

```bash
clojure -M:run --disable spade src/cljs/myapp
```

The `docstrings`, `tests`, `layout` and `idioms` groups are about Clojure rather than ClojureScript, and
`--experimental-clj` lets them read `.clj` files too — see
[Reading .clj files](docs/rules.md#experimental-reading-clj-files).

## Output

Console output is the default, laid out like ESLint's: findings grouped by file, one line each with the line, severity,
message and rule, then the totals and a docs link per rule that fired.

```
test/app/views_test.cljs
  14  warning  (is (some? (s/get-by-text "Saved"))): s/get-by-text throws on a miss, so this check never fails. Use s/query-by-text.  get-by-presence-check
  31  warning  save-button: Rename to get-save-button, after the get-by-role lookup it returns.                                       lookup-helper-name

✖ 2 problems (0 errors, 2 warnings)

Rule docs:
  get-by-presence-check  https://github.com/olecve/cljs-patrol/blob/main/docs/rules/react-testing-library-cljs.md#get-by-presence-check
  lookup-helper-name     https://github.com/olecve/cljs-patrol/blob/main/docs/rules/react-testing-library-cljs.md#lookup-helper-name
```

Two other formats are available.

### HTML report

Generate a self-contained HTML report instead of console output:

```bash
clojure -M:run --output html src/cljs/myapp
```

Writes `report.html` in the current directory and prints the summary counts to stdout. File entries in the report are
clickable VS Code links (`vscode://file/...`) that open the file at the exact line. Combinable with other flags:

```bash
clojure -M:run --only re-frame --output html src/cljs/myapp
```

### EDN output

Print structured EDN to stdout for programmatic or AI-assisted analysis:

```bash
clojure -M:run --output edn src/cljs/myapp
```

File paths in the output are absolute, making it easy to read files directly. The output includes a `:suggestions` map
with a short fix for each issue type, and a `:docs` map with the URL of each rule's section in the docs of the running
version (`docs/rules/<group>.md` in this repository), where the edge cases and examples live. Combinable with other
flags:

```bash
clojure -M:run --only re-frame --output edn src/cljs/myapp
```

## Checking only some files

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

## Documentation

- **[What it detects](docs/rules.md)** — every rule, what it flags, and what it deliberately leaves alone, one page per
  rule group
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
 :layout {:split-map-after-symbol true}
 :idioms {:min-calls 3}
 :experimental-clj true}
```

A CLI flag overrides the matching setting. See [Severity tiers](docs/severity.md) for `:fail-on`,
[Baseline](docs/baseline.md) for `:baseline`, [Property-order tables](docs/rules/css-order.md#property-order-tables) for
`:css-order`, [A11y component aliases](docs/rules/a11y.md#a11y-component-aliases) for `:a11y`,
[Test paths](docs/rules/tests.md#test-paths) for `:tests`, [Layout options](docs/rules/layout.md#layout-options) for
`:layout`, [Idioms options](docs/rules/idioms.md#idioms-options) for `:idioms`, and
[Reading .clj files](docs/rules.md#experimental-reading-clj-files) for `:experimental-clj`. A flag written
`--no-experimental-clj` turns that last one off for one run.
