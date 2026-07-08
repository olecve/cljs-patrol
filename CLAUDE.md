# CLAUDE.md

Guidance for AI assistants working on cljs-patrol.

## What This Tool Does

cljs-patrol is a standalone static analysis CLI for ClojureScript codebases. It detects:

- **Re-frame:** unused/phantom subscriptions and events, dynamic dispatch sites
- **Spade:** unused CSS-in-ClojureScript styles (`defclass`, `defattrs`)
- **A11y:** accessibility issues in Hiccup (`:img` missing `:alt`, invalid `:tabIndex`, `:on-click` on non-interactive tags)
- **Docstrings:** bbatsov style-guide violations on every def (summary, indentation, leading/trailing whitespace)

Exits with code 1 when issues are found (CI-friendly).

## Project Structure

```
src/cljs_patrol/
├── core.clj           # CLI entry point, argument parsing
├── parser.clj         # AST walking, namespace resolution, file discovery
├── reporter.clj       # Console output formatting
├── html_reporter.clj  # Self-contained HTML report generation
└── groups/
    ├── re_frame.clj   # Re-frame analysis rules
    ├── spade.clj      # Spade analysis rules
    └── a11y.clj       # Accessibility rules on literal Hiccup vectors
build.clj              # tools.build uberjar config
deps.edn               # Dependencies and aliases
```

## Running the Tool

```bash
clojure -M:run <src-dir> [<src-dir> ...]        # analyze one or more directories
clojure -M:run --only re-frame <src-dir>         # only run re-frame checks
clojure -M:run --disable spade <src-dir>         # skip spade checks
clojure -M:run --output html <src-dir>           # write report.html
```

## Building

```bash
clojure -T:build uber          # produces target/cljs-patrol-0.1.0.jar
java -jar target/cljs-patrol-0.1.0.jar <src-dir>
```

## Testing

```bash
clojure -M:test          # run all tests
```

Tests live in `test/`, mirroring the `src/` structure. Fixture CLJS files for
integration tests are in `test/fixtures/myapp/`.

## Formatting

```bash
clojure -M:cljfmt fix src/ test/
```

Max line width: **129 characters**. Format before marking work complete.

## Docstrings

The `docstrings` group in this tool enforces the [bbatsov clojure-style-guide](https://github.com/bbatsov/clojure-style-guide) docstring rules on `.cljs`/`.cljc` code. Our own `.clj` sources aren't scanned, but we hold them to the same standard by hand:

- **Summary line** must be a self-contained sentence ending in `.`, `!`, `?`, or `:` (colon is fine when it introduces an indented list/example). Put any additional prose on the next line, not as a continuation of the summary.
- **Continuation lines** in a multi-line docstring must be indented at least to the column of the opening quote.
- **No leading or trailing whitespace** inside the docstring.

Beyond style: **prefer no docstring over a redundant one**. If a well-named function's contract is already clear from the name and signature, skip the docstring. Only write one when the docstring conveys something the reader can't derive: a non-obvious return contract (e.g. "returns nil vs. empty set has different meaning"), a hidden invariant, an enumerated shape, or the reason a conservative choice was made. Restating the name in prose is noise.

## Adding a New Rule Group

Each group is a map with a fixed interface:

```clojure
{:id           :my-group
 :name         "My Group"
 :parse        {:handle-list   (fn [ctx node] ...)
                :handle-vector (fn [ctx node] ...)
                :handle-token  (fn [ctx node] ...)}
 :analyze      (fn [parse-results] ...)     ; returns analysis map
 :report       (fn [analysis] ...)          ; prints to stdout
 :summary-lines (fn [analysis] ...)         ; returns [[label count] ...]
 :failed?      (fn [analysis] ...)          ; returns boolean
 :html-sections [{:title "..." :description "..." :columns [...] :data-fn fn}]}
```

Register the group in `core.clj` in the `all-groups` vector.

## Key Architectural Notes

- **Parse phase:** `parser/analyze-project` walks all `.cljs`/`.cljc` files, calls each group's `:parse` handlers per node type (list, vector, token), and accumulates results per file. Each group declares the file extensions it applies to via `file-extensions`; only matching groups are invoked per file.
- **Analyze phase:** Each group cross-references declarations vs. usages to compute unused/phantom sets.
- **Namespace resolution:** `parser/resolve-kw` handles `::alias/name`, `::local`, and `:plain` keywords using per-file alias maps extracted from `ns` forms.
- **Dynamic sites:** Dispatch/subscribe calls with non-literal keywords are collected separately for manual review and do not trigger a failure.
- **VS Code links:** Both console and HTML reports include `vscode://file/<abs-path>:<line>` links for one-click navigation.
