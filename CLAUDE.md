# CLAUDE.md

Guidance for AI assistants working on cljs-patrol.

## What This Tool Does

cljs-patrol is a standalone static analysis CLI for ClojureScript codebases. It detects:

- **Re-frame:** unused/phantom subscriptions and events, dynamic dispatch sites
- **Spade:** unused CSS-in-ClojureScript styles (`defclass`, `defattrs`)
- **A11y:** accessibility issues in Hiccup (`:img` missing `:alt`, invalid `:tabIndex`, `:on-click` on non-interactive
  tags)
- **Docstrings:** bbatsov style-guide violations on every def (summary, indentation, leading/trailing whitespace)

Exits with code 1 when issues are found (CI-friendly).

## Project Structure

```
src/cljs_patrol/
├── core.clj            # CLI entry point, argument parsing, group assembly
├── parser.clj          # AST walking, namespace resolution, file discovery
├── group.clj           # RuleGroup protocol every group implements
├── hiccup.clj          # Shared Hiccup-shape helpers (attrs, tags, result positions)
├── style_blocks.clj    # Shared reading of Spade/Garden selector blocks
├── baseline.clj        # Baseline identities, read/write, comparison
├── severity.clj        # Tier classification and --fail-on parsing
├── fs.clj              # File discovery and path helpers
├── groups/             # One namespace per rule group
│   ├── re_frame.clj    #   subs/events: unused, phantom, duplicate, effects
│   ├── spade.clj       #   styles: unused, defattrs-in-merge, selector shapes
│   ├── reagent.clj     #   defclass-as-sole-attr, redundant into-hiccup
│   ├── a11y.clj        #   accessibility on literal Hiccup vectors
│   ├── css_order.clj   #   CSS property order, with orders/ holding the tables
│   ├── typography.clj  #   mixed Figma typography token groups
│   └── docstrings.clj  #   bbatsov docstring rules
└── reporters/          # console.clj, html.clj, edn.clj, markdown.clj
build.clj               # tools.build uberjar + native-image config
deps.edn                # Dependencies and aliases
docs/                   # Reference pages the README links to (see Documentation)
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

Tests live in `test/cljs_patrol/`, mirroring the `src/` structure. The runner scans that directory and no other, so a
test file placed elsewhere under `test/` never runs. Fixture projects for the integration tests are in
`test/projects/<name>-app/`, out of the runner's way because several of them are malformed on purpose.

## Linting

```bash
clojure -M:clj-kondo --lint src test/cljs_patrol build.clj
```

`test/projects/` is excluded: those fixtures are malformed on purpose.

## Formatting

```bash
clojure -M:cljfmt fix src/ test/
```

Max line width: **129 characters**. Format before marking work complete.

Markdown is formatted separately, by prettier:

```bash
npm ci          # once
npm run format  # rewrite, or `npm run format:check` to verify
```

Prose wraps at **120 characters** (`.prettierrc.json`). `CHANGELOG.md` is excluded because the release workflow prepends
to it on every tag, and `test/projects/` because the Clojure linters skip it too.

## Documentation

The README is for someone who has not decided to use the tool yet: what it is, how to install and run it, the rule
groups one line each, and the config file. It is kept short on purpose — it was 671 lines before the reference moved
out, and the failure mode is appending to it until nobody reads it.

Everything that a reader consults **after** deciding lives under `docs/`:

| Page               | Holds                                                                        |
| ------------------ | ---------------------------------------------------------------------------- |
| `docs/rules.md`    | What every rule flags, what it leaves alone, and the per-group configuration |
| `docs/baseline.md` | Snapshotting an existing codebase and blocking only what is new              |
| `docs/severity.md` | Tiers, `--fail-on`, and composing the two                                    |

So when a change needs documenting:

- **A new rule, or a change to what one flags** — `docs/rules.md`. Add one line to the README's rule-group list only if
  the group itself is new.
- **A new flag** — the page for the feature it belongs to; the README only if it is one a first-time reader needs.
- **A new config key** — the example in the README's Configuration file section, and the page that explains it.

Two things that keep the split honest: every page links back to the README, and a cross-reference between pages is a
relative link with an anchor (`[Property-order tables](rules.md#property-order-tables)`), never a bare `#anchor`, which
silently resolves to the wrong page.

## Docstrings

The `docstrings` group in this tool enforces the
[bbatsov clojure-style-guide](https://github.com/bbatsov/clojure-style-guide) docstring rules on `.cljs`/`.cljc` code.
Our own `.clj` sources aren't scanned, but we hold them to the same standard by hand:

- **Summary line** must be a self-contained sentence ending in `.`, `!`, `?`, or `:` (colon is fine when it introduces
  an indented list/example). Put any additional prose on the next line, not as a continuation of the summary.
- **Continuation lines** in a multi-line docstring must be indented at least to the column of the opening quote.
- **No leading or trailing whitespace** inside the docstring.
- **A body of more than one sentence starts after a blank line.** A single sentence continuing the summary may sit
  directly beneath it; once there are two, the blank line is what keeps the summary readable as a summary.

Beyond style: **prefer no docstring over a redundant one**. If a well-named function's contract is already clear from
the name and signature, skip the docstring — `:missing-docstring` is not enabled, so nothing will ask you for one, and
the params then go inline with the name. Only write one when the docstring conveys something the reader can't derive: a
non-obvious return contract (e.g. "returns nil vs. empty set has different meaning"), a hidden invariant, an enumerated
shape, or the reason a conservative choice was made. Restating the name in prose is noise, and so is describing another
var — "Wraps `severity/count-by-fail-on`" tells the reader to go and read something else.

The three style rules are machine-checked by this tool's own `docstrings` group, which reads `.clj` under
`--experimental-clj`. CI runs this on every push, and so can you:

```bash
clojure -M:run --experimental-clj --only docstrings,tests --fail-on cleanup src test/cljs_patrol build.clj
```

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

Register the group in `core.clj` in the `all-groups` vector, then document it: the rules themselves in `docs/rules.md`,
and one line in the README's rule-group list naming the group.

## Key Architectural Notes

- **Parse phase:** `parser/analyze-project` walks all `.cljs`/`.cljc` files, calls each group's `:parse` handlers per
  node type (list, vector, token), and accumulates results per file. Each group declares the file extensions it applies
  to via `file-extensions`; only matching groups are invoked per file.
- **Analyze phase:** Each group cross-references declarations vs. usages to compute unused/phantom sets.
- **Namespace resolution:** `parser/resolve-kw` handles `::alias/name`, `::local`, and `:plain` keywords using per-file
  alias maps extracted from `ns` forms.
- **Dynamic sites:** Dispatch/subscribe calls with non-literal keywords are collected separately for manual review and
  do not trigger a failure.
- **VS Code links:** Both console and HTML reports include `vscode://file/<abs-path>:<line>` links for one-click
  navigation.
