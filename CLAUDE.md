# CLAUDE.md

Guidance for AI assistants working on cljs-patrol.

## What This Tool Does

cljs-patrol is a standalone static analysis CLI for ClojureScript codebases. The rule groups are listed one line each in
the README's [Rule groups](README.md#rule-groups), and every rule is under [docs/rules/](docs/rules.md), one page per
group.

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
│   ├── spade/          #   selector-shape helpers
│   ├── a11y.clj        #   accessibility on literal Hiccup vectors
│   ├── css_order.clj   #   CSS property order
│   ├── css_order/      #   the embedded stylelint order tables
│   ├── typography.clj  #   mixed Figma typography token groups
│   ├── tests.clj       #   conventions on test code
│   ├── layout.clj      #   what cljfmt's keypair split leaves behind
│   ├── idioms.clj      #   nesting a thread would read better
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
clojure -T:build uber                                    # produces target/cljs-patrol-dev.jar
clojure -J-Dcljs-patrol.version=0.2.0 -T:build uber      # produces target/cljs-patrol-0.2.0.jar
java -jar target/cljs-patrol-dev.jar <src-dir>
```

### Releasing

Push a version tag, and `.github/workflows/release.yml` does the rest: it runs the tests, builds the jar and the native
binaries with the version taken from the tag, creates the
[GitHub Release](https://github.com/olecve/cljs-patrol/releases) with them attached, and prepends the release notes to
`CHANGELOG.md` on `main`.

```bash
git tag v0.2.0
git push origin v0.2.0
```

## Testing

```bash
clojure -M:test          # run all tests
```

Tests live in `test/`, mirroring the `src/` structure. Fixture projects for the integration tests are in
`test-projects/<name>-app/`, outside `test/` because `test/` is a classpath root the runner scans: a fixture named
`*_test.clj` there would be loaded as a namespace, and several fixtures are malformed on purpose.

## Linting

```bash
clojure -M:clj-kondo --lint src test build.clj
```

`test-projects/` is not linted: those fixtures are malformed on purpose.

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
to it on every tag, and `test-projects/` because the Clojure linters skip it too.

## Documentation

The README is for someone who has not decided to use the tool yet: what it is, how to install and run it, the rule
groups one line each, and the config file. It is kept short on purpose — it was 671 lines before the reference moved
out, and the failure mode is appending to it until nobody reads it.

Everything that a reader consults **after** deciding lives under `docs/`:

| Page                    | Holds                                                                                   |
| ----------------------- | --------------------------------------------------------------------------------------- |
| `docs/rules.md`         | The index of rule groups, and reading `.clj` under `--experimental-clj`                 |
| `docs/rules/<group>.md` | What every rule in the group flags, what it leaves alone, and the group's configuration |
| `docs/baseline.md`      | Snapshotting an existing codebase and blocking only what is new                         |
| `docs/severity.md`      | Tiers, `--fail-on`, and composing the two                                               |

So when a change needs documenting:

- **A new rule, or a change to what one flags** — its group's page, `docs/rules/<group>.md`. A new group gets a new
  page, a line in the `docs/rules.md` index, and one line in the README's rule-group list.

Every rule's section leads with code, not prose: a `;; flagged` part, a `;; fix` part, and a `**Left alone:**` block
where the edges need showing, with the full explanation folded into `<details>` below them. `docs_examples_test.clj`
runs every blank-line-separated chunk through the rule as a file of its own, so a flagged example must be reported and a
fix or left-alone one must not; an example that drifts from the tool fails the build. A rule PR leads with the same kind
of examples.

- **A new flag** — the page for the feature it belongs to; the README only if it is one a first-time reader needs.
- **A new config key** — the example in the README's Configuration file section, and the page that explains it.

Two things that keep the split honest: every page links back to the README, and a cross-reference between pages is a
relative link with an anchor (`[Property-order tables](rules/css-order.md#property-order-tables)`), never a bare
`#anchor`, which silently resolves to the wrong page.

## Docstrings

The `docstrings` group in this tool enforces the
[bbatsov clojure-style-guide](https://github.com/bbatsov/clojure-style-guide) docstring rules, and our own `.clj`
sources follow them too:

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

The first three are machine-checked: CI runs the tool on its own sources under `--experimental-clj`, in the "Patrol our
own sources" step of `.github/workflows/test.yml`. Copy the command from there to run it locally.

## Adding a New Rule Group

Each group is a record implementing the `RuleGroup` protocol in `group.clj`: `group-id`, `group-name`, `parse-handlers`,
`analyze`, `summary-lines`, `failed?`, `suggestions`, `rule->tier` and `file-extensions`. A group with config takes it
through a `make-group` fn; see `layout.clj` for a small one.

Register the group in `assemble-groups` in `core.clj`. Then, for every rule it adds:

- give it a tier in `rule->tier` and list it under that tier in `docs/severity.md`
- give it a baseline identity in one of the rule sets at the top of `baseline.clj`; `baseline_test.clj` fails for a rule
  that has none
- if it holds on `.clj`, add it to the `--fail-on` list of the "Patrol our own sources" step in `test.yml`, or it will
  not gate
- document it on its group's page under `docs/rules/`; a new group gets a new page, a line in the `docs/rules.md` index,
  and one line in the README's rule-group list

## Key Architectural Notes

- **Parse phase:** `parser/analyze-project` walks every file an enabled group reads (`.cljs`/`.cljc`, and `.clj` under
  `--experimental-clj`), calls each group's `:parse` handlers per node type (list, vector, token), and accumulates
  results per file. Each group declares the file extensions it applies to via `file-extensions`; only matching groups
  are invoked per file.
- **Analyze phase:** Each group cross-references declarations vs. usages to compute unused/phantom sets.
- **Namespace resolution:** `parser/resolve-kw` handles `::alias/name`, `::local`, and `:plain` keywords using per-file
  alias maps extracted from `ns` forms.
- **Dynamic sites:** Dispatch/subscribe calls with non-literal keywords are collected separately for manual review and
  do not trigger a failure.
- **VS Code links:** Both console and HTML reports include `vscode://file/<abs-path>:<line>` links for one-click
  navigation.
