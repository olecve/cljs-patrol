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
├── timings.clj         # What --timings records and prints
├── groups/             # One namespace per rule group
│   ├── re_frame.clj    #   subs/events: unused, phantom, duplicate, effects
│   ├── spade.clj       #   styles: unused, defattrs-as-data, selector shapes
│   ├── reagent.clj     #   defclass-as-sole-attr, redundant into-hiccup
│   ├── spade/          #   selector-shape helpers
│   ├── a11y.clj        #   accessibility on literal Hiccup vectors
│   ├── css_order.clj   #   CSS property order
│   ├── css_order/      #   the embedded stylelint order tables
│   ├── typography.clj  #   mixed Figma typography token groups
│   ├── tests.clj       #   conventions on test code
│   ├── layout.clj      #   what cljfmt's keypair split leaves behind
│   ├── idioms.clj      #   code a core idiom says more plainly
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

### Mutation testing

Every branch a change adds needs a test that fails when the branch breaks. The dev-only runner checks that in one JVM.
[`docs/mutation-testing.md`](docs/mutation-testing.md) is the guide for people: why, how to run it, and how to read the
report. The details for running it follow:

```bash
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test -s main
```

It changes the code under test, never the tests: it swaps one operator at a time in a `def`, `defn`, `defn-` or
`defmethod` of the target namespace, such as `when` for `when-not`, `<` for `<=`, `some` for `every?` or `true` for
`false`. An integer literal written in decimal changes too, in function code (a `defn`, `defn-` or `defmethod` body, or
a `def` whose value is a `fn` or `#(…)`): `0` and `1` swap, and any other goes up by one, such as `-1` to `0`. A
definition's name, a `defmethod` dispatch value and a `^:const` def, whose value callers copy, get no mutant at all, and
the report lists `^:const` defs apart. A skip marker names a changed number by its text, such as
`; mutate: skip 0: REASON`. Each arity of that function code, but a `#(…)`, also gets a body mutant (`:swap :body`), as
cargo-mutants does for Rust: its whole body becomes `nil`, so a function with no operator, or one whose result no test
checks, still shows up. Its operator is `body`, and it sits at the body's first form, so `--apply 'ROW:COL:body'` and
`; mutate: skip body: REASON` on that line name it; `--apply` picks it by column and operator, since a token there, such
as `80` in `(defn width [] 80)`, has its own mutant. A leading `{:pre … :post …}` map with forms after it stays, and
only the forms after it become `nil`. An empty body or a lone `nil` gets none, since it already gives nil. A first run
of the tests records which tests call each function of the target. Then, for each mutant, it evaluates the mutated form
over the namespace, runs the tests that call that function until the first failure, and evaluates the original back.
Pass every test namespace that exercises the target with `-t`: the run knows only those. Every test runs instead when
the function cannot be recorded (a `defmethod`, a `def` that holds data, or a function with a primitive type hint), when
it is called from a fixture, when a test namespace has a `test-ns-hook`, when the selected tests fail on their own, and
with `-a` (`--all-tests`). A test that only reads what another test wrote is not selected for the function that wrote
it, and a call that a background thread makes after its test ended counts for the test running then, so use `-a` for
such a namespace. If recording the calls makes the tests fail, or run past a timeout given with `--timeout`, every
mutant runs every test, and the report says so. Each mutant ends as one of:

- **killed**: a test failed, or the mutated form threw while it loaded. A failure is recorded before the run is cut
  short, so an assertion that catches exceptions, such as `thrown?`, cannot hide it.
- **no coverage**: the first run saw no call of the function, in a test or in a fixture, and every test passes with the
  mutant. The report names these functions once, under "No coverage", instead of a survivor for each operator. A
  function the tests call only through a value stored when the namespace loaded, such as a `partial`, shows here too.
  When another mutant of the same function is killed or times out, the tests do reach it, so its passing mutants count
  as survivors.
- **survived**: every test passed, so no test pins that branch. It may also be an equivalent mutant, such as `first` for
  `last` on a one-element list.
- **unviable**: the mutated form could not be read or compiled. Not scored.
- **timed out**: the tests ran longer than 1.5 times their own unmutated run plus 2 s, which is the first run for every
  test and the plan check for a function's selected tests (`--timeout` overrides both). Not scored. If its tests still
  run half a second after that, the run stops, since a looping thread would disturb every later run, and the report
  names the definitions whose mutants it did not reach, with how many. Run those with `-d` to get past it.

A definition under metadata or in the `#?` branch Clojure reads counts like any other. Metadata, attribute maps,
`comment` blocks, `#_`, quoted code and `#?` branches Clojure does not read are not mutated. Neither are `defmacro`,
`defmulti`, `defonce`, `defprotocol`, `defrecord` and `deftype`, which Clojure keeps from the first load or other code
captures then, nor top-level forms that are no definition, whose effects would run again with every mutant. The report
has a row per definition with its counts, two scores (killed over all scored mutants, and over the mutants the tests
reach), names what was not mutated or has no coverage and why, and lists every survivor, unviable and timed-out mutant
with its source line. Survivors are grouped by line. On the first 10 lines, the line shows once before the swaps as a
`- ` line, and each survivor shows the line after its swap as a `+ ` line, both without the indent. A line longer than
100 characters is cut to 100, from 40 before the swap, or earlier so the cut ends at the line end; the `- ` line shows
again when a swap needs another part of the line. The survivors on later lines show only their position and swap, and a
line points to `--html` and `-r` for them, or to the files the run writes. A `; mutate: skip OPERATOR: REASON` comment
after a line, or on the line above it in the same form, leaves that operator's mutant out, and `:mutate/skip "REASON"`
on a definition's name or in a `defn`'s attribute map leaves out all of its mutants: they do not run, are not scored,
and are listed as ignored with the reason. Only real comments count. The run stops on a comment that starts like a
marker but is not one, on a `:mutate/skip` that is no reason string, and on one on a form the runner does not mutate; a
marker that matches no mutant or has no code after it in its form prints a warning. `-d` limits the run to named
top-level definitions, `-s REF` to the lines the branch changed since it left a git ref (a body mutant counts when any
of its lines changed), `-r FILE` writes every mutant and its outcome to EDN, and `--html FILE` writes a page that leads
with the test gaps: each one's line before and after the change, a likely reason no test caught it, and the tests that
run for it, then tables by function and by test, and the source. The page needs no network. Its plain words, such as
"planted bug" for a mutant, map to the usual ones in [`docs/mutation-testing.md`](docs/mutation-testing.md).
`--apply 'ROW:COL:OPERATOR'` writes that one mutant into the file and runs nothing, keeping the replaced text in
`.mutate-applied.edn`, and `--restore` puts it back. Neither takes the options of a run, and a second `--apply` or a run
waits for `--restore`. A run writes `.mutate-cache/<target-ns>.edn` in the directory it runs from again after each
mutant's outcome, with the safe write of `--apply`, so a stopped or killed run keeps what it finished; a run that
records no outcome, such as one whose baseline fails, leaves the cache as it was. A cache that cannot be read prints one
warning: the run does not use it, and its first outcome replaces it. A key that cannot be made, or a cache that cannot
be written, prints one warning, and the run goes on without the cache. Each mutant has an `:id` in the cache and the EDN
report, such as `holds-focus?/490:43/remove->filter`: its definition, line, column and swap, and a place: the hash of
its top-level definition's text, its line and column inside that definition, and its swap. The cache's key is a hash of
every file under `src/`, `test/`, `dev/`, `resources/`, `test-projects/` and `docs/`, of `deps.edn` and `README.md`, and
of the classpath directories under the working directory, counting only the files git sees in a git work tree, the
sorted `-t` namespaces, a hash of the runner's own sources (`mutate.clj` and `mutate/*.clj`), and `-a` and `--timeout`.
A run with the same key reuses each cached outcome, a plain timeout included, and runs only the other mutants, and `-d`
and `-s` runs share one cache. A mutant whose tests kept running after the timeout is never reused: the next run runs it
again after every other mutant. When a run with the same key finds every mutant in scope cached, no test runs, the
baseline included. Another key makes the run start over. `--iterate` needs only the same `-t` namespaces, runner and
options: it reuses the caught and unviable outcomes by place, so an edit runs again only the definitions it touches; it
runs the baseline, and a definition whose outcomes are all reused shows the tests the baseline saw call it. It marks
those outcomes `:trusted?`, and a run without `--iterate` runs them again. `--fresh` runs every mutant again. At the end
of a run that prints its report, in every mode, the cache keeps what that run reported, by place, as the last run: each
gap whose place the last run reported is tagged new or still open, on the console and the page, the others count as "not
run then", and the report counts the gaps closed since. A run exits 0 when nothing survives, 1 when something survives
or has no coverage, and 2 when it cannot start or stops early. A value a `def` computes when the namespace loads keeps
the original, so a mutant only it reaches survives.

The runner's command line is `dev/cljs_patrol/dev/mutate.clj`. Each job has its namespace under
`dev/cljs_patrol/dev/mutate/`, and its tests under `test/cljs_patrol/dev/mutate/`: `forms` reads the target into
mutants, skip markers and scope, `run` loads each mutant and runs the tests, `report` summarizes and prints, `apply`
does `--apply` and `--restore`, `git` reads the lines `-s` changed, `cache` keeps each outcome for the next run and
compares the gaps with the last one, and `error` holds the error that stops a run. The end-to-end tests in
`test/cljs_patrol/dev/mutate_test.clj` and `mutate_html_test.clj` also exercise every job, so to mutate a runner
namespace, pass all of its test namespaces with `-t`: `cljs-patrol.dev.mutate-test`, `cljs-patrol.dev.mutate-html-test`
and the `cljs-patrol.dev.mutate.*-test` ones. `fixtures.clj` holds the helpers they share.

## Linting

```bash
clojure -M:clj-kondo --lint src test dev build.clj
```

`test-projects/` is not linted: those fixtures are malformed on purpose.

## Formatting

```bash
clojure -M:cljfmt fix src/ test/ dev/
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
- **A new flag** — the page for the feature it belongs to; the README only if it is one a first-time reader needs.
- **A new config key** — the example in the README's Configuration file section, and the page that explains it.

Every rule's section in `docs/rules/` leads with code, not prose, in this order: a key line,
`` `rule-key` · tier — Summary. `` (with ``· config [`:option`](#anchor)`` before the dash when a setting changes the
rule); a block with a `;; flagged` part and a `;; fix` part; a `**Left alone:**` block where the edges need showing;
and, when the summary is not enough, the full explanation folded into `<details>`. `docs_examples_test.clj` splits each
block into examples: a `;;` comment opening a line starts one, `;; flagged` and `;; fix` included, which runs to the
next such comment, blank lines and all, and an example with no opening comment ends at a blank line. Label an example of
several forms with a comment above its first form, never between its forms, and put a blank line before a form that uses
a name defined above it. A part of an example after a blank line must use a name an earlier part defines, so an
unlabeled example cannot slip into the one above it. Every other `clojure` or `edn` block in `docs/rules.md`,
`docs/rules/*.md` and `README.md` runs too. A config block must pass the config checks. Any other block holds labeled
examples: its first label starts with `flagged`, `left alone` or `fix`, and a later label that starts with none of them
keeps the expectation of the example before it. A `;; config {…}` line gives a block the config its examples need.
[ADR 0004](docs/adr/0004-rule-docs-examples-are-tests.md) records why and lists the failure messages with their fixes.
The test runs each example through the rule as a file of its own: a flagged example must be reported, a fix or
left-alone one must not, every chunk must parse, every rule the group declares needs a section, and every key-line link
must hit a heading. A fix is only checked against its own rule, so it may still trip a sibling rule if copied as is. A
rule PR leads with the same kind of examples.

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
- document it on its group's page under `docs/rules/`, examples first (see Documentation); a new group gets a new page,
  a line in the `docs/rules.md` index, and one line in the README's rule-group list

## Key Architectural Notes

- **Parse phase:** `parser/analyze-project` walks every file an enabled group reads (`.cljs`/`.cljc`, and `.clj` under
  `--experimental-clj`), calls each group's `:parse` handlers per node type (list, vector, token), and accumulates
  results per file. Each group declares the file extensions it applies to via `file-extensions`; only matching groups
  are invoked per file. Files are walked in parallel (`--jobs`), each on one thread, and their results merged in file
  order. A handler that keeps state about the file it reads puts it in `parser/file-state` or `parser/*file-memo*`,
  which `parser/analyze-file` binds afresh per file, never in a namespace-level atom another thread's file would share.
- **Analyze phase:** Each group cross-references declarations vs. usages to compute unused/phantom sets.
- **Namespace resolution:** `parser/resolve-kw` handles `::alias/name`, `::local`, and `:plain` keywords using per-file
  alias maps extracted from `ns` forms.
- **Dynamic sites:** Dispatch/subscribe calls with non-literal keywords are collected separately for manual review and
  do not trigger a failure.
- **VS Code links:** Both console and HTML reports include `vscode://file/<abs-path>:<line>` links for one-click
  navigation.
