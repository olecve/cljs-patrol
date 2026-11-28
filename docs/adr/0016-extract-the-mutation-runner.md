# ADR 0016: Move The Mutation Runner To A Project Of Its Own

## Status

Proposed, 2026-11-28

## Context

[ADR 0005](0005-mutation-testing-in-one-jvm.md) added a mutation runner to `dev/`. ADRs 0006 to 0012 added to it. The
runner changes one definition at a time in one JVM. It runs the tests that call that definition. It reports the mutants
that no test caught.

The runner has these files:

- 2,911 lines of Clojure in `dev/cljs_patrol/dev/mutate.clj`, `dev/cljs_patrol/dev/mutate/` and `mutate_html.clj`.
- The report stylesheet `dev/cljs_patrol/dev/mutate_report.css`, 492 lines. `mutate_html.clj` reads it from the
  classpath.
- 3,896 lines of tests under `test/cljs_patrol/dev/`.

53 commits changed the runner sources. 54 commits changed the sources or the tests.

The runner reads any Clojure source and runs any `clojure.test` suite. It uses only these parts of cljs-patrol:

- Six functions of `cljs-patrol.parser`: `sym-name`, `declared-name-loc`, `position`, `position-span`,
  `written-child-locs` and `child-locs`, which `written-child-locs` calls. They are 40 lines with their docstrings, and
  26 lines without them.
- The test helper `cljs-patrol.temp-dir`, 60 lines. Two runner test namespaces use it. Nine other cljs-patrol test
  namespaces use it too.

Its other dependencies are rewrite-clj, tools.cli and hiccup. rewrite-clj is the parser. The six functions are small
helpers over it.

The first Revisit when condition of ADR 0005 is "The runner is used outside this repository". The runner has no
dependency file of its own. Another project must copy its files, or put this repository's `dev/` folder on its
classpath. This record moves the runner before other projects use it, so that they can use it with one alias.

The runner already changes only the lines of a branch with `-s`. Of the Clojure mutation tools that were checked
(Heretic, mutant-test, mutator and clj-mutate), none has this mode. Heretic is the nearest: it also runs in one warm JVM
and selects tests. Its README says that it needs ClojureStorm and that it is experimental.

## Decision

Move the runner, its tests and its guide to a new repository, `olecve/mutacio-clj`, under the Eclipse Public License
2.0. cljs-patrol then uses it as a git dependency, as any other project can.

- **History.** `git filter-repo` keeps only the runner sources, `mutate_report.css`, the runner tests and
  `docs/mutation-testing.md`, with their commits. `git log` and `git blame` still work in the new repository. The
  stylesheet stays a classpath resource.
- **Names.** The namespaces change from `cljs-patrol.dev.mutate.*` to `mutacio.*`. The main namespace is `mutacio.main`.
  The skip markers `; mutate: skip` and `:mutate/skip`, and the files `.mutate-cache/` and `.mutate-applied.edn`, keep
  their names.
- **Shared code.** The new repository gets its own copies of the six parser functions, and of the temp-dir helper and
  its tests. cljs-patrol keeps its parser and its temp-dir helper. There is no shared library.
- **Records.** ADRs 0005 to 0012 stay in cljs-patrol. The new repository starts its own records at ADR 0001. Its ADR
  index lists ADRs 0005 to 0012 as links to cljs-patrol, pinned to the last commit before the move. This record
  partially supersedes ADR 0005: it replaces the decision to keep the runner in `dev/`. The other parts of ADRs 0005 to
  0012 still describe how the runner works.
- **Release.** A git tag is the release. There is no Clojars release and no native binary.
- **cljs-patrol.** The `:mutate` alias gets the git dependency, pinned by tag and SHA, and `mutacio.main`. The runner
  sources, the stylesheet and the runner tests go. These stay: `dev/` with the file access check, the `:java-io-check`
  alias and its CI step, the `:test` alias and its dependencies, and both `.gitignore` lines. `docs/mutation-testing.md`
  stays as a short page with the command for this repository and a link to the guide in the new repository, so the
  README link still works. The long runner section in `CLAUDE.md` becomes the command and a link to that page.

## Options

### Keep the runner in cljs-patrol (rejected)

- Good, because nothing changes, and the runner and the parser stay in one place.
- Bad, because another project can use the runner only by copying its files or by using this repository's `dev/` folder.
- Bad, because the runner's issues, releases and docs mix with the linter's.

### Publish it from a subfolder of cljs-patrol with `:deps/root` (rejected)

- Good, because the history stays in one repository and needs no split.
- Bad, because each user downloads the whole linter repository to get the runner.
- Bad, because the issues and the repository page stay shared with the linter. Runner tags and linter tags need a naming
  rule to stay apart.
- Bad, because the subfolder cannot use the parser in the linter's `src/` without the linter's dependencies. It needs
  copies of the six functions anyway.

### A new repository with a git dependency (chosen)

- Good, because any project with `deps.edn` can use it with one alias.
- Good, because the runner gets its own name, README, issues and tags.
- Good, because `git filter-repo` keeps the history of each file.
- Good, because a Leiningen plugin can later be added to the project of its own, without the linter.
- Neutral, because 26 lines of helpers over rewrite-clj are now in two places. Both projects use rewrite-clj itself as a
  normal dependency, so the parsing is not copied.
- Bad, because a change to the runner and a change to cljs-patrol that uses it are now two pull requests.

### A new repository with a Clojars release (deferred)

- Good, because Leiningen and Maven tools get a normal Maven artifact. Clojars is the usual public place for it.
- Bad, because each release then has a second step and a second place to fail.
- Deferred until a user asks for it.

### A shared parser library for both projects (rejected)

Rejected because the shared part is six small functions. A library for them costs more than the copies.

## Consequences

cljs-patrol loses 2,911 lines of Clojure and 492 lines of CSS from `dev/`, and 3,896 lines of tests. Its test run gets
shorter by the runner tests.

The runner's cache key has two hashes. One is a hash of the project files, `src/` included. The other is a hash of the
runner's own sources. In cljs-patrol, that second hash includes `fs.clj`, `group.clj`, `parser.clj` and `timings.clj`.
After the move, it includes the copied functions instead. In cljs-patrol, a parser change still clears the cache through
the project hash.

A cljs-patrol contributor who wants a runner change makes it in `mutacio-clj`, tags it, and then updates the tag in
cljs-patrol.

The names `; mutate: skip`, `:mutate/skip`, `.mutate-cache/` and `.mutate-applied.edn` stay, so no skip marker and no
`.gitignore` line in a project needs a change.

At acceptance, the Status of ADR 0005 gets `; partially superseded by ADR 0016`.

## Confirmation

Before acceptance, confirm these results:

- Before the move, record the cljs-patrol commit, the test and assertion counts of the runner tests, and one run:
  `clojure -M:test:mutate cljs-patrol.groups.focus-ring -t cljs-patrol.groups.focus-ring-test`, with no
  `.mutate-cache/`.
- In `mutacio-clj`, the runner tests pass with the same test and assertion counts, and cljs-patrol is not on the
  classpath.
- In cljs-patrol, the same run through the git dependency, with no `.mutate-cache/`, gives the same mutants and the same
  outcomes.
- `git log --follow` in `mutacio-clj` shows commit `adf8d79` for the main namespace.
- `mutacio-clj` has a `LICENSE` file with the EPL-2.0 text. Its CI runs its tests, cljfmt and clj-kondo. The CI of both
  repositories passes.

## Implementation

1. Create `olecve/mutacio-clj` from the filtered history. Rename the namespaces, add the copied functions and the
   temp-dir helper, `deps.edn`, the license, the README, the ADR index and CI. Tag `v0.1.0`.
2. In cljs-patrol, point `:mutate` at the tag, remove the runner sources, the stylesheet and the runner tests, shorten
   `docs/mutation-testing.md` and `CLAUDE.md`, and mark ADR 0005 as partially superseded.

## Revisit when

- A user asks for a Clojars release, or for a Leiningen plugin.
- Two merged cljs-patrol pull requests each need a runner change and a runner tag update.

## More Information

- The runner record: [ADR 0005](0005-mutation-testing-in-one-jvm.md), and its first Revisit when condition.
- The guide: [Mutation testing](../mutation-testing.md).
- The first runner commit: `adf8d79`, "Add a dev-only mutation testing runner that runs in one JVM".
- The cache key: [`cache.clj`](../../dev/cljs_patrol/dev/mutate/cache.clj), `runner-src-files` and `runner-files`.
- The task: [CP-0060](../../backlog/tasks/cp-0060%20-%20Mutation-runner-extract-into-its-own-project.md).
- Heretic, the nearest other tool:
  [README at commit 45ed7c7](https://github.com/parenstech/heretic/blob/45ed7c7deffd5fee1b3050efa7396d0113b30a4f/README.md).
