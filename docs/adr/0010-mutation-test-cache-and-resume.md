# ADR 0010: Keep The Results Of A Mutation Run To Resume And Iterate

## Status

Accepted, 2026-11-21 (proposed 2026-11-21)

## Context

Each run of the runner of [ADR 0005](0005-mutation-testing-in-one-jvm.md) starts from nothing. Three problems follow
from that:

- **A stopped run loses its work.** The runner stops at the first mutant whose tests keep running past the timeout,
  because its test thread keeps running and can change the result of each later mutant (ADR 0005). On
  `cljs-patrol.groups.reagent`, the run plans 231 mutants. In 1 of 5 runs on `main`, it stopped at the mutant at 302:59
  after 83 mutants, and 148 mutants did not run. The other 4 runs reached all 231 mutants, and in those runs a test
  caught that mutant. Whether that mutant times out depends on the order of the tests, which changes from one JVM to the
  next.
- **Closing gaps is slow.** To close a gap, the author writes a test and runs the runner again. Each run repeats every
  mutant that a test already caught, though the author changed only tests.
- **Progress does not show.** A run cannot say which gaps the author closed since the last run, or which gap is new.

Patrol Backlog #79 asked for a cache of results and a resume of a stopped run. With `--iterate`, cargo-mutants runs only
the mutants that the last run did not catch. Stryker reuses the results of the last run in its incremental mode.

## Decision

The runner keeps the outcome of each mutant in a cache, and reuses an outcome when nothing that decides it changed.

- **A stable id.** A mutant is named by its definition, line, column, operator and replacement, such as
  `holds-focus?/490:43/remove->filter`. The EDN report and the cache carry it. The page shows it only for the gaps that
  closed since the last run.
- **A place.** An id holds a line, and a line moves when the file changes above it. So the runner also names a mutant by
  a place: the hash of the text of its top-level definition, its line and column inside that definition, and its
  operator and replacement. `--iterate` and the progress match by place.
- **The cache.** `.mutate-cache/<target-ns>.edn` in the directory the command runs from. The repository's `.gitignore`
  lists `.mutate-cache/`.
- **The key.** One hash of the path and the bytes of every file under `src/`, `test/`, `dev/`, `resources/`,
  `test-projects/` and `docs/`, of `deps.edn` and `README.md`, and of each directory on the classpath under the
  directory the command runs from. The target and its tests load from the classpath, and the code reads `resources/`.
  The tests read the fixtures in `test-projects/` and the examples in `docs/` and `README.md`. In a git work tree, only
  the files that git sees count: tracked files, and untracked files that git does not ignore, so a `.DS_Store` or a
  generated report does not change the key. Outside git, or when git lists no file under the roots, every file counts
  but those in `.git/` and `.mutate-cache/`. Each file adds its path and its length before its bytes, so text that moves
  from one file to the next changes the key. The key also holds the names of the `-t` namespaces in name order, the hash
  of the runner's own source and of the parser it reads forms through, and the options that change an outcome, `-a` and
  `--timeout`.
- **When an outcome is reused.** A run with the same key reuses every cached outcome and runs only the other mutants.
  When any part of the key differs, the run uses no cached outcome and says which part changed. Its first outcome
  replaces the cache. A run that reports no outcome, such as one whose tests fail before any mutation or whose scope is
  empty, leaves the cache as it was. Every other run that prints its report saves it, also one that reused every
  outcome.
- **Resume.** The run writes the whole cache again after each mutant, with the same safe write as `--apply`: a temporary
  file, then one move. A run that stops or is killed keeps every outcome it finished. A run whose tests fail before any
  mutation, or that stops before its first outcome, keeps the last run's cache. The next run runs the mutant that
  stopped the run last, after every other mutant. When that mutant times out again, the run stops there and loses
  nothing. When it does not, it gets its real outcome. This covers a mutant whose tests keep running. A definition whose
  own tests keep running without a mutation still stops each run, as ADR 0005 decides, and the run says so. A cache that
  the run cannot read gives one warning: the run does not use it, and its first outcome replaces it. A key that the run
  cannot make, or a cache that it cannot write, gives one warning that names the file or what failed, and the run goes
  on without the cache.
- **`--iterate`.** For the work of writing tests and fixing gaps: when the sources changed, `--iterate` reuses the
  cached outcomes of the mutants that were caught or did not compile. When nothing changed since the last `--iterate`
  run, it resumes: it keeps reusing the outcomes it trusts, runs only what is left, and runs no baseline when nothing is
  left. It matches by place, not by id. An edit thus keeps the outcomes of every definition it does not touch, also a
  definition that moved, and runs again each mutant of a definition it touches, also for a skip marker inside it. It
  runs again the mutants that survived, had no coverage or timed out, and the new mutants. It does not keep the test
  plans or the notes of the last run, because the sources changed. After a change, it runs the baseline, also when it
  reuses every outcome, and the report names for each definition the tests that the baseline saw call it. The cache
  keeps the test lists that the report used, so a later run that runs no baseline still names them. The `-t` namespaces,
  the runner's source and the options must be the same as in the cache. The run says how many outcomes it reused, and
  that it trusts them. The cache marks these outcomes as trusted, drops the name of the test that caught them, which may
  be gone, and only another `--iterate` run reuses them: a plain run checks them again.
- **`--fresh`.** It ignores the cache and starts over.
- **No work when all is known.** When a run with the same key finds every outcome in the cache, it runs no baseline and
  no test, and uses the test plans and the notes that the cache holds for the definitions in scope. The cache keeps a
  baseline's note, such as "every mutant runs every test", for each definition whose outcomes ran under it, and the
  report shows it beside the note of this run's baseline. A run with nothing in scope makes no key and uses no cache.
- **Progress.** At the end of a run that prints its report, the cache keeps the outcomes that the run reported, by
  place, as the last run. A killed run does not change it, and a `-d` or `-s` run sets it to what that run reported. The
  console and the page say how many gaps closed since the last run, and mark a gap as new or still open when the last
  run reported its place. An edit thus keeps the marks of the definitions it does not touch, also after `--fresh`. A gap
  whose place the last run did not report, as after a stopped run, a `-d` run or an edit to its definition, gets no
  mark, and the run counts it as "not run then".

## Options

### Start each run from nothing (rejected)

- Good, because a run can never use an outcome that no longer holds.
- Bad, because a stopped run loses its work, and a run after a new test repeats every caught mutant.

### Reuse an outcome only when the sources, the runner and the options did not change (chosen)

- Good, because a reused outcome is the outcome a new run would give, since nothing that decides it changed.
- Good, because a stopped run continues where it stopped.
- Good, because one file, written again after each mutant, is simple, and a killed run keeps all it finished. On
  `cljs-patrol.groups.reagent`, 231 writes of the full 108 KB file take less than 0.5 s; a run writes a file that grows
  from empty, so it pays about half of that. An append-only log was tried, and its fold and recovery added more code
  than this cost justifies.
- Bad, because a change to any source file makes every mutant run again, also the mutants that the change does not
  touch.

### Reuse caught outcomes after a change, by default (rejected)

- Good, because a run after a new test is fast.
- Bad, because a test that the author removed or weakened can make a caught mutant a gap again, and a run with this rule
  does not show it. This rule is the opt-in `--iterate`, for the work of adding tests.

### Reuse a caught outcome while the test that caught it did not change (deferred)

- Good, because it keeps more outcomes than the chosen option after a test change, and does not trust a weakened test.
  Stryker's incremental mode works this way.
- Bad, because the key must then follow each test and each source file that a test reads, which the runner does not
  record today.

### Keep the cache in `target/` (rejected)

Rejected because `clojure -T:build uber` deletes `target/`, as
[ADR 0007](0007-mutation-test-undo-apply-with-a-kept-copy.md) found for the kept copy of `--apply`.

## Consequences

- On `cljs-patrol.groups.reagent`, runs in a row on one machine:

  | Run                                        | Time  | Result                                                                               |
  | ------------------------------------------ | ----- | ------------------------------------------------------------------------------------ |
  | First run, killed with `kill -9` after 4 s | 4.0 s | 82 outcomes in the cache                                                             |
  | Resumed runs, the looping mutant last      | 5.1 s | 6th retry: 210 caught, 21 survived, score 91%                                        |
  | Run that reuses every outcome              | 2.4 s | Runs no tests; the same counts                                                       |
  | `--iterate` after a skip marker in a defn  | 2.9 s | Reuses 208 caught outcomes; runs 22 mutants: the 2 of that defn and the 20 survivors |

  The resumed run gives the same counts as a first run that does not stop. Whether the mutant at 302:59 times out
  depends on the order of the tests. Here it looped on five retries; each time it ran last, so the run stopped with 230
  outcomes kept.

- Hashing the 313 files of the key takes 109 ms on a first run and 48 ms after it.
- A run with `--iterate` can report a mutant as caught though the test that caught it changed. The run says how many
  outcomes it reused, so the author knows to run once without `--iterate` before a merge.
- A cached plain timeout is reused while the key holds. A timeout depends on the speed of the machine, so a run on a
  slower or faster machine with the same sources can disagree on it.
- The JVM is not in the key. A run on another JVM version with the same sources reuses the outcomes.
- The cache is one more directory, `.mutate-cache/`, with one file for each target.
- Two runs on one target at the same time each write the whole file, and the last write wins, so the outcomes of the
  other run are lost. Run one runner at a time on a target.

## Confirmation

- The runner's tests pass, also those that show a reused outcome, a resumed run, a mutant that stopped the run running
  last, an `--iterate` run that runs again only the edited definition, a plain run that checks the outcomes `--iterate`
  trusted, a key that does not match, a failing baseline that keeps the cache, a cache that cannot be written, a run of
  one definition that becomes the last run, and progress marks kept after an edit.
- On `cljs-patrol.groups.reagent`, a cached run and a `--fresh` run on the same sources report the same outcome for each
  mutant that was caught or survived.

## Revisit when

- The runner moves to a project of its own (Patrol Backlog #85). Then the place of the cache must follow that project's
  working directory and ignore rules.
- A cached run reports a caught or survived mutant that a `--fresh` run on the same sources reports differently. Then
  the key misses something that decides an outcome.
- Hashing the sources takes more than 1 s on this repository. Then the key needs a cheaper form, such as file times.

## More Information

- The code: `cljs-patrol.dev.mutate.cache` in [`dev/cljs_patrol/dev/mutate/`](../../dev/cljs_patrol/dev/mutate/)
- How to use it: the "Run again" section of [`docs/mutation-testing.md`](../mutation-testing.md)
- The pull request: [olecve/cljs-patrol#175](https://github.com/olecve/cljs-patrol/pull/175)
- cargo-mutants.
  [Iterating on missed mutants](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/iterate.md), at
  v27.1.0.
- Stryker. [Incremental](https://github.com/stryker-mutator/stryker-js/blob/v9.0.0/docs/incremental.md), at v9.0.0.
