# ADR 0010: Keep The Results Of A Mutation Run To Resume And Iterate

## Status

Proposed, 2026-11-21

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
- **The cache.** `.mutate-cache/<target-ns>.edn` in the directory the command runs from, and a log beside it,
  `<target-ns>.log.edn`. The repository's `.gitignore` lists `.mutate-cache/`.
- **The key.** One hash of the path and the bytes of every file under `src/`, `test/`, `dev/`, `resources/`,
  `test-projects/` and `docs/`, and of `deps.edn` and `README.md`. The code on the classpath reads `resources/`. The
  tests read the fixtures in `test-projects/` and the examples in `docs/` and `README.md`. The key also holds the names
  of the `-t` namespaces in name order, the hash of the runner's own source, and the options that change an outcome,
  `-a` and `--timeout`.
- **When an outcome is reused.** A run with the same key reuses every cached outcome and runs only the other mutants.
  When any part of the key differs, the run uses no cached outcome, says which part changed, and replaces the cache.
- **Resume.** The run appends each outcome to the log after each mutant, as one line, so a run that stops or is killed
  keeps every outcome it finished. At its end, the run folds the log into the cache and deletes the log. A run that
  finds a log, left by a killed run, folds it first. The log starts with the cache as that run opened it, so the fold
  uses that run's own key, and the key check then decides what this run reuses. A run that records no outcome does not
  change the cache, so a run whose tests fail before any mutation, or that stops before its first outcome, keeps the
  last run's cache. The next run runs the mutant that stopped the run last, after every other mutant. When that mutant
  times out again, the run stops there and loses nothing. When it does not, it gets its real outcome. This covers a
  mutant whose tests keep running. A definition whose own tests keep running without a mutation still stops each run, as
  ADR 0005 decides, and the run says so. A cache or a log that the run cannot read gives one warning, and the run starts
  over. A key that the run cannot make, or a cache that it cannot write, gives one warning that names what failed, and
  the run continues without the cache.
- **`--iterate`.** For the work of writing tests and fixing gaps: when the sources changed, `--iterate` reuses the
  cached outcomes of the mutants that were caught or did not compile. When nothing changed, it resumes as a plain run
  does. It matches by place, not by id. An edit thus keeps the outcomes of every definition it does not touch, also a
  definition that moved, and runs again each mutant of a definition it touches, also for a skip marker inside it. It
  runs again the mutants that survived, had no coverage or timed out, and the new mutants. It does not keep the test
  plans or the notes of the last run, because the sources changed. It always runs the baseline, also when it reuses
  every outcome, so that each definition has a test plan of the current sources. The `-t` namespaces, the runner's
  source and the options must be the same as in the cache. The run says how many outcomes it reused, and that it trusts
  them. The cache marks these outcomes as trusted, and only another `--iterate` run reuses them: a plain run checks them
  again.
- **`--fresh`.** It ignores the cache and starts over.
- **No work when all is known.** When a run with the same key finds every outcome in the cache, it runs no baseline and
  no test, and uses the test plans and the notes that the cache holds. When the reused outcomes ran under a note, such
  as "every mutant runs every test", the report keeps that note, also beside the note of this run's baseline.
- **Progress.** The cache keeps the outcomes that the last run reported, by place. The console and the page say how many
  gaps closed since the last run, and mark a gap as new or still open when the last run reported its place. An edit thus
  keeps the marks of the definitions it does not touch, also after `--fresh`. A gap whose place the last run did not
  report, as after a stopped run, a `-d` run or an edit to its definition, gets no mark, and the run counts it as "not
  run then".

## Options

### Start each run from nothing (rejected)

- Good, because a run can never use an outcome that no longer holds.
- Bad, because a stopped run loses its work, and a run after a new test repeats every caught mutant.

### Reuse an outcome only when the sources, the runner and the options did not change (chosen)

- Good, because a reused outcome is the outcome a new run would give, since nothing that decides it changed.
- Good, because a stopped run continues where it stopped.
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

  | Run                                        | Time  | Result                                                            |
  | ------------------------------------------ | ----- | ----------------------------------------------------------------- |
  | First run, killed with `kill -9` after 4 s | 4.0 s | 82 outcomes in the log                                            |
  | Resumed run                                | 4.2 s | Reuses 82 outcomes; 210 caught, 21 survived, score 91%            |
  | Run that reuses every outcome              | 2.2 s | Runs no tests; the same counts                                    |
  | Plain run after a comment line at the top  | 4.8 s | Runs every mutant; all 21 gaps marked still open                  |
  | `--iterate` after a skip marker in a defn  | 3.5 s | Reuses 208 caught outcomes; runs the 2 of that defn, 20 survivors |

  The resumed run gives the same counts as a first run that does not stop. Whether the mutant at 302:59 times out still
  depends on the order of the tests. When it times out on its retry, the run stops after every other mutant, with 230
  outcomes kept.

- Hashing the 313 files of the key takes 109 ms on a first run and 48 ms after it.
- A run with `--iterate` can report a mutant as caught though the test that caught it changed. The run says how many
  outcomes it reused, so the author knows to run once without `--iterate` before a merge.
- A cached plain timeout is reused while the key holds. A timeout depends on the speed of the machine, so a run on a
  slower or faster machine with the same sources can disagree on it.
- The JVM is not in the key. A run on another JVM version with the same sources reuses the outcomes.
- The cache is one more directory, `.mutate-cache/`, with one file for each target, and a log while a run is going.

## Confirmation

- The runner's tests pass, also those that show a reused outcome, a resumed run, a mutant that stopped the run running
  last, an `--iterate` run that runs again only the edited definition, a plain run that checks the outcomes `--iterate`
  trusted, a key that does not match, a failing baseline that keeps the cache, a cache that cannot be written, a log
  left by a killed run, and progress marks kept after an edit.
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
- cargo-mutants.
  [Iterating on missed mutants](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/iterate.md), at
  v27.1.0.
- Stryker. [Incremental](https://github.com/stryker-mutator/stryker-js/blob/v9.0.0/docs/incremental.md), at v9.0.0.
