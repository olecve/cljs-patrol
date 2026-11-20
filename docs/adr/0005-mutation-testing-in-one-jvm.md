# ADR 0005: Test The Tests By Mutation In One JVM

## Status

Proposed, 2026-11-20

## Outcome

2026-11-20: the runner now runs only the tests that reach a mutant. A first run records which tests call each function
of the target namespace, also from worker threads. Each mutant runs those tests. A function that no test calls gets a
fifth outcome, no coverage, and the report names it once. The report gives two scores: killed over killed, survived and
not covered, and killed over killed and survived. The whole focus-ring namespace now takes about 18 s, not 24 s. The
sections below keep the proposal as it was.

## Context

Each branch that a change adds must have a test that fails when the branch breaks. A test that stays green when its code
is wrong gives false confidence, and a review cannot see it: the code and the test both look correct.

Until now, the author checked this by hand. The author broke each new branch, ran the tests, and put the branch back. A
script did the same steps and started one JVM for each broken version. On `focus_ring.clj` the script took about 25
minutes for 38 changes, so it ran only on the branches that the author chose. The author can miss a branch, and the
manual check of the `remounting-branch` rule (pull request 163) missed three branches that no test pinned.

Mutation testing does this check by machine. A tool changes the code under test, one small change at a time, and runs
the tests on each version. Each changed version is a mutant. When a test fails, the mutant is killed. When all tests
pass, the mutant survived, and its change shows a place that no test checks. The tests do not change.

No Clojure tool fit in November 2026:

- `jstepien/mutant` is archived, and its fork is not proven.
- `cljest` runs only as a Leiningen plugin, and this project uses `deps.edn`. Its design is close to this one, and its
  selection of the tests that reach a mutant is a planned follow-up.
- `clj-mutate` starts one JVM for each mutant, which costs 30 to 60 s for each mutant.
- `heretic` ran for more than 10 minutes with no output in a trial.
- `apophis` runs in one JVM, but its reader rejects files that hold `#(…)`.
- No maintained tool connects PIT, the Java mutation tool, to Clojure.

## Decision

A dev-only runner in this repository mutates the code under test in one warm JVM. It runs as
`clojure -M:test:mutate TARGET-NS -t TEST-NS`.

- **One form for each mutant.** The runner parses the target namespace with rewrite-clj. Each mutant swaps one operator
  in one `def`, `defn`, `defn-` or `defmethod`, such as `when` for `when-not`, `<` for `<=`, or `true` for `false`. The
  runner evaluates the changed form over the namespace, runs the test namespaces, and evaluates the original form back.
  Clojure calls a function through its var, so the tests run the changed code.
- **Stop at the first failure.** One failing test is enough to kill a mutant. The runner records the failure before it
  ends the test run, so an assertion that catches exceptions, such as `thrown?`, cannot hide it.
- **Four outcomes.** A mutant is killed, survived, unviable (it cannot be read or compiled), or timed out. The score is
  the killed mutants divided by the killed and survived mutants. The other two outcomes are listed but not scored.
- **No mutants that prove nothing.** The runner does not change `defmacro`, `defmulti`, `defonce`, `defprotocol`,
  `defrecord` and `deftype`. Clojure keeps these forms from the first load, or other code captures them then, so a new
  version does not take effect. The runner also does not change a top-level form that is no definition, because its side
  effects would run again with each mutant.
- **A report of what was changed and what was not.** The report has one row for each definition, with its outcome
  counts. It names the definitions that were not changed, and the reason. It lists each survivor with its source line.
  `-r FILE` writes all of it to EDN.
- **Only the changes of a branch.** `-s REF` limits the run to the lines that the branch changed since it left `REF`.
- **A limit on time.** The timeout of a test run is 1.5 times the first run plus 2 s. When a timed-out run continues to
  loop, the runner stops, because that thread would disturb each later run.

## Options

### Break each branch by hand (rejected)

- Good, because it needs no tool.
- Bad, because it takes about 40 s for each change, so it runs only on the branches that the author chooses.
- Bad, because the author can miss a branch. Pull request 163 shows this.

### Use an existing Clojure tool (rejected)

Rejected because no tool fits. The Context lists each one and the reason.

### One JVM for each mutant (rejected)

- Good, because each mutant starts from a clean state.
- Bad, because a JVM with Clojure and the test namespaces takes 30 to 60 s to start, so a namespace with 170 mutants
  takes hours.

### Compile all mutants in at once, chosen at run time (rejected)

Stryker for JavaScript and Mull for C++ use this method, called mutant schemata. They build once, and a switch picks the
mutant for each run.

- Good, because the code is compiled only once.
- Bad, because Clojure can change a function in a running JVM by evaluating its form again. That costs a few
  milliseconds, so the switch gives no gain and adds a code transform.

### Load the whole file again for each mutant (rejected)

- Good, because one step covers each kind of form.
- Bad, because `defmulti` and `defonce` keep their first value, so their mutants survive though a test checks them.
- Bad, because a new `defprotocol` or `defrecord` creates new classes, and the code that other namespaces compiled
  against the old classes breaks for the rest of the run.

### Change one form for each mutant in one JVM (chosen)

- Good, because a mutant costs one form evaluation and one test run. The whole focus-ring namespace, 171 mutants, takes
  24 s.
- Good, because `-s main` limits a run to the lines that a pull request changed.
- Good, because the report shows what was not changed and why, so a definition with no tested branch cannot hide.
- Bad, because the forms that Clojure loads only once are not changed, so their branches stay unchecked.
- Bad, because a value that a `def` computes when the namespace loads keeps its original value. A mutant that only this
  value reaches survives.
- Bad, because the runner stops at the first mutant whose test run loops, so the definitions after it need a run with
  `-d`.

## Consequences

- A pull request can show, in seconds, which new branches have no test. The survivors of the first runs were test gaps,
  not bugs in the rules: 3 in `remounting-branch`, 13 in `focus-ring`, and 4 in the runner itself.
- A survivor is not always a gap. An equivalent mutant, such as `first` for `last` on a list of one item, cannot be
  killed. The author must read each survivor.
- The runner is code in `dev/`, so it has tests, and CI checks it with cljfmt, clj-kondo and the own-source patrol.
- The runner uses only five helpers of `cljs-patrol.parser`: `sym-name`, `declared-name-loc`, `position`,
  `position-span` and `written-child-locs`.

## Confirmation

- `clojure -M:test -n cljs-patrol.dev.mutate-test` passes.
- A run on `cljs-patrol.groups.focus-ring` gives the same survivors twice in a row.
- A run with `-s main` on a branch that changes one definition changes only the lines of that definition.

## Revisit when

- The runner is used outside this repository. Then move it to a project of its own, with copies of the five parser
  helpers.
- The runner runs only the tests that reach a mutated definition, as cljest, PIT and Stryker do. Then update the
  Decision and the report sections of this record.
- Many survivors are functions that no test calls. Then add a mutant that replaces a whole function body with `nil`, as
  cargo-mutants does for Rust.
- Clojure starts to evaluate `defmulti` or `defonce` again on a new load.

## More Information

- The runner: [`dev/cljs_patrol/dev/mutate.clj`](../../dev/cljs_patrol/dev/mutate.clj)
- Its tests: [`test/cljs_patrol/dev/mutate_test.clj`](../../test/cljs_patrol/dev/mutate_test.clj)
- How to run it: the "Mutation testing" section of [`CLAUDE.md`](../../CLAUDE.md)
- The pull request that adds it: [olecve/cljs-patrol#164](https://github.com/olecve/cljs-patrol/pull/164)
- The pull request whose manual check missed three branches:
  [olecve/cljs-patrol#163](https://github.com/olecve/cljs-patrol/pull/163)
