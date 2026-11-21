# Mutation testing

[← back to the README](../README.md)

A dev-only runner that checks whether the tests would notice a bug: it plants one small bug at a time in the code under
test and runs the tests. A planted bug that every test lets through marks a line no test really checks.

## Why

Every branch a change adds needs a test that fails when the branch breaks. A test that stays green when its code is
wrong gives false confidence, and a review cannot see it, because the code and the test both look correct. Breaking each
branch by hand took about 25 minutes for 38 changes and still missed three branches in pull request 163. The runner does
the same check in seconds. [ADR 0005](adr/0005-mutation-testing-in-one-jvm.md) records why it works as it does and why
no existing tool fit.

## Run it

```bash
# the lines your branch changed, with a page to read
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test -s main --html reagent.html

# one definition
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test -d holds-focus?

# the whole namespace
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test
```

```text
58 mutants: 47 killed, 10 survived, 0 no coverage, 0 unviable, 1 timed out.
Score 82%, covered score 82%.
HTML report written to reagent.html
```

Pass every test namespace that tests the target with `-t`. The run exits 0 when every planted bug was caught, 1 when
there is a test gap, and 2 when it cannot start or stops early. Every option, what is not mutated, and when every test
runs instead of the selected ones are in the "Mutation testing" section of [`CLAUDE.md`](../CLAUDE.md#mutation-testing).

## Read the report

The page leads with the result and a to-do list of the functions with test gaps. Each gap shows:

- the forms around the line, the line now, and the line with the planted bug;
- a likely reason no test caught it, such as "Likely: no test checks both sides of this condition.";
- the closest test to extend and the tests that ran for the function.

To close a gap, test the behavior the code is for, not the planted bug itself. A test written only to catch one swap
tends to break on every refactor. Several gaps in one function usually point to one missing case. The reason is a guess
from the two operators, so it starts with "Likely". For example, `first` to `last` that goes unnoticed most often means
every test has only one item there. When that is the only possible case, the change is harmless, and the page tags it
"May be harmless".

Below the gaps are tables by function and by test, then, folded, the source with every planted bug marked, all the
numbers, and the terms and sources.

## Words

The HTML page is written for a reader who has not met mutation testing before, so it uses plain words. The console
report and the EDN file use the usual words, which also name the outcomes in the code.

| Page            | Console and EDN                 | Other tools                  | Meaning                                                                                                                                                     |
| --------------- | ------------------------------- | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| planted bug     | mutant                          | mutant                       | One small change to the code under test, such as `when` to `when-not`. The tests never change.                                                              |
| caught          | killed (`:killed`)              | killed, detected             | A test failed, or the changed code threw while it loaded.                                                                                                   |
| not caught      | survived (`:survived`)          | survived, undetected, missed | Every test that ran passed.                                                                                                                                 |
| never run       | no coverage (`:no-coverage`)    | no coverage, NoCoverage      | No test calls the changed function, so no test could catch the change.                                                                                      |
| test gap        | survivor or no coverage         | survivor, undetected mutant  | A planted bug that is not caught or never run: a real bug of that shape could hide there.                                                                   |
| may be harmless | (none)                          | equivalent mutant            | A change that may behave the same as the original in every case, so no test can catch it.                                                                   |
| timed out       | timed out (`:timeout`)          | timeout                      | The tests ran past the time limit. Left out of the score. Stryker counts a timeout as detected; we do not.                                                  |
| did not compile | unviable (`:unviable`)          | unviable, compile error      | The changed code could not be read or compiled. Left out of the score.                                                                                      |
| score           | score                           | mutation score               | Caught, divided by caught, not caught and never run.                                                                                                        |
| covered score   | covered score                   | covered score, test strength | Caught, divided by caught and not caught: how well the tests check the code they run.                                                                       |
| reaches         | a definition's tests (`:tests`) | covered by                   | The test ran for the planted bug: every test its function ran when none failed, or only the test that caught it, since the run stops there.                 |
| caught by       | killed by (`:killed-by`)        | killed by                    | The test that failed first for one planted bug. A run stops there, so a test that would also fail later is not named. A failure in a fixture names no test. |
| closest test    | (none)                          | (none)                       | On the page only: among the tests a function ran, the one that caught the most of its other planted bugs.                                                   |

A run stops each planted bug at the first failing test, so only that test is recorded as the one that caught it. A test
that would also fail later gets no credit. The page's "By test" table therefore counts the planted bugs a test reaches,
and the gaps it reaches and still passes, not the planted bugs it caught.

## More Information

- [ADR 0005: Test The Tests By Mutation In One JVM](adr/0005-mutation-testing-in-one-jvm.md). The decision and the
  options it was weighed against.
- The runner: [`dev/cljs_patrol/dev/mutate.clj`](../dev/cljs_patrol/dev/mutate.clj), and its page:
  [`dev/cljs_patrol/dev/mutate_html.clj`](../dev/cljs_patrol/dev/mutate_html.clj).
- Petrović, Ivanković, Fraser, Just.
  [Practical Mutation Testing at Scale: A view from Google](https://arxiv.org/abs/2102.11378). IEEE TSE, 2021. Google
  shows at most one surviving mutant per line, and skips "arid" code such as logging, which raised the share of mutants
  developers found useful from about 15% to 89%.
- Petrović, Ivanković, Fraser, Just.
  [Does mutation testing improve testing practices?](https://arxiv.org/abs/2103.07189) ICSE, 2021. Developers who saw
  mutants wrote more and better tests over time.
- Petrović et al.
  [Please fix this mutant: How do developers resolve mutants surfaced during code review?](https://research.google/pubs/please-fix-this-mutant-how-do-developers-resolve-mutants-surfaced-during-code-review/)
  ICSE SEIP, 2023. How developers act on the mutants a review shows them.
- Schuler, Zeller.
  [(Un-)Covering Equivalent Mutants](https://www.st.cs.uni-saarland.de/publications/files/schuler-icst-2010.pdf).
  ICST, 2010. How often a survivor is an equivalent mutant, and what it costs to check one by hand.
- Stryker.
  [Mutant states and metrics](https://stryker-mutator.io/docs/mutation-testing-elements/mutant-states-and-metrics/). The
  usual names for each outcome and score.
- cargo-mutants. [Using the results](https://mutants.rs/using-results.html). Test the behavior, not the mutant.
