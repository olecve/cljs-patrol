# Mutation testing words

The words the mutation runner and its HTML page use, how they map to the words of other tools, and the sources behind
the page's advice. How to run the runner is in the "Mutation testing" section of [`CLAUDE.md`](../CLAUDE.md), and why it
works as it does is in [ADR 0005](adr/0005-mutation-testing-in-one-jvm.md).

## Words

The HTML page is written for a reader who has not met mutation testing before, so it uses plain words. The console
report and the EDN file use the usual words, which also name the outcomes in the code.

| Page            | Console and EDN              | Other tools                  | Meaning                                                                                                    |
| --------------- | ---------------------------- | ---------------------------- | ---------------------------------------------------------------------------------------------------------- |
| planted bug     | mutant                       | mutant                       | One small change to the code under test, such as `when` to `when-not`. The tests never change.             |
| caught          | killed (`:killed`)           | killed, detected             | A test failed, or the changed code threw while it loaded.                                                  |
| not caught      | survived (`:survived`)       | survived, undetected, missed | Every test that ran passed.                                                                                |
| never run       | no coverage (`:no-coverage`) | no coverage, NoCoverage      | No test calls the changed function, so no test could catch the change.                                     |
| test gap        | survivor or no coverage      | survivor, undetected mutant  | A planted bug that is not caught or never run: a real bug of that shape could hide there.                  |
| may be harmless | (none)                       | equivalent mutant            | A change that may behave the same as the original in every case, so no test can catch it.                  |
| timed out       | timed out (`:timeout`)       | timeout                      | The tests ran past the time limit. Left out of the score. Stryker counts a timeout as detected; we do not. |
| did not compile | unviable (`:unviable`)       | unviable, compile error      | The changed code could not be read or compiled. Left out of the score.                                     |
| score           | score                        | mutation score               | Caught, divided by caught, not caught and never run.                                                       |
| covered score   | covered score                | covered score, test strength | Caught, divided by caught and not caught: how well the tests check the code they run.                      |
| reaches         | covered by (`:covered-by`)   | covered by                   | A test runs for the changed function, from the first run that records calls.                               |
| closest test    | killed by (`:killed-by`)     | killed by                    | The test that caught the most other planted bugs in the same function, among the tests that reach the gap. |

A run stops each planted bug at the first failing test, so only that test is recorded as the one that caught it. A test
that would also fail later gets no credit. The page's "By test" table therefore counts the planted bugs a test reaches,
and the gaps it reaches and still passes, not the planted bugs it caught.

## Reading a gap

Each gap on the page shows its line now and with the planted bug, a likely reason no test caught it, and the tests that
run for its function. The reason is a guess from the two operators, so the page starts it with "Likely". For example,
`first` to `last` that goes unnoticed most often means every test has only one item there. When that is the only
possible case, the change is harmless, and the page tags it "May be harmless".

To close a gap, test the behavior the code is for, not the planted bug itself. A test written only to catch one swap
tends to break on every refactor. Several gaps in one function usually point to one missing case.

## More Information

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
  usual names for each outcome and score, used in the "Other tools" column.
- cargo-mutants. [Using the results](https://mutants.rs/using-results.html). Test the behavior, not the mutant.
