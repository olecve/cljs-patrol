# Mutation testing

[← back to the README](../README.md)

A dev-only runner that checks whether the tests would notice a bug: it plants one small bug at a time in the code under
test, such as `when` to `when-not`, `<` to `<=`, `0` to `1` or a whole function body to `nil`, and runs the tests. A
planted bug that every test lets through marks a line no test really checks.

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

The run of one definition ends with:

```text
Survived:
  490:43  remove -> filter  in holds-focus?
    - (boolean (if-let [bodies (seq (remove #(= :uneval (z/tag %)) (mapping-body-locs loc ns-info)))]
    + (boolean (if-let [bodies (seq (filter #(= :uneval (z/tag %)) (mapping-body-locs loc ns-info)))]
  490:52  = -> not=  in holds-focus?
    + (boolean (if-let [bodies (seq (remove #(not= :uneval (z/tag %)) (mapping-body-locs loc ns-info)))]
  491:38  last -> first  in holds-focus?
    - (holds-focus? (last bodies) ns-info)
    + (holds-focus? (first bodies) ns-info)
  493:52  some -> every?  in holds-focus?
    - (and (not= [loc] results) (some #(holds-focus? % ns-info) results)))))))))
    + (and (not= [loc] results) (every? #(holds-focus? % ns-info) results)))))))))

To watch a new test fail on a survivor, apply it to the file, such as:
  clojure -M:test:mutate cljs-patrol.groups.reagent --apply '490:43:remove'

13 mutants: 9 killed, 4 survived, 0 no coverage, 0 unviable, 0 timed out.
Score 69%, covered score 69%.
```

The console lists each survivor by line and column, the swap, and its function. For the first 10 lines with survivors,
it shows the line before the swaps (`-`) once, and the line after each swap (`+`) under that swap, cut around the swap
when the line is long. For more, read the page that `--html FILE` writes.

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

Each function also gets one planted bug that replaces its whole body with `nil`, one for each arity, shown as `body` to
`nil`. It finds a function that has no operator to swap, or one that a test calls but never checks: "Likely: no test
checks what this function returns or does." A function that runs only for its side effects, such as printing, gives this
gap when no test checks those effects. A `{:pre … :post …}` map at the start of a body stays, and only the forms after
it become `nil`. A body that is empty or only `nil` gets no such planted bug. With `-s`, the body's planted bug runs
when any line of the body changed.

Below the gaps are tables by function and by test, then, folded, the source with every planted bug marked, all the
numbers, and the terms and sources.

## Watch a new test fail

A test that closes a gap must fail with the planted bug in place. `--apply` writes one planted bug into the file, at the
place the console and the page show, and runs nothing. Run your new test, watch it fail, then put your code back with
`--restore`:

```bash
clojure -M:test:mutate cljs-patrol.groups.reagent --apply '87:4:and'
# Applied 87:4  and -> or  in key-meta-map? to src/cljs_patrol/groups/reagent.clj.
# Run the tests and watch the new test fail, then put the code back with:
#   clojure -M:test:mutate --restore
```

The position names the operator too, so a position from an older report cannot write a different planted bug, and in
`(defn width [] 80)` the body and the `80` share a place but `'2:16:body'` and `'2:16:80'` name one each. `--apply`
keeps the file's text in `.mutate-applied.edn`, so it works on a file with uncommitted changes, and `--restore` writes
that text back exactly. A second `--apply`, or a run, waits for `--restore`. `--restore` leaves a file alone that
changed after the apply, and only clears the kept text when you already put the file back by hand. Each gap the tests
ran has its command on the page; code no test calls has none, since no test can fail on it yet.

## Run again

Each run writes the outcomes to `.mutate-cache/<target-ns>.edn` after each planted bug. The next run of the same code
reuses them:

- **Resume.** A run that stopped early, or was killed, goes on where it ended. A planted bug whose tests kept running
  runs again last, after every other one: if it loops again, the run stops there with nothing else lost. A function
  whose tests keep running without any planted bug stops every run at the same place, until you fix those tests. `-d`
  and `-s` runs share the cache with a full run.
- **No change, no tests.** When every planted bug in scope is cached, the run reads the cache and runs no test. A plain
  timeout is reused too; run with `--fresh` to give it another try.
- **`--iterate`.** After you add tests or change code, run with `--iterate`: it keeps the caught planted bugs, and those
  that did not compile, of each function whose text did not change, and runs the others, such as the gaps, the timeouts
  and every planted bug of a function you edited. It says how many it kept. It trusts that those are still caught, so
  run once without it before a merge: that run checks them again.
- **`--fresh`.** Runs every planted bug again and replaces the cache. It keeps the last run for the progress marks.

Run one runner at a time on a target: each run writes the whole cache, so the last write wins.

A plain run reuses an outcome only when nothing changed in `src/`, `test/`, `dev/`, `resources/`, `test-projects/`,
`docs/`, `deps.edn`, `README.md` or the classpath directories of the project, in the `-t` namespaces, in the runner, or
in `-a` and `--timeout`. A file that git ignores, such as `.DS_Store`, does not count. A run that reports no outcome,
such as one whose tests fail before any mutation, leaves the cache as it was. A cache that cannot be read is not used,
and the run's first outcome replaces it. Each run tags each gap that the last run that printed its report also had as
new or still open, on the console and the page, also after you edit other functions, and counts the gaps closed since:

```text
Since the last run: 1 gap closed, 0 new, 3 still open.
  Closed: holds-focus?/490:43/remove->filter
```

The name of each planted bug, its `:id` in the cache and in the `-r` report, is its function, its line and column, and
its swap, such as `holds-focus?/490:43/remove->filter`. The cache also keeps a separate `:place`: a hash of the
function's text with the line and column inside it, which `--iterate` and the progress marks use.

## Skip a harmless planted bug

When a planted bug behaves the same as the original in every case, no test can catch it. Mark it once with the operator
and the reason, and the runner leaves it out of later runs and of the score. The console, the EDN report and the page
list it as ignored, with the reason.

```clojure
(defn pick [xs] (first xs)) ; mutate: skip first: xs holds one item

(defn both [x]
  ; mutate: skip when: x is never nil here
  (when x :one))

(defn ^{:mutate/skip "wraps a JDK call"} wrapped [x] (if x 1 2))

(defn also-wrapped
  {:mutate/skip "wraps a JDK call"}
  [x]
  (if x 1 2))
```

A changed number is named by its text, such as `; mutate: skip 0: zero is the floor`, and a body replaced by `nil` by
`body`, on the line where the body's first form starts, such as `; mutate: skip body: only logs`. A comment after code
skips that operator on its own line, and a comment on a line of its own skips it on the next line of code in the same
form. Other planted bugs on the same line still run. `:mutate/skip` on a definition's name, or in a `defn`'s attribute
map, skips all of its planted bugs, and its value must be the reason, as a string. Only real comments count, not the
same words in a string or docstring. A comment that starts like a marker but does not follow the pattern, such as
`; mutate:skip first: ...`, stops the run, and so does `:mutate/skip` on a form the runner does not mutate, such as a
`defmacro`. The run warns about a marker that matches no planted bug or has no code after it in its form, so stale
markers do not pile up. Skip only what you have read and judged harmless: a skipped line is a line no test checks.

## Why it works this way

Each choice below has a record in [`docs/adr/`](adr/README.md) with the options it beat and when to look at it again.

- **One warm JVM, one form at a time** ([ADR 0005](adr/0005-mutation-testing-in-one-jvm.md)). Evaluating one changed
  form costs milliseconds, where a new JVM for each mutant costs 30 to 60 s. Only the tests that call a function run for
  its mutants.
- **Operators and the numbers of function code, never names**
  ([ADR 0006](adr/0006-mutation-test-which-code-the-runner-changes.md)). A number in data would run every test for a
  change no test checks on purpose. A changed name or `defmethod` dispatch value would leave a var or a method behind,
  and a `^:const` value is copied into its callers, so a changed value takes no effect. A harmless mutant is skipped
  with a reason, which stays next to the code.
- **`--apply` keeps a copy, not git** ([ADR 0007](adr/0007-mutation-test-undo-apply-with-a-kept-copy.md)). It works on
  code that is not committed yet, which is when you write the new test, and `--restore` never overwrites a later edit.
- **A page in plain words** ([ADR 0008](adr/0008-mutation-test-report-in-plain-words.md)). The Stryker viewer showed
  each mutant but not what to do. The page leads with the gaps and the next step, and uses the same score as the
  console.
- **A body replaced by `nil`** ([ADR 0009](adr/0009-mutation-test-replace-a-body-with-nil.md)). A function with no
  operator and no number gives no swap, so one more mutant per arity shows a function that no test checks at all.
- **A cache that only trusts unchanged sources** ([ADR 0010](adr/0010-mutation-test-cache-and-resume.md)). A run reuses
  an outcome only when the source files, the runner and the options are the same as in the last run, so a reused outcome
  is the one a new run would give. `--iterate` trusts caught outcomes after a change, for the work of adding tests.

## Words

The HTML page is written for a reader who has not met mutation testing before, so it uses plain words. The console
report and the EDN file use the usual words, which also name the outcomes in the code.

| Page            | Console and EDN                 | Other tools                  | Meaning                                                                                                                                                     |
| --------------- | ------------------------------- | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| planted bug     | mutant                          | mutant                       | One small change to the code under test, such as `when` to `when-not`. The tests never change.                                                              |
| body to nil     | body mutant (`:swap :body`)     | replace function body        | One planted bug for each arity of a function: its whole body becomes `nil`. `--apply` and skip markers name it `body`.                                      |
| caught          | killed (`:killed`)              | killed, detected             | A test failed, or the changed code threw while it loaded.                                                                                                   |
| not caught      | survived (`:survived`)          | survived, undetected, missed | Every test that ran passed.                                                                                                                                 |
| never run       | no coverage (`:no-coverage`)    | no coverage, NoCoverage      | No test calls the changed function, so no test could catch the change.                                                                                      |
| test gap        | survivor or no coverage         | survivor, undetected mutant  | A planted bug that is not caught or never run: a real bug of that shape could hide there.                                                                   |
| may be harmless | (none)                          | equivalent mutant            | A change that may behave the same as the original in every case, so no test can catch it.                                                                   |
| timed out       | timed out (`:timeout`)          | timeout                      | The tests ran past the time limit. Left out of the score. Stryker counts a timeout as detected; we do not.                                                  |
| did not compile | unviable (`:unviable`)          | unviable, compile error      | The changed code could not be read or compiled. Left out of the score.                                                                                      |
| ignored         | ignored (`:ignored`)            | ignored                      | Left out with a `; mutate: skip` comment or `:mutate/skip` metadata, with a reason. Not run, and left out of the score.                                     |
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
- The runner: [`dev/cljs_patrol/dev/mutate.clj`](../dev/cljs_patrol/dev/mutate.clj), its command line, with one
  namespace per job under [`dev/cljs_patrol/dev/mutate/`](../dev/cljs_patrol/dev/mutate/), and its page:
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
