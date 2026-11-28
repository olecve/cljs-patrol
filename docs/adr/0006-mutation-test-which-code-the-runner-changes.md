# ADR 0006: Choose Which Code The Mutation Runner Changes

## Status

Accepted, 2026-11-21 (proposed 2026-11-21)

## Context

[ADR 0005](0005-mutation-testing-in-one-jvm.md) decided that the runner swaps one operator at a time in a `def`, `defn`,
`defn-` or `defmethod`, such as `when` for `when-not`. It also decided not to change the forms that Clojure loads only
once. This record decides two later questions: which other code the runner changes, and which code it must leave alone
so that a mutant proves something.

These facts started the decisions:

- **Boundaries need numbers.** backlog task CP-0052 asked for number mutants, because the first survivors sat on
  boundaries: `>=` against `>`, and `inc` against `dec`, in `without-negations` of `cljs-patrol.groups.focus-ring`.
  Operator swaps never change a number that sets a boundary, such as the `0` in `(subs text 0 start)`.
- **Some survivors are equivalent.** An equivalent mutant behaves the same as the original in every case, so no test can
  catch it. Schuler and Zeller measured that about 45% of the undetected mutants in their study were equivalent, and
  that a person needs about 15 minutes to check one. Without a way to mark them, the same equivalent survivors come back
  in each run. The page code of the runner had 3 of them when pull request 167 added skip markers.
- **Two mutants leave something behind.** Evaluating the original form back does not undo them. Both cases came with ADR
  0005, before number mutants:
  - A changed `defmethod` dispatch value, such as `(defmethod area true …)` to `(defmethod area false …)`, adds a method
    for the new value. The restore puts back the method for the old value, but the new method stays.
  - A changed definition name, such as `(defn first …)` to `(defn last …)`, interns a new var. The restore does not
    remove it.
- **One kind of mutant takes no effect.** Clojure copies the value of a `^:const` def into each caller as the caller
  compiles. When the runner evaluates a changed `^:const` def, the callers keep the old value. Its mutants survive,
  though a test checks the value.
- **Data is expensive to mutate.** A `def` that holds data cannot be recorded, so each of its mutants runs every test
  (ADR 0005, Outcome). A number in data, such as a table entry or a limit, is also rarely checked on purpose.

## Decision

The runner changes the operators of `swaps` and the integer literals of function code. It never changes a definition
name, a `defmethod` dispatch value, or a `^:const` def. A person can leave out a mutant that they read and judged
harmless, and must give a reason.

- **Integer literals in function code.** `0` and `1` swap, and any other integer goes up by one. Only a plain decimal
  literal changes, so the mutant keeps the written form: `0x10`, `+1`, `1N`, `1.5` and `1/2` stay. Function code is the
  body of a `defn`, `defn-` or `defmethod`, or a `def` whose value is a `fn` or `#(…)`. A number in other data stays,
  also in a `fn` inside data, which runs once when the namespace loads.
- **Never a name or a dispatch value.** No operator changes the name slot of a definition or the dispatch value of a
  `defmethod`.
- **Never a `^:const` def.** The report lists it apart, as "Not mutated, ^:const, callers copy its value". `-d` with its
  name, or `:mutate/skip` on it, stops the run.
- **A skip marker with a reason.** `; mutate: skip OPERATOR: REASON` after a line, or on the line above it in the same
  form, leaves that one mutant out. For a number, OPERATOR is the literal, as in `; mutate: skip 0: zero is the floor`.
  `:mutate/skip "REASON"` on a definition's name or in a `defn`'s attribute map leaves out all of its mutants. Skipped
  mutants do not run, are not scored, and are listed as ignored with their reasons.
- **Marker checks.** A marker that is written wrong, or metadata that is no reason string, stops the run. A marker that
  matches no mutant prints a warning: on a full run for the whole file, and with `-d` or `-s` only for the lines in
  scope.

## Options

### Swap operators only (rejected)

- Good, because each operator mutant is a branch or a comparison, which a test can check directly.
- Bad, because the runner never changes a number that sets a boundary, so no run shows whether a test checks it. When
  number mutants were added, `(subs text 0 start)` to `1` in `without-negations` of `cljs-patrol.groups.focus-ring`
  survived: no test checks that the first character stays.

### Change every integer literal (rejected)

- Good, because one rule covers all code.
- Bad, because every number in data becomes a mutant that runs every test.
- Bad, because a number in a dispatch value or a `^:const` def gives a mutant that leaves a method behind or takes no
  effect.

### Change the integer literals of `defn`, `defn-` and `defmethod` only (rejected)

- Good, because it leaves all data out.
- Bad, because a function that a `def` holds, such as `(def f (fn [x] (+ x 1)))`, loses its number mutants, though the
  runner records it as it records a `defn`. The review of pull request 170 found this.

### Change the integer literals inside any `fn` or `#(…)` (rejected)

Rejected because a `fn` inside data, such as `(def table (into {} (map (fn [k] [k 0])) ks))`, runs once when the
namespace loads. Its numbers are data, the `def` cannot be recorded, and each mutant would run every test. The review of
pull request 170 found this.

### Change the integer literals of function code (chosen)

- Good, because each changed number is in code that runs when a test calls it.
- Good, because data stays out, so a run does not run every test for numbers that no test checks on purpose.
- Bad, because a number in function code that sets a delay, such as `(Thread/sleep 100)`, still gives a harmless
  survivor. backlog task CP-0061 plans to leave such calls out.

### Leave harmless survivors in the report (rejected)

Rejected because the same survivors come back in each run, and the author must judge them again each time.

### Skip a whole line with a marker (rejected)

Rejected because a line often holds more than one mutant, and the others must still run. A run with `-d 'holds-focus?'`
on `cljs-patrol.groups.reagent` shows two survivors on line 490, `remove` and `=`.

### Skip one operator, with a reason (chosen)

- Good, because the reason stays next to the code, and the report shows it.
- Good, because a wrong or stale marker does not hide a gap: it stops the run or prints a warning.
- Bad, because each marker is a comment that a person must change when the code changes.

## Consequences

- On `cljs-patrol.groups.reagent`, the runner now plans 180 mutants, and 8 of them are numbers. A run stops at the
  mutant at 302:59, which times out, so only 59 mutants run, against 58 on `main`. The 10 survivors are the same as on
  `main`. Only 1 of the 8 number mutants runs before the stop.
- A run no longer leaves a method or a var behind, so a later mutant does not run against a changed namespace.
- The operators of a `^:const` def are no longer changed. A skip marker in a `^:const` def now matches no mutant, and
  the run warns about it.
- The page tells a number mutant by `:swap :number`, which the runner sets, and gives it its own hint.

## Confirmation

- `clojure -M:test -n cljs-patrol.dev.mutate-test -n cljs-patrol.dev.mutate-html-test` passes.
- The tests show that a dispatch value, a definition name, a `def` value, a `fn` inside data and a `^:const` def get no
  mutant, and that a number in a `def` whose value is a `fn` or `#(…)` gets one.

## Revisit when

- backlog task CP-0061 leaves calls such as `Thread/sleep` out. Then the bad item of the chosen option no longer holds.
- A run on `cljs-patrol.groups.focus-ring` or `cljs-patrol.groups.reagent` shows that more than half of the number
  survivors are equivalent. Then numbers cost more than they find.
- Clojure starts to read a `^:const` value at each call, or to remove a method when a `defmethod` is evaluated again.

## More Information

- The runner:
  [`dev/cljs_patrol/dev/mutate.clj`](https://github.com/olecve/cljs-patrol/blob/c72792b2ccad935207813dbfaec48e953aab07cc/dev/cljs_patrol/dev/mutate.clj)
- How to skip a mutant: the "Skip a harmless planted bug" section of
  [`docs/mutation-testing.md`](https://github.com/olecve/cljs-patrol/blob/c72792b2ccad935207813dbfaec48e953aab07cc/docs/mutation-testing.md)
- Skip markers: [olecve/cljs-patrol#167](https://github.com/olecve/cljs-patrol/pull/167)
- Number mutants, names, dispatch values and `^:const` defs:
  [olecve/cljs-patrol#170](https://github.com/olecve/cljs-patrol/pull/170)
- Schuler, Zeller. (Un-)Covering Equivalent Mutants. ICST, 2010.
  [doi:10.1109/ICST.2010.30](https://doi.org/10.1109/ICST.2010.30)
