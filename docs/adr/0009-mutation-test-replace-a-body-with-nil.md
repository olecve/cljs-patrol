# ADR 0009: Replace A Function Body With Nil As A Mutant

## Status

Accepted, 2026-11-21 (proposed 2026-11-21)

## Context

[ADR 0005](0005-mutation-testing-in-one-jvm.md) gives each mutant one operator swap, and
[ADR 0006](0006-mutation-test-which-code-the-runner-changes.md) adds the integer literals of function code. A swap needs
an operator or a number. A function that has none, or whose result no test checks, gives no survivor, so its gap does
not show. ADR 0005 named this case in Revisit when: "Many survivors are functions that no test calls. Then add a mutant
that replaces a whole function body with `nil`, as cargo-mutants does for Rust." Patrol Backlog #82 asked for it.

## Decision

The runner adds one mutant for each arity of a function: it replaces the body of that arity with `nil`.

- **Which functions.** Each arity of a `defn`, `defn-` or `defmethod`, and of a `def` whose value is a `fn`. One mutant
  for each arity, because an arity that no test calls is the gap this mutant finds.
- **Which part.** The forms from the first body form to the last. A leading condition map, `{:pre … :post …}`, stays
  when more forms follow it, so the mutant changes the result, not the checks.
- **No mutant.** The name, docstring, attribute map, dispatch value and type hints never change. A `^:const` def or a
  `def` that holds data gets none, as ADR 0006 decides. A `#(…)` gets none, because `#(nil)` calls `nil`. An empty body,
  or a body that is only `nil`, gets none.
- **How it shows.** The operator is `body`, at the line and column of the first body form, so `--apply 'ROW:COL:body'`
  and `; mutate: skip body: REASON` work there. `--apply` picks a mutant by its place and its operator together, so a
  body mutant and a swap that start at the same place stay apart. `-s` selects a body mutant when any line of the body
  changed. The page gives the hint "Likely: no test checks what this function returns or does."

## Options

### Swaps only (rejected)

- Good, because each mutant changes one token, which is easy to read in the report.
- Bad, because a function with no operator and no number gives no mutant, so no run shows whether a test checks it.

### Replace a whole function body with nil (chosen)

- Good, because one mutant shows a function or an arity that no test checks at all.
- Bad, because each arity adds one mutant. On `cljs-patrol.groups.focus-ring`, the mutants went from 176 to 231, and the
  run from 30.0 s to 34.4 s.
- Bad, because a function that a `def` calls only when the namespace loads gives a survivor that no test can catch. In
  focus-ring, `make-group` is such a function.

## Consequences

- On `cljs-patrol.groups.focus-ring`, 54 of 55 body mutants are caught. On `cljs-patrol.groups.reagent`, 48 of 51 are
  caught, and the survivors show two real gaps: no test has a body that derefs (`reactive-deref?`, `first-in-subtree`),
  and no test checks `:kw` on two findings (`head-identifier`).
- A mutant now has an end, `:end-row` and `:end-col`, and `--apply` replaces the range from its place to its end. For a
  swap, that range is one token, as [ADR 0007](0007-mutation-test-undo-apply-with-a-kept-copy.md) describes.

## Confirmation

- The runner's tests pass, also those that show a condition map kept, a mutant picked by place and operator, and a body
  mutant selected by a changed line inside the body.

## Revisit when

- A run on `cljs-patrol.groups.focus-ring` or `cljs-patrol.groups.reagent` shows that more than half of the body
  survivors are functions called only when a namespace loads. Then such functions need to be left out.
- The run time of focus-ring grows past 1.5 times its time without body mutants (45 s against 30.0 s).

## More Information

- The code: `body-mutants` in [`dev/cljs_patrol/dev/mutate/forms.clj`](../../dev/cljs_patrol/dev/mutate/forms.clj)
- How it shows: the words table of [`docs/mutation-testing.md`](../mutation-testing.md)
- The pull request: [olecve/cljs-patrol#174](https://github.com/olecve/cljs-patrol/pull/174)
- cargo-mutants. [Generated mutants](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/mutants.md), at
  v27.1.0.
