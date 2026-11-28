# Mutation testing

[← back to the README](../README.md)

Every branch a change adds needs a test that fails when the branch breaks. A test that stays green when its code is
wrong gives false confidence, and a review cannot see it. The mutation runner checks this: it plants one small bug at a
time in the code under test, such as `when` to `when-not` or `<` to `<=`, and runs the tests that call that code. A
planted bug that every test lets through marks a line no test really checks.

The runner is [mutacio-clj](https://github.com/olecve/mutacio-clj). It started in this repository, and
[ADR 0016](adr/0016-extract-the-mutation-runner.md) records why it moved. The `:mutate` alias in `deps.edn` gets it as a
git dependency.

## Run it

```bash
# the lines your branch changed, with a page to read
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test -s main --html reagent.html

# one definition
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test -d holds-focus?

# the whole namespace
clojure -M:test:mutate cljs-patrol.groups.reagent -t cljs-patrol.groups.reagent-test
```

Pass every test namespace that tests the target with `-t`. The run exits 0 when every planted bug was caught, 1 when
there is a test gap, and 2 when it cannot start or stops early.

## Read more

- [The guide](https://github.com/olecve/mutacio-clj/blob/v0.1.1/docs/mutation-testing.md): why, how to run it, how to
  read the report, and how to watch a new test fail with `--apply`.
- [The reference](https://github.com/olecve/mutacio-clj/blob/v0.1.1/docs/reference.md): every option, outcome and file.
- The decisions behind it: ADRs [0005](adr/0005-mutation-testing-in-one-jvm.md) to
  [0012](adr/0012-mutation-test-guard-skips-what-git-ignores.md) in this repository.
