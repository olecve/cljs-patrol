# ADR 0008: Show The Mutation Report In Plain Words On A Page Of Its Own

## Status

Accepted, 2026-11-21

## Context

The runner of [ADR 0005](0005-mutation-testing-in-one-jvm.md) prints its report to the console. A run on a whole
namespace prints tens of rows and survivors, and the reader must find the gaps and their code in the source by hand.

The first `--html` report (commit 41a3ee4, pull request 166) used `mutation-testing-elements@3.9.0`, the viewer of
Stryker, loaded from jsDelivr. On the `remounting-branch` run, its reader found these problems:

- The reader said: "I am not 100% sure what it tells me". The viewer shows each mutant on the source, but it does not
  say which gaps to close first, or how.
- The viewer shows many emoji, and has no option to turn them off.
- The viewer counts a timeout as detected, so its score was different from the console score.
- The page needed a network connection to load the viewer.
- The page set the theme of the viewer from the system setting, so the viewer's own theme switch did not decide the
  theme.

Two reviews of the page by agents with UI and UX skills gave the first changes. A study of the reports of other tools
gave more: Stryker, PIT, cargo-mutants, Infection, mutmut, and the mutation testing at Google.

## Decision

The runner writes its own page, which leads with the test gaps and what to do about each. It uses words for a reader who
has not met mutation testing before.

- **The first screen.** One sentence with the score, then a to-do list of the functions with gaps and their lines.
- **One card for each function with gaps.** Each gap shows the forms around its line, the line now and with the planted
  bug, a likely reason no test caught it, the closest test to extend, and the `--apply` command.
- **Plain words.** A mutant is a "planted bug", killed is "caught", and a survivor or a mutant no test reaches is a
  "test gap". `docs/mutation-testing.md` maps each word to the words of the console and of other tools.
- **The same numbers as the console.** Timeouts and mutants that do not compile are listed but not scored.
- **No network.** hiccup builds the page on the JVM, with its style inline and one small script.
- **Sources.** The page and the guide cite the papers and docs behind their advice.

## Options

### Use the Stryker viewer (rejected)

- Good, because it is a known tool with a source view, a test view and filters.
- Bad, because its reader could not tell from it what to do.
- Bad, because its score was different from the console score, and it needed a network connection.

### Build the page in the browser from JSON in the page (rejected)

Rejected because hiccup on the JVM escapes each value, needs no JSON writer, and the runner's tests can check it.

### Build the page with hiccup, in plain words (chosen)

- Good, because the page answers "how good are my tests" and "what do I do now" on the first screen.
- Good, because the runner's tests check the page, and the mutation runner runs on the page code itself.
- Bad, because the page is code to maintain: about 530 lines of Clojure and 490 lines of CSS, where the Stryker viewer
  was maintained by others.

## Consequences

- The page keeps three parts of the Stryker viewer: a table by test, filters on the source, and all the numbers, folded.
- From the study of other tools, a gap shows each planted bug on its own line. When one line has several gaps, the page
  says that one new test will likely catch all of them. The intro says to test the behavior, not the planted bug.
- The page code is a separate namespace, `cljs-patrol.dev.mutate-html`, so the runner loads hiccup only for `--html`.
- The JSON writer of the first version is gone. It wrote symbols and sets as invalid JSON. The report did not hold them,
  but each new field had to be checked for them.

## Confirmation

- `clojure -M:test -n cljs-patrol.dev.mutate-html-test` passes.
- A run with `--html` on `cljs-patrol.groups.reagent` writes a page that opens with no network.

## Revisit when

- The runner covers more than one namespace in a run. Then the page needs a list of files, each with its score, as the
  Stryker viewer has.
- A tool that reads only the Stryker JSON schema, such as the Stryker Dashboard, must show a run. Then an export in that
  schema lets that tool show the run with no new page code.

## More Information

- The page: [`dev/cljs_patrol/dev/mutate_html.clj`](../../dev/cljs_patrol/dev/mutate_html.clj)
- The words and the sources: [`docs/mutation-testing.md`](../mutation-testing.md)
- The pull request: [olecve/cljs-patrol#166](https://github.com/olecve/cljs-patrol/pull/166)
- The first version, with the Stryker viewer:
  [41a3ee4](https://github.com/olecve/cljs-patrol/commit/41a3ee47e0ae4e67f8c6e4e1344270656c4471fa)
- Stryker.
  [Mutant states and metrics](https://github.com/stryker-mutator/mutation-testing-elements/blob/v3.9.0/docs/mutant-states-and-metrics.md),
  at v3.9.0.
- Petrović, Ivanković, Fraser, Just.
  [Practical Mutation Testing at Scale: A view from Google](https://arxiv.org/abs/2102.11378v1). IEEE TSE, 2021.
