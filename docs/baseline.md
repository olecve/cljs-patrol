# Baseline

Adopting cljs-patrol on a codebase that already has findings: snapshot what is there, then block only what is new.

[← back to the README](../README.md)

Existing codebases often have many issues. The baseline feature lets you snapshot current issues so CI only fails on
**new** ones.

## Setup

1. Run analysis and write the baseline:

```bash
clojure -M:run --baseline-write src/cljs/myapp
```

This creates `src/cljs/myapp/.cljs-patrol/baseline.edn` with all current issues. The baseline file is placed inside the
source directory by default. Commit this file.

2. Use `--baseline` in CI:

```bash
clojure -M:run --baseline src/cljs/myapp
```

Exits `0` if every found issue is in the baseline. Exits `1` only on **new** issues.

## Why baselines survive refactors and reformats

Every issue is identified by what it is, never by its line: a re-frame issue by its fully-qualified keyword; a style,
typography or docstring issue by its namespace and var; a test name by the var it names; and a Hiccup, test-code or
layout issue by its file and its source text with whitespace collapsed. Moving code, renaming a file that holds a
registration, or running `cljfmt` leaves the baseline as it is, while an offending form with different source text still
counts as new.

A baseline written by an older release can carry an older format version. The tool then stops with an error asking you
to run `--baseline-write` once.

## Output with baseline

Console output tags each issue as `[NEW]` or `[BASE]`. A summary shows counts:

```
Found 12 issues: 2 new, 8 in baseline, 3 fixed.
3 baseline issues no longer present - consider running --baseline-write to refresh.
```

All output formats work with `--baseline`:

```bash
clojure -M:run --baseline --output edn src/cljs/myapp   # {:new-issues [...] :baseline-issues [...] :fixed-issues [...] :exit-code 0}
clojure -M:run --baseline --output html src/cljs/myapp  # report.html with visual new/baseline distinction
```

## Strict mode

By default, fixed issues (present in baseline but no longer found) don't cause CI failure. Use `--strict-baseline` to
require baseline regeneration when issues disappear:

```bash
clojure -M:run --baseline --strict-baseline src/cljs/myapp
```

This prevents quiet drift where issues get "fixed by ignoring."

## Quiet mode

Suppress baseline issues from output, showing only new ones. Useful for PR comment bots:

```bash
clojure -M:run --baseline --quiet-baseline src/cljs/myapp
```

## Changed-files PR pattern

Combine `--files` with `--baseline` to check only files changed in a PR:

```bash
changed=$(git diff --name-only origin/main...HEAD | grep -E '\.clj[sc]?$' | paste -sd, -)
clojure -M:run --baseline --files "$changed" src/cljs/myapp
```

Note: `--baseline-write` cannot be combined with `--files` (would write a partial baseline).

Baseline settings live in `.cljs-patrol/config.edn` alongside every other setting — see
[Configuration file](../README.md#configuration-file). A CLI flag overrides what the file says.
