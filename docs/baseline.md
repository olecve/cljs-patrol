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

Every issue but two is identified by what it is, never by its line: a re-frame issue by its fully-qualified keyword; a
style, typography or docstring issue by its namespace and var; a test name by the var it names; and a Hiccup, test-code
or layout issue by its file and its source text with whitespace collapsed. Moving code, renaming a file that holds a
registration, or running `cljfmt` leaves the baseline as it is, while an offending form with different source text still
counts as new. The two exceptions are a deprecated effect and a dynamic dispatch or subscribe site, identified by their
file and line: a line added above one makes it count as new.

A baseline written by an older release can carry an older format version. The tool then stops with an error asking you
to run `--baseline-write --allow-new` once.

## Rewriting the baseline

Once a baseline exists, `--baseline-write` only shrinks it. A write that would add a finding the baseline lacks is
refused: the file is left as it was, the findings are listed, and the run exits `1`.

```bash
clojure -M:run --baseline-write src/cljs/myapp   # a finding was fixed: written, the baseline loses its entry
clojure -M:run --baseline-write src/cljs/myapp   # a finding was added: refused, nothing written
```

```
=== Missing accessible name (1) ===
  [NEW]   src/cljs/myapp/settings.cljs:12  [:textarea {:on-change on-change}]

Refusing to write src/cljs/myapp/.cljs-patrol/baseline.edn: the [NEW] findings above are not in it. Fix them, or pass --allow-new to add them.
```

Pass `--allow-new` when the additions are meant, as after upgrading cljs-patrol, changing `.cljs-patrol/config.edn`, or
moving a deprecated effect or dynamic site to another line:

```bash
clojure -M:run --baseline-write --allow-new src/cljs/myapp
```

<details>
<summary>Why a write is refused</summary>

`--baseline` blocks a new finding; rewriting the baseline is how a finding stops counting as new. Without the guard,
fixing a red CI run and accepting the bug that turned it red are the same command. With it, accepting is a separate flag
that shows up in the command and in review.

The first write, with no baseline file yet, needs no flag. A baseline that cannot be compared, an older format version
or a file that does not parse, stops the write with the read error unless `--allow-new` is passed. `--allow-new` without
`--baseline-write` is an error.

A write records only the groups it runs. Run it with the same `--only` or `--disable` as the `--baseline` check: a
narrower write drops the other groups' entries as if fixed, and a later full write then needs `--allow-new` to put them
back.

</details>

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
