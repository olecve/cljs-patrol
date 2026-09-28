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

Every issue is identified by content, not by position:

- **Re-frame** issues are keyed by their fully-qualified keyword (e.g. `:app.subs/users`), so baselines survive file
  moves and renames without regeneration — a real advantage over line-based baselines in JS/TS tools.
- **Spade** issues are keyed by ns + var (plus selector, where relevant), so renaming a class within its ns is the only
  change that shifts identity.
- **Hiccup a11y** issues are keyed by file + tag + whitespace-collapsed source snippet, so `cljfmt` or manual
  reformatting never rewrites the baseline. A truly new offending vector (with different source text) still counts as
  new.

Note: bumping from an older baseline (`:version 1`) requires re-running `--baseline-write` once. The tool reports a
clear error otherwise.

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
