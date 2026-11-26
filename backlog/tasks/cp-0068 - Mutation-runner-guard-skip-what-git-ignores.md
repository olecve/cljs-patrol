---
id: CP-0068
title: "Mutation runner guard: skip what git ignores"
status: Done
labels: ["mutation-runner"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/177"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The file guard of ADR 0011 walks every folder under the working directory, ignored ones too, and keeps a hand list of
tool caches and editor files to skip. Most of its cost is walking node_modules/, target/ and .shadow-cljs/, and each new
tool folder can cause a false stop until it joins the list.

### Why

Faster snapshots and no hand list to maintain; the incident's src/ and .hidden/ files are not ignored, so it is still
caught.

### Fix

```clojure
;; once after the baseline
(git ls-files --others --ignored --exclude-standard --directory)
;; the snapshot skips those paths; the hand list stays only as a fallback outside git
```

### Notes

Merged 2026-11-23 as PR 177; ADR 0012 accepted (partially supersedes ADR 0011). The guard asks
`git status --ignored=matching` once after the baseline and `git check-ignore` only for added paths; a working directory
git ignores (check-ignore -q --no-index .) counts as outside git and uses the hand list. Reagent with node_modules
installed: old guard 10.38 s, new 4.70 s, off 2.46 s. Open lows from the last review, left as they are: git.clj cut of
status entries can throw if check-ignore and status disagree; any non-zero check-ignore exit counts as not ignored;
look-from-ignored-folder test passes because the folder is missing; the out/ mutant test does not assert the file was
written; ADR says two git processes (now three); kept? refilters on every look; start takes a bare boolean.

### Links

- cljs-patrol PR 177: https://github.com/olecve/cljs-patrol/pull/177

<!-- SECTION:DESCRIPTION:END -->
