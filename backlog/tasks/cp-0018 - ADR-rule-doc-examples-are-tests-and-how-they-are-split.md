---
id: CP-0018
title: "ADR: rule-doc examples are tests, and how they are split"
status: Done
labels: ["docs"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/156"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

docs_examples_test.clj runs every example in docs/rules/*.md through the rule it illustrates, so the docs cannot drift
from the tool. The way it splits a code block into examples is a set of decisions that live only in PR 155 and chat: a
;; comment at the start of a line opens an example (;; flagged and ;; fix count), which runs to the next such comment,
blank lines included; an unlabeled example ends at a blank line; a part after a blank line must use a name an earlier
part defines; a form that uses a name defined above gets a blank line before it. Nobody remembers why, so a docs-only
change can look like it needs no code, and the next person may undo a choice.

### Why

These decisions need recording, in an ADR and in docstrings, because they are easy to forget.

### Fix

```clojure
;; ADR in docs/adr/ (house format, STE style, no author), options with win/loss next to each:
;; 1. blank line ends an example (before PR 155)
;; 2. explicit ;; --- separator
;; 3. a comment opens an example (chosen, PR 155)
;; plus a revisit trigger

;; and docstrings that state the decision where the code is:
(defn- chunks
  "Split code into examples, ...")
```

### Notes

Merged 2026-11-16 as PR 156. ADR 0004 records the decision. The scope grew: every clojure/edn block of docs/rules.md,
docs/rules/*.md and README.md now runs (config blocks get config checks; other blocks hold labeled examples with
optional ;; config lines). Three review rounds; several docs examples that passed for the wrong reason were fixed.
Follow-up bug: t34.

### Links

- cljs-patrol PR 156: https://github.com/olecve/cljs-patrol/pull/156

<!-- SECTION:DESCRIPTION:END -->
