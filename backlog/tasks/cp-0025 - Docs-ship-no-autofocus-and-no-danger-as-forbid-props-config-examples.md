---
id: CP-0025
title: "Docs: ship no-autofocus and no-danger as forbid-props config examples"
status: Idea
labels: ["forbid-props"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

jsx-a11y's no-autofocus and react's no-danger map onto forbid-props entries rather than new rules. Document
configurations for :auto-focus/:autoFocus, :dangerouslySetInnerHTML, and :access-key/:accessKey.

### Why

Teams get the ESLint behavior from config, with no new code.

### Flagged

```clojure
;; config {:forbid-props {:props [{:prop :auto-focus :message "Move focus on purpose, after a user action."}]}}
;; flagged
[:input {:auto-focus true}]
```

### Fix

```clojure
[:input {:ref focus-on-open!}]
```

### Left alone

```clojure
[:input {:type "text"}]
```

### Notes

Docs-only change to docs/rules/forbid-props.md; the examples run as tests (ADR 0004), so check the config shape the
group accepts.
<!-- SECTION:DESCRIPTION:END -->
