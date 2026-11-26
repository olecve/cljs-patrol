---
id: CP-0070
title: "Prefer :tab-index in Reagent DOM Hiccup"
status: Done
labels: ["reagent", "a11y"]
created_date: "2026-11-25"
updated_date: "2026-11-26"
references: ["https://github.com/olecve/cljs-patrol/pull/189"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

Reagent DOM attribute maps use `:tab-index`, `:tabIndex`, and `:tabindex` for the same DOM attribute. Prefer the
kebab-case spelling in literal Hiccup that renders a native element.

### Why

One spelling makes props easier to search and avoids the React warning for lowercase `tabindex`.

### Flagged

```clojure
[:button {:type "button" :tabIndex -1} "More"]
[:a {:href "/help" :tabindex 0} "Help"]
```

### Fix

```clojure
[:button {:type "button" :tab-index -1} "More"]
[:a {:href "/help" :tab-index 0} "Help"]
```

### Left alone

```clojure
(react/createElement "button" #js {:type "button" :tabIndex 0} "More")
[:> MenuItem {:tabIndex -1} "More"]
```

### Notes

Replaced before release by `forbid-props` configuration with `:disallowed-for :native` for `:tabIndex` and `:tabindex`.
The dedicated rule was never released. This task remains Done.
[ADR 0015](../../docs/adr/0015-replace-prefer-tab-index-with-forbid-props-config.md) records the decision and its
limits.

Make this a cleanup rule. Check readable props on native Hiccup tags. Leave raw JavaScript objects, component props, and
maps outside Hiccup alone. If a map supplies two spellings, ask the author to keep one key and choose its value.

Accessibility rules still read all supported spellings when deciding whether an element can take focus. The spelling
finding and the check for an invalid value remain independent.

### Links

- [Implementation in PR #189](https://github.com/olecve/cljs-patrol/pull/189)
- [Rule documentation](../../docs/rules/forbid-props.md#forbid-props-options)
- [CP-0071 SVG coverage](cp-0071%20-%20Recognize-SVG-tags-in-the-tab-index-rule.md)
- [CP-0010](cp-0010%20-%20Flag-a-tooltip-trigger-that-cannot-take-focus.md)
- [Invalid tabindex rule](../../docs/rules/a11y.md#invalid-tabindex)

<!-- SECTION:DESCRIPTION:END -->
