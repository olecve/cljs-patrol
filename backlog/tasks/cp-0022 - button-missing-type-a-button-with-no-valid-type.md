---
id: CP-0022
title: "button-missing-type: a button with no valid :type"
status: Done
labels: ["a11y"]
priority: high
created_date: ""
updated_date: ""
references:
  ["https://github.com/olecve/cljs-patrol/pull/158", "https://github.com/olecve/cljs-patrol/releases/tag/v0.0.57"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A <button> with no type is a submit button. Inside a form, clicking it submits the form, which is rarely what an icon or
toolbar button means. Ports eslint-plugin-react's button-has-type. Literal buttons without :type are a useful target for
a syntax-level check.

### Why

Low false positives, small rule, and a real bug class inside forms.

### Flagged

```clojure
[:button {:on-click f} "Save"]
```

### Fix

```clojure
[:button {:on-click f :type "button"} "Save"]
```

### Left alone

```clojure
[:button {:type "submit"} "Send"]
;; props built elsewhere: the rule cannot see :type
[:button props "Save"]
```

### Notes

Released in v0.0.57 (PR 158, bugs tier) after five review rounds. Also reads components :component-aliases maps to
:button. For configured button components, report invalid literal type values, including values formerly used to select
styling.

### Links

- PR 158: https://github.com/olecve/cljs-patrol/pull/158
- v0.0.57: https://github.com/olecve/cljs-patrol/releases/tag/v0.0.57

<!-- SECTION:DESCRIPTION:END -->
