---
id: CP-0016
title: "Docs examples: let one example span blank lines"
status: Done
labels: ["docs"]
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/155"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

test/cljs_patrol/docs_examples_test.clj splits every Flagged, Fix and Left-alone code block in docs/rules/*.md into
chunks at a blank line between two top-level forms, and lints each chunk as its own file, so an example cannot lean on
another. An example that needs two forms, such as a local component plus the button that renders it
(empty-interactive-element after PR 153), must therefore have no blank line between them. A blank line there splits the
pair, the button loses the component, and the Left-alone example gets flagged. The rule page reads cramped.

### Why

Rule docs lead with examples; multi-form examples should be as readable as single-form ones.

### Flagged

```clojure
;; today: must stay glued together
(defn- item-caption [{:keys [label]}]
  [:span label])
(defn item-button [{:keys [label open!]}]
  [:button {:on-click open!} [item-caption {:label label}]])
```

### Fix

```clojure
;; idea: an explicit separator between examples, so a blank line can sit inside one
(defn- item-caption [{:keys [label]}]
  [:span label])

(defn item-button [{:keys [label open!]}]
  [:button {:on-click open!} [item-caption {:label label}]])
;; ---
(defn save-button [save!] …)
```

### Notes

Merged 2026-11-16 as PR 155. A comment that opens a line starts an example (;; flagged and ;; fix count); an unlabeled
example ends at a blank line; a part after a blank line must use a name an earlier part defines. 20 examples got labels,
and forms that use a name above got a blank line. Decisions to record: CP-0018.

### Links

- cljs-patrol PR 155: https://github.com/olecve/cljs-patrol/pull/155

<!-- SECTION:DESCRIPTION:END -->
