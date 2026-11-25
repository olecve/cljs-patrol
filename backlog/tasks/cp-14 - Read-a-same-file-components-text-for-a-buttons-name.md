---
id: CP-14
title: "Read a same-file component's text for a button's name"
status: Done
labels: ["a11y"]
priority: high
created_date: ""
updated_date: ""
references: ["https://github.com/olecve/cljs-patrol/pull/154"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

empty-interactive-element reports a button as having no name when its visible text comes from a child component defined
in the same file. cljs-patrol did not look inside local components, so authors added a redundant :aria-label to silence
it. PR 153 reads a same-file component's rendered Hiccup (defn, defn-, reagent/rum defc), substitutes what the caller
passes for its parameters, and skips text inside aria-hidden elements.

### Why

Removing a redundant aria-label can expose this false positive. It also blocks #24 (aria-label repeats the visible
text).

### Flagged

```clojure
;; no longer flagged (false positive)
(defn- item-caption [{:keys [label]}]
  [:<>
   [:div [:img {:alt "" :src "x.png"}]]
   [:div label]])

(defn item-button [{:keys [label open!]}]
  [:button {:on-click open! :type "button"}
   [item-caption {:label label}]])

;; newly flagged: hidden text is not a name
[:button [:span {:aria-hidden true} "Save"]]
```

### Fix

```clojure
;; nothing to fix: the button is named by the component text
[:button {:on-click open!} [item-caption {:label label}]]
```

### Left alone

```clojure
[:button [:span {:aria-hidden true} [icon]] "Save"]
;; external or unknown components keep today's behavior
[:button [ui/row {:as :div}]]   ; still flagged as before
```

### Notes

Merged 2026-11-16 as PR 154 (merge commit). Four review rounds; round 4 added: a computed or passed :alt names the
image, and at most 64 component calls are read per control. Depth limit 4.

### Links

- cljs-patrol PR 154: https://github.com/olecve/cljs-patrol/pull/154

<!-- SECTION:DESCRIPTION:END -->
