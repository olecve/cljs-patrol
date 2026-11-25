---
id: CP-31
title: "a11y: flag an interactive role that cannot take focus"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: ["https://github.com/jsx-eslint/eslint-plugin-jsx-a11y/blob/main/docs/rules/interactive-supports-focus.md"]
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

on-click-on-non-interactive leaves a non-native element alone when it has a :role or a keyboard handler, but never
checks that the keyboard can reach it. [:div {:on-click f :role "button"}] and the same with :tab-index -1 pass today,
yet Tab never lands on them. :tab-index 0 alone is correctly still flagged, since Enter and Space do nothing without a
handler. Probed against the PR 154 build: role string or keyword, :on-key-down or :onKeyDown pass; :onClick,
:tab-index/:tabIndex alone, and :type :button on a div are flagged.

### Why

An element with an interactive role announces itself as a control, so a keyboard user expects to reach it with Tab (WCAG
2.1.1 Keyboard).

### Flagged

```clojure
(defn opener [on-open]
  [:div {:on-click on-open
         :role "button"}
   "Open"])

(defn opener [on-open]
  [:div {:on-click on-open
         :role "button"
         :tab-index -1}
   "Open"])
```

### Fix

```clojure
(defn opener [on-open]
  [:div {:on-click on-open
         :on-key-down #(when (= "Enter" (.-key %)) (on-open))
         :role "button"
         :tab-index 0}
   "Open"])
```

### Notes

Port of eslint jsx-a11y/interactive-supports-focus. Interactive roles: button, link, checkbox, menuitem, option, radio,
switch, tab, textbox, and the rest of the widget roles. Accept :tab-index and :tabIndex, 0 or more. Native interactive
tags (:button, :a with :href, :input) are already focusable. Leave opaque props and built maps the rule cannot read
alone, as the sibling rules do. Decide whether it is a new rule id or a second check in on-click-on-non-interactive.

### Links

- jsx-a11y interactive-supports-focus:
  https://github.com/jsx-eslint/eslint-plugin-jsx-a11y/blob/main/docs/rules/interactive-supports-focus.md

<!-- SECTION:DESCRIPTION:END -->
