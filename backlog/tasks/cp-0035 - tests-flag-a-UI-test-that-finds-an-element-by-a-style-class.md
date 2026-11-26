---
id: CP-0035
title: "tests: flag a UI test that finds an element by a style class"
status: Idea
labels: ["tests"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

UI tests find elements by a spade style's class: (.querySelector container (str "." (:class (styles/x)))), or (.contains
(.-classList el) (:class (styles/x))). Such a test pins the implementation, and a class that is the only sign of a state
(an expanded panel) often means the markup carries no meaning for assistive tech either.

### Why

Pushes tests toward role and name queries, next to the react-testing-library-cljs rules, and surfaces state shown only
by styling.

### Flagged

```clojure
;; flagged
(.querySelector container (str "." (:class (styles/expanded))))

;; flagged
(.contains (.-classList el) (:class (styles/expanded)))
```

### Fix

```clojure
;; fix
(is (= "true" (.getAttribute (s/get-by-role "button" {:name "Details"}) "aria-expanded")))
```

### Left alone

```clojure
;; a data attribute or a role query
(.querySelector container "[data-id='b-alpha']")
```

### Notes

Match .querySelector, .querySelectorAll, .closest, .-classList .contains and .getElementsByClassName when the selector
comes from a defclass or defattrs call (resolve through :require aliases), in test files only. Open: also flag a literal
".some-class" string selector, and which tier.
<!-- SECTION:DESCRIPTION:END -->
