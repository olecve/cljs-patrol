---
id: CP-22
title: "Flag a click test on an element with no handler"
status: Idea
labels: ["react-testing-library-cljs"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A test clicks an element found by a non-interactive role (group, status, region, heading, img, list…) and then asserts
that nothing happened. Such an element has no click handler, so the assertion can never fail and the test proves nothing
about blocking the action.

### Why

Clicking a non-interactive wrapper does not exercise the control that should block the action.

### Flagged

```clojure
(let [save-calls (atom 0)]
  (render! [:span {:role "group" :aria-label "Save"}
            [:button {:disabled true :type "button"
                      :on-click #(swap! save-calls inc)} "Save"]])
  (act #(fire-event/click (s/get-by-role "group" {:name "Save"})))
  (is (zero? @save-calls) "a disabled Save control does not call its handler"))
```

### Fix

```clojure
;; assert the blocked control instead
(is (true? (.-disabled (s/get-by-role "button" {:name "Save"}))))
```

### Left alone

```clojure
;; clicks on roles that take clicks
(fire-event/click (s/get-by-role "button" {:name "Save"}))
;; unknown role (test id or text query)
(fire-event/click (s/get-by-test-id "row"))
```

### Notes

Match fire-event/click (and user-event click) whose target is a by-role query with a literal non-interactive role.
<!-- SECTION:DESCRIPTION:END -->
