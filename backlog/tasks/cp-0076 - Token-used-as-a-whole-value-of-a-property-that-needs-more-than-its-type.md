---
id: CP-0076
title: "tokens: flag a token used as the whole value of a property that needs more than its type"
status: Idea
labels: ["tokens"]
created_date: "2026-11-26"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A design-token namespace can change the type of a token and keep its name. For example, a border token that held a whole
CSS border shorthand now holds only a color, and a helper adds the width and the style:

```clojure
;; before
(def border-primary "1px solid var(--app-separator)")

;; after
(def border-primary "var(--app-border)")
(defn border [color] (str "1px solid " color))
```

Code written for the old meaning still compiles and passes tests. The browser gets a border with a color and no style,
and draws no border:

```clojure
;; flagged: no border is drawn
(defclass panel []
  {:border-top tokens/border-primary})

;; fix
(defclass panel []
  {:border-top (tokens/border tokens/border-primary)})

;; left alone: the token as a color
(defclass panel []
  {:border-color tokens/border-primary})
```

### Why

Branches written before the change, and old docs, keep the old form, so new uses keep appearing. The same token is
correct in one property and wrong in another, so a check on the token name alone does not work. Other token families can
change type the same way later, for example radius, shadow and typography composites.

### Ideas

- One general rule driven by config, not one rule for each token family. A new `tokens` group with a
  `token-type-mismatch` rule.
- The tool cannot know the type of a token from its value: `"var(--app-border)"` does not tell that it holds a color.
  The project lists the tokens of each type:

  ```clojure
  {:tokens {:types [{:type :color
                     :tokens #{app.tokens/border-primary
                               app.tokens/border-secondary}
                     :fix "Wrap it: (app.tokens/border token), or use :border-color."}]}}
  ```

- A built-in table gives the properties that need more than a value of each type. For `:color`: `:border`, the four side
  properties, `:border-block*`, `:border-inline*`, `:outline` and `:column-rule`.
- A finding names the token, the property and the configured fix.
- The rule reports only a bare token as the whole value. A token inside a call, such as `(tokens/border token)`, is left
  alone. The project does not need to name its helper; the helper goes only in the fix message.
- Forms and positions for a first version:
  - `defclass` and `defattrs`, plain `def` style maps, and inline Hiccup `:style` maps;
  - a value that is a branch result of `when`, `if`, `case`, `or` or `cond->`;
  - any alias of the token namespace, resolved from the `ns` requires.
- Reuse the style-map reader of the `focus-ring` group (nested selectors, `at-media` and `at-supports`, branches). Do
  not reuse its token-value table: the type comes from config.
- Later versions:
  - a config list of custom property names whose value is a border, such as `#{"--app-card-border"}`, checked the same
    way;
  - other types (radius, shadow, typography) as new table entries.
- Projects without a cljs-patrol step adopt it with one native binary, one config file and `--only tokens`. Old branches
  fail on their own once the main branch has no such findings, because each old use is a new finding.

### Notes

Leave `str` builds out of the first version: `(str "1px solid " token)` is the correct form. Decide whether the group
reads test files. Write an ADR before the implementation: a general config-driven rule against one rule for each token
family.

<!-- SECTION:DESCRIPTION:END -->
