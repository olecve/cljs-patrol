# Forbid props rules

The rules of the `forbid-props` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only forbid-props`. It does nothing until `:forbid-props` is set in
`.cljs-patrol/config.edn`; see [Forbid props options](#forbid-props-options). The examples below run under the config
shown there.

## Rules

### Forbidden prop

`forbidden-prop` · deprecations · config [`:forbid-props`](#forbid-props-options) — A prop the config forbids on this
element or component. The finding gives the entry's message.

```clojure
;; flagged
(defn save-button [allowed?]
  [:button {:aria-disabled (not allowed?)
            :on-click #(when allowed? (save!))
            :type "button"}
   "Save"])

(defn comment-button []
  [button/button {:aria-disabled true
                  :on-click nil}
   "Comment"])

;; fix
(defn save-button [allowed?]
  [:button {:disabled (not allowed?)
            :on-click save!
            :type "button"}
   "Save"])
```

**Left alone:**

```clojure
(defn option [disabled label]
  [:div {:aria-disabled (when disabled "true")
         :role "option"}
   label])

(defn link [label]
  [:a {:aria-disabled true
       :role "link"}
   label])

(defn idle-button [label]
  [:button {:aria-disabled false}
   label])

(defn built-button [attrs label]
  [:button (merge attrs {:aria-disabled true})
   label])
```

<details>
<summary>Details</summary>

The rule reads every Hiccup vector whose head is an HTML tag, such as `:button` or `:button.primary`, or a component the
file's aliases and refers resolve, such as `button/button` for `app.ui.button/button`. A keyword that names no HTML
element, as in a re-frame event vector or a Malli schema, is not an element, and a local component is not one either.

A prop counts when the element's props are a literal map, or a symbol bound to one, and the prop's value reaches the
element. Reagent drops a prop whose value is `false` or `nil`, so `{:aria-disabled false}` is left alone. Props built by
a call, `(merge attrs {…})` or `(assoc attrs …)`, are not read, so a prop in them is never reported.

The example config forbids `:aria-disabled` on buttons, a pattern coding agents copy from blog posts to keep a disabled
button focusable for a tooltip. On a native button it only announces the state: click, Enter and Space still fire, so
every handler has to guard itself, and a disabled look needs extra CSS. Native `:disabled` does both. When the reason
must stay reachable by keyboard, a focusable wrapper carries it:
`[app.components/blocked-action {:reason "…"} [button/button {:disabled true} "Comment"]]`. Elements with no `disabled`
attribute, `:a` and a `:div` with `role="option"`, keep `aria-disabled`, and the config does not name them. A case that
needs it on a button anyway, one that turns busy while it has focus, goes in the [baseline](../baseline.md).

</details>

## Forbid props options

The group reads `:forbid-props` from `.cljs-patrol/config.edn`, a vector of entries, and does nothing until it is set.
The examples on this page run under this config:

```clojure
{:forbid-props
 [{:prop :aria-disabled
   :disallowed-for #{:button app.ui.button/button}
   :message "Use :disabled. When the reason must stay reachable by keyboard, wrap the button in app.components/blocked-action."}]}
```

Each entry is a prop keyword, forbidden on every element and component, or a map:

- `:prop` — the prop, a keyword as written in the props map.
- `:disallowed-for` — the tags and components the prop is forbidden on: tag keywords such as `:button`, and fully
  qualified component symbols such as `app.ui.button/button`.
- `:allowed-for` — the tags and components the prop is allowed on; it is forbidden on every other. An entry takes one of
  the two at most, and with neither the prop is forbidden everywhere.
- `:message` — what the finding says: why the prop is forbidden and what to use instead. Without it the finding names
  the prop and the element.

An entry of another shape, such as a prop that is not a keyword, an unknown key, or both `:allowed-for` and
`:disallowed-for`, stops the run with a message that names it.

The shape follows ESLint's
[`react/forbid-dom-props`](https://github.com/jsx-eslint/eslint-plugin-react/blob/master/docs/rules/forbid-dom-props.md)
and
[`react/forbid-component-props`](https://github.com/jsx-eslint/eslint-plugin-react/blob/master/docs/rules/forbid-component-props.md),
as one rule: Hiccup writes a tag and a component the same way.
