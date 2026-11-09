# Reagent rules

The rules of the `reagent` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only reagent`.

## Rules

### `defclass` as sole attr

`defclass-as-sole-attr` · deprecations — A `defclass` only ever used as the whole props map, which `defattrs` says
directly.

```clojure
;; flagged
(defclass row-style []
  {:display :flex})
(defn row [] [:div {:class (row-style)}])

;; fix
(defattrs row-attrs []
  {:display :flex})
(defn row [] [:div (row-attrs)])
```

**Left alone:**

```clojure
(defclass row-style []
  {:display :flex})
;; carries other props
(defn row [on-select] [:div {:class (row-style) :on-click on-select}])
```

### Redundant `into` around Hiccup

`redundant-into-hiccup` · cleanup — An `into` splicing children that already carry keys.

```clojure
;; flagged
(defn items [xs]
  (into [:ul] (for [x xs] ^{:key (:id x)} [:li (:name x)])))

;; fix
(defn items [xs]
  [:ul (for [x xs] ^{:key (:id x)} [:li (:name x)])])
```

**Left alone:**

```clojure
;; no keys: into is what avoids the warning
(defn nodes [xs] (into [:<>] (map render-node) xs))

;; a Malli schema, not Hiccup
(def schema (into [:enum] sections))
```

<details>
<summary>Details</summary>

An `(into [:tag …] (map …))` or `(into [component …] (for …))` whose mapping body already attaches a `:key` to each
element it produces. React then has the keys it needs without the `into` splicing the elements in as positional
children, so the sequence can sit inside the Hiccup vector: `[:<> (for [x xs] ^{:key x} [item x])]`. A key counts in any
of its three live forms — reader metadata `^{:key k}`, an explicit `(with-meta … {:key k})`, and `:key` in the props map
of an element the body writes out. Three shapes are left alone on purpose:

- **A keyless sequence** — `(into [:<>] (map render) nodes)`. There the `into` is load-bearing: it splices the elements
  in as positional children, which React accepts, where the same sequence inside the vector draws a missing-key warning
- **A vector that is not Hiccup** — the head keyword has to name an HTML/SVG element or the `:<>` fragment.
  `(into [:enum] sections)` is a Malli schema, `(into [:cart] ks)` a re-frame db path; `:map` is excluded from the tag
  set, since `[:map {:closed true} …]` is a schema far more often than it is the HTML `<map>` element
- **A body that derefs** — `(into [:span] (map-indexed (fn [i seg] … @state …) segs))`. Reagent's caveat is a ratom
  deref inside a lazy seq, and the eager `into` is what keeps it out of one

A `:key` in the props of the container being spliced into — `(into [:div {:class c :key k}] children)` — belongs to the
container, not to its children, and is not read as one.

</details>

### Empty fragment props

`empty-fragment-props` · cleanup — A fragment written with an empty `{}`.

```clojure
;; flagged
(defn phase-actions []
  [:<> {}
   [edit-controls {:editing? false}]
   [accept-button]])

(defn status [busy-reason]
  [:<> {}
   busy-reason
   [spinner]])

;; fix
(defn phase-actions []
  [:<>
   [edit-controls {:editing? false}]
   [accept-button]])

(defn status [busy-reason]
  [:<>
   busy-reason
   [spinner]])
```

**Left alone:**

```clojure
;; a real prop
(defn row-group [id item] [:<> {:key id} [row item]])
```

<details>
<summary>Details</summary>

Why: a fragment takes no props but `:key`, and a keyed fragment writes `{:key k}`. An empty `{}` reads as though
something is configured when nothing is. All it changes is which child Reagent reads as props: Reagent reads a
fragment's second element as its props when it is a map or `nil`. A map child cannot render anyway, so in practice the
`{}` only keeps a `nil` first child from being read as props, which keeps the children after it in place. Code should
not depend on that, so every empty `{}` is flagged, whatever follows it. A `#_` form is skipped wherever it sits.

Left alone:

- a `{}` holding anything, `:key` or otherwise, and a `{}` under metadata
- a fragment that is quoted, discarded, or sits in a style declaration or the ns form

</details>

### Redundant fragment

`redundant-fragment` · cleanup — A fragment around a single element, which the element can stand in for.

```clojure
;; flagged
(defn company-preferences [company]
  [:<>
   [:section.company-preferences
    [settings-form-title "Links to external systems"]
    [links-form company]]])

;; fix
(defn company-preferences [company]
  [:section.company-preferences
   [settings-form-title "Links to external systems"]
   [links-form company]])
```

**Left alone:**

```clojure
;; busy-reason could be a map Reagent reads as props
(defn status [busy-reason] [:<> busy-reason])

;; data into gives more children
(defn table [header rows] (into [:<> [header]] rows))

;; the fragment carries the key
(defn cells [id] [:<> {:key id} [cell id]])
```

<details>
<summary>Details</summary>

Why: a fragment groups several children where only one element may stand. Around a single element it groups nothing, and
the element can stand in its place. ESLint's `react/jsx-no-useless-fragment` reports the same shape in JSX.

Flagged: a `[:<> …]` whose only child is a Hiccup element written out, an HTML tag or a component, under no props map,
standing where its value renders. That is the last form of a function or `defmethod` body, or, when what holds it
renders too, a child of an element, a value in its props map, the last form of a `for` body, or a value a `let`, `when`,
`if`, `cond` or `case` yields. A `#_` form is skipped wherever it sits.

The rule asks for one shape everywhere, even where the fragment changed something. If one branch yields `[:<> [panel]]`
and another a bare `[panel]`, the fragment makes React remount `panel` when the branch changes. Without it, `panel`
keeps its state. Where a remount is wanted, give each branch its own `^{:key …}`, which says so. The same goes for a
fragment inside a JS component that clones its child, such as a Radix `:as-child` trigger: the element itself is what it
expects.

Left alone, since the scan cannot vouch that the element can take the fragment's place:

- a child that is a symbol, a call, a `for` or a `when`, which may yield a map Reagent reads as the fragment's props, or
  no element at all; more than one child; and a child under metadata or a reader conditional
- a fragment with props, or handed a key by reader metadata or `with-meta`
- a fragment handed to a call, such as `into`, `conj` or `=`, which may change it or compare it as data, also through a
  `when`, an `if` or an element that holds it; a fragment bound in a `let`; and one that is not the last form of a
  function body
- a vector that is data rather than Hiccup, such as `[:<> [:path :to :key]]`
- a fragment that is quoted, discarded, or sits in a style declaration or the ns form

</details>

### Inline component

`inline-component` · cleanup — A `let` binding whose Hiccup holds two or more elements with event handlers.

```clojure
;; flagged
(defn settings-form [{:keys [on-cancel on-save]}]
  (let [actions [:div {:class (action-row)}
                 [button {:on-click on-save}
                  "Save"]
                 [button {:on-click on-cancel}
                  "Cancel"]]]
    [:form
     [fields]
     actions]))

;; fix
(defn- form-actions [{:keys [on-cancel on-save]}]
  [:div {:class (action-row)}
   [button {:on-click on-save}
    "Save"]
   [button {:on-click on-cancel}
    "Cancel"]])

(defn settings-form [props]
  [:form
   [fields]
   [form-actions props]])
```

**Left alone:**

```clojure
;; one element with a handler, bound so both branches can show it
(defn run-button [{:keys [busy? on-run]}]
  (let [launcher [button {:disabled busy?
                          :on-click on-run}
                  "Run"]]
    (if busy?
      [tooltip {:content "Wait for the current run"}
       launcher]
      launcher)))

;; one element in a `for` body is one element, however many items the `for` gives
(defn card-list [{:keys [items on-open]}]
  (let [cards [:ul
               (for [item items]
                 ^{:key (:id item)}
                 [card {:item item
                        :on-open on-open}])]]
    [:section cards]))
```

<details>
<summary>Details</summary>

Why: a small element bound in a `let` is often the clearest form, such as one button bound once so two branches can show
it with or without a wrapper. A block of several controls, each with its own handler, is a component written inside
another one. It has no name, props or docstring of its own, cannot be rendered or tested alone, and makes the outer
`let` long. So the rule never flags Hiccup in a `let` as such, only a binding holding several interactive elements.

Flagged: a binding of a `let` or Reagent's `with-let` whose value is Hiccup, a tag, a fragment or a component, also
under a `when`, an `if`, a `cond`, a `case`, a `let`, or a `for` or `map`, holding two or more elements whose attrs have
an unqualified key starting with `:on-` set to no literal, the value itself included. Each element counts once, however
many handlers it has. The hint names the binding and the count.

Only the Hiccup tree is counted, through `^{:key …}` metadata: an element and its children, and what a `when`, an `if`,
a `let`, or a `for` or `map` body yields. A loop body counts once however many items it gives, and an `if`, `cond` or
`case` as its busiest branch, since only one renders. An attrs map, a handler's body and any other call are not read, so
an event vector a handler dispatches, `[::open {:on-confirm f}]`, is no element. Left alone as well: a data vector such
as a re-frame path, a binding that destructures, a value that is any other call, and a `let` that is quoted, discarded,
or sits in a style declaration.

</details>
