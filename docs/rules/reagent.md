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
(defn row [on-select] [:div {:class (row-style) :on-click on-select}]) ; carries other props
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
(defn nodes [xs] (into [:<>] (map render-node) xs)) ; no keys: into is what avoids the warning

(def schema (into [:enum] sections)) ; a Malli schema, not Hiccup
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

`empty-fragment-props` · cleanup — A fragment's `{}` that keeps nothing from being read as props.

```clojure
;; flagged
(defn phase-actions []
  [:<> {}
   [edit-controls {:editing? false}]
   [accept-button]])

(defn title-adornment [unsaved?]
  [:<> {}
   (when unsaved?
     [:span "Unsaved"])])

;; fix
(defn phase-actions []
  [:<>
   [edit-controls {:editing? false}]
   [accept-button]])

(defn title-adornment [unsaved?]
  [:<>
   (when unsaved?
     [:span "Unsaved"])])
```

**Left alone:**

```clojure
(defn status [busy-reason] [:<> {} busy-reason [spinner]]) ; busy-reason could be a map at runtime

(defn children [items] [:<> {} (render-children items)]) ; so could a call's result

(defn row-group [id item] [:<> {:key id} [row item]]) ; a real prop
```

<details>
<summary>Details</summary>

Why: a fragment takes no props but `:key`, and a keyed fragment writes `{:key k}`. An empty `{}` reads as though
something is configured when nothing is. Reagent reads a fragment's second element as its props when it is a map or
`nil`, the same as for an HTML tag. So the `{}` matters only when the child after it could be a map, which Reagent would
otherwise take as the props and not render.

Flagged: a `[:<> {} …]` whose `{}` is empty and whose next child is missing or can yield no map: a vector, which covers
a component written as `[edit-controls …]`, or a literal string, number, keyword or `nil`. A `when`, `if`, `cond`,
`case`, `let` and the like count by every value they can yield. One that yields `nil`, written or because no branch
matched, is safe: with the `{}` gone, Reagent reads the `nil` as the props, which renders the same nothing. A `#_` form
is skipped wherever it sits.

Left alone, since the scan cannot vouch that the next child is no map:

- a symbol, a call, a `for` or a `map`, or a branch yielding one, and a child under metadata or a reader conditional
- a `{}` holding anything, `:key` or otherwise, and a `{}` under metadata
- a fragment that is quoted, discarded, or sits in a style declaration or the ns form

</details>
