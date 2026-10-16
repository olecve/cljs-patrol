# Reagent rules

The rules of the `reagent` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

- **defclass as sole attr** (`defclass-as-sole-attr`, deprecations) — `defclass` where every usage is
  `{:class (style-fn)}` with no other props; should be `defattrs` instead
- **Redundant into-hiccup** (`redundant-into-hiccup`, cleanup) — `(into [:tag …] (map …))` or
  `(into [component …] (for …))` where the mapping body already attaches a `:key` to each element it produces. React
  then has the keys it needs without the `into` splicing the elements in as positional children, so the sequence can sit
  inside the Hiccup vector: `[:<> (for [x xs] ^{:key x} [item x])]`. A key counts in any of its three live forms —
  reader metadata `^{:key k}`, an explicit `(with-meta … {:key k})`, and `:key` in the props map of an element the body
  writes out. Three shapes are left alone on purpose:
  - **A keyless sequence** — `(into [:<>] (map render) nodes)`. There the `into` is load-bearing: it splices the
    elements in as positional children, which React accepts, where the same sequence inside the vector draws a
    missing-key warning
  - **A vector that is not Hiccup** — the head keyword has to name an HTML/SVG element or the `:<>` fragment.
    `(into [:enum] sections)` is a Malli schema, `(into [:cart] ks)` a re-frame db path; `:map` is excluded from the tag
    set, since `[:map {:closed true} …]` is a schema far more often than it is the HTML `<map>` element
  - **A body that derefs** — `(into [:span] (map-indexed (fn [i seg] … @state …) segs))`. Reagent's caveat is a ratom
    deref inside a lazy seq, and the eager `into` is what keeps it out of one

  A `:key` in the props of the container being spliced into — `(into [:div {:class c :key k}] children)` — belongs to
  the container, not to its children, and is not read as one
