# ADR 0003: Tell Data Vectors From Hiccup

## Status

Proposed, 2026-11-13

## Context

cljs-patrol reads a vector as Hiccup when its head is an HTML tag keyword from `hiccup/html-tags`, or names a component.
The groups that read Hiccup then check it as an element: `a11y`, `forbid-props`, `reagent` and
`react-testing-library-cljs`. Other groups, such as `layout`, read the same vectors as plain code.

ClojureScript code also writes data models in the same shape:

- **Malli schemas.** `[:map [:label {:optional true} [:maybe :string]]]`. A keyword names each entry, and the keyword
  can be an HTML tag name, such as `:label`, `:button` or `:title`.
- **re-frame db paths, event vectors and keyword lists.** `[:settings :style]`, `(rf/dispatch [:select {…}])`,
  `[:line :rect :circle]`.

Three guards protect against this today:

- `hiccup/html-tags`, which the groups that read Hiccup share, leaves out `:map`, `:search`, `:filter`, `:image` and
  `:desc`. Each is also an everyday Malli schema, db path or event id.
- `label-not-associated` (`hiccup-label?`) reads a `[:label …]` as markup only when `names-a-dom-attr?` finds a DOM
  attribute in its props, when its body holds a string or a nested vector, or when a props call has a non-empty body.
- `forbid-props` (`re-frame-vector?`) skips a vector that is itself an argument of `dispatch`, `subscribe`,
  `reg-event-*` or `reg-sub`, a vector that follows a `:dispatch`-like effect key, an fx tuple, and the items of
  `:dispatch-n`. Hiccup nested in such a vector is still checked.

These guards fail for a Malli entry whose type is a vector. Take this schema for the props of a tag component:

```clojure
(def tag-props-schema
  [:map {:closed true}
   [:label {:optional true
            :description "The text of the tag."}
    [:maybe [:or :string :int]]]])

(defn tag {:malli/schema [:=> [:cat tag-props-schema] :any]} [{:keys [label]}]
  [:span.tag label])
```

`label-not-associated` reports the `[:label …]` entry as a `<label>` that is tied to no control. The nested `[:maybe …]`
is a Malli type, but `hiccup-label?` reads it as markup.

A scan of a large codebase, tests included, found these facts:

- **Top-level `def`s whose value is a vector are almost all data.** They hold Malli schemas, db paths, keyword lists,
  and lists of colors, icons or test fixtures. Very few of them have an HTML tag head, and the children of those are
  keywords.
- **The rest hold Hiccup, or look like it.** A few hold help texts and legal texts, such as
  `(def terms-text [:div [:p "…"] …])`. They reach components as prop values, `{:content terms-text}`, often from other
  files. Some hold Garden CSS rules, such as `[:div.paragraph [:br {:margin-top "5px"}] …]`, that a `defclass` uses.
- **Vector `def`s can hold functions that return Hiccup.** Route tables hold `{:view (fn [match] [page match])}`, and
  action tables hold `{:render-item (fn [user] [dropdown/item {…} "…"])}`.
- **Hiccup also travels as data outside vector `def`s.** Event handlers put toasts and modals in effects, with Hiccup in
  map values: `{:message "…" :description [:div …]}`. Map `def`s hold toast maps and `r/as-element` calls. All of these
  are in function bodies or in map and call `def`s.
- **In this scan, all current findings inside the data `def`s come from Malli entries,** apart from style findings of
  the `layout` group. The `label-not-associated` findings in data `def`s are all false positives. The
  `form-after-split-map` findings in Malli `def`s are real.

Hiccup can live in a `defn`, a `defc`, a `fn` or a `def`, and each place is valid. Thus the place of a vector cannot
tell alone that it is data. For a `def`, its content must tell.

## Decision

Read a vector `def` as data only when its content is clearly data. Keep a Malli type check in `label-not-associated` as
a fallback for schemas outside vector `def`s.

The parts of the decision:

1. **A test tells clear data.** The test leans toward Hiccup: a vector it reads as data loses today's findings, so it
   reads a vector as data only in the shapes that a data model has and markup does not. A vector is clearly data when
   one of these holds:
   - It is a Malli schema all through. Its head is a data type keyword, and its other elements are an optional
     properties map, then children. Every vector anywhere inside a value of the properties map, also inside a nested
     map, set, list or reader conditional, is itself clearly data. In a `:map`, `:multi`, `:catn`, `:altn`, `:orn` or
     `:andn` schema, each child is an entry `[key properties? schema]`: the key is a keyword, a string or a number, the
     properties map is optional, and the schema is a Malli schema. In an `:enum`, `:=`, `:not=`, `:<`, `:<=`, `:>=`,
     `:re` or `:ref` schema, each child is a literal value: a keyword, a symbol, a string, a number, a boolean or a
     regular expression. In any other schema, each child is a Malli schema. A Malli schema is a keyword, a symbol, a
     regular expression, a call in which every vector is clearly data, or a vector that is a Malli schema all through. A
     string, a number or a boolean is a literal value, not a schema, so `[:a {:href "/"} "Home"]` is no entry.
   - It has two or more elements, and every element is a keyword, such as the db path `[:settings :style]` or the
     keyword list `[:line :rect :circle]`. Inside a list, or inside a call in a schema position, such a vector counts
     only when its head, read without class and id shorthand, is not in `hiccup/html-tags`, because
     `[[:th :name] [:th.cell :email]]` renders two table cells.
   - Its head is a vector, and every element is clearly data, such as a list of schemas.

   The data type keywords are the keyword types of `malli.core/base-schemas`, `type-schemas`, `sequence-schemas` and
   `comparator-schemas`, without the ones that Hiccup or Garden also use: the tags in `hiccup/html-tags`, which today
   take out `:symbol`, the Reagent interop `:>`, and the Garden selectors `:*` and `:+`. The set is computed from those
   lists, so it follows `hiccup/html-tags`. `:map` and `:set` stay in the set, though they also name the HTML `<map>`
   and the SVG `<set>` elements, which `hiccup/html-tags` leaves out. `:malli.core/schema` counts only when it is
   written out. An aliased `::m/schema` is not resolved.

   A `#_` form is no element, and metadata on a vector, at any depth, is looked through. Every vector in such a metadata
   map, and every key and value of a properties map at any depth, must be clearly data too. Any other vector is not
   clearly data, and stays Hiccup: an HTML element, such as an image map `[:map {:name "nav"} [:a {:href "/"} "Home"]]`,
   a custom element, a component, a fragment, a list of elements, a Garden rule, a pair such as `[:title "asc"]`, a path
   that holds a number or a string, and a schema with Hiccup in a schema position or in a property value.

2. **The data `def` rule.** For `a11y`, `forbid-props`, `reagent` and `react-testing-library-cljs`, the value of a
   top-level `def` is data, with the vectors nested in it, when the value is a vector with a keyword head or a vector
   head, and it is clearly data.
   - A `def` is a `def` or `defonce` form whose parent is the file itself, with or without metadata on the name or a
     docstring. A `def` inside a reader conditional or another form is read as today. Its value is the last form, with
     `#_` forms dropped. One helper reads the value for this rule and for the lookup of a symbol's `def`, so both see
     the same value. A value with its own metadata, a map, a call, a symbol-headed vector or a reader conditional is
     read as today.
   - A `fn`, `fn*` or `#(…)` removes the data `def` rule from the forms under it, so a route view stays Hiccup. So does
     the argument of `reagent.core/as-element`, resolved through aliases.
   - Other groups, such as `layout`, read all `def` values as today.

3. **The label-check fallback.** In `hiccup-label?`, a nested vector whose head is a Malli core type keyword does not
   make the label markup. The types are the keyword types of `malli.core/base-schemas`, `type-schemas`,
   `sequence-schemas` and `comparator-schemas`, without `:>`.

[Options](#options) compares this choice with the other options.

## Options

### A clearly data vector `def` is data (chosen, with the label-check fallback)

The test for clear data and the data `def` rule above, with the label-check fallback.

- Good, because it covers Malli schemas, db paths and keyword lists, which the scan found to be almost all data `def`s.
- Good, because any vector it does not know stays Hiccup, as today. A new HTML attribute, a custom element or a new way
  to write Hiccup cannot turn markup into data.
- Good, because the test is small: one list of Malli types and a check of keywords. It needs no list of CSS properties
  or HTML attributes.
- Good, because a `def` that holds Hiccup stays valid code and stays checked, and it needs no change to the code under
  check.
- Bad, because data in other shapes stays Hiccup: a pair with a string, such as `(def default-sort [:title "asc"])`, a
  path with a number or a string, a list of colors, and test fixtures with map heads.
- Bad, because a Hiccup vector of keywords only, such as `[:p :hello]`, which Reagent renders as the text "hello",
  becomes data.
- Bad, because the data `def` rule does not cover schemas outside vector `def`s. Only the fallback covers their
  `[:label …]` case.

### A vector `def` without clear markup is data (rejected)

A test for clear markup, and every vector `def` that it does not read as markup is data. A vector is markup when it has
a markup head and a sign: a string or number child, a props map with a key that is neither a CSS property nor a Malli
property key, or a nested vector that is markup.

- Good, because it covers data in more shapes than the chosen test, such as pairs and lists of colors.
- Bad, because each miss loses findings without a sign, and each review found new kinds of real Hiccup that it missed.
  It read these as data, though the rules report them today: a custom element or an excluded tag over other markup, as
  `[:my-dialog [:button {:aria-hidden true :tab-index 0} "x"]]`, props named by a symbol, as `[:img logo-attrs]`, HTML
  attributes that share a CSS name, as `[:img {:width 1 :height 1}]`, an empty props map, as `[:<> {} …]`, and
  components with no arguments, as `[:button [icons/close]]`.
- Bad, because it needs a list of CSS properties with exceptions for element attributes, and a list of Malli property
  keys.

### Hiccup only in functions, enforced by a rule (rejected)

A new rule, `hiccup-in-def`, reports a vector `def` that is clearly markup. The fix is a component function. Then every
vector `def` could be data.

- Good, because the classification of `def`s becomes simple: every vector `def` is data.
- Good, because it is close to JSX, where markup is syntax and eslint-plugin-react never has to guess.
- Bad, because a `def` is a valid place for Hiccup, as a `defn` and a `defc` are. The rule makes users change valid code
  to help the linter.
- Bad, because it adds no safety: a Hiccup `def` is not clearly data, so the groups already keep checking it.
- Bad, because each Hiccup `def` gets a finding, which goes to the baseline or needs a change.

### Data `def`s, with Hiccup when the file renders the name (rejected)

Any `def` value is data, unless the same file uses the name as a child of a Hiccup vector or in the result of a
function.

- Good, because it does not look at the content of the value, so a data pair such as `[:title "asc"]` is data.
- Bad, because in the scan, the Hiccup `def`s reach components as prop values, often from other files. Their findings
  would go away without a sign.
- Bad, because a map `def` that holds toast Hiccup would become data.
- Bad, because it needs a new scan of each file for the places where a name is rendered.

### An explicit marker (rejected)

`^:cljs-patrol/data` on a `def`, or a config list of data namespaces or `def`s, like the `:lint-as` and ignore
annotations of clj-kondo.

- Good, because it is precise, and the user decides each case.
- Good, because it needs no test that can guess wrong.
- Bad, because each data `def` needs a mark. Data `def`s are almost all vector `def`s, so the cost falls on the common
  case.
- Bad, because a missing mark gives the false positive back, and a wrong mark hides findings.

### Exclusion by path (rejected)

Treat namespaces such as `*.schema` or `*.db` as data.

- Good, because it is simple to configure and to explain.
- Bad, because schemas and db paths often live next to their components, in the same namespace.
- Bad, because a namespace that holds both data and Hiccup cannot be split by path.

### A Malli naming convention (rejected)

A rule that requires schema `def`s to be named `*-schema` or `=>*`.

- Good, because it makes the intent clear to a reader.
- Good, because a rule could skip schema `def`s by name.
- Bad, because the classification does not need it: the test reads a schema by its Malli type head.
- Bad, because it makes users rename many `def`s. A team that wants it can add it later as a style rule.

### Malli types in the label check alone (rejected, kept as the fallback)

In `hiccup-label?`, a nested vector whose head is a Malli type keyword does not make the label markup.

- Good, because it fixes the entry above with a small change, and other groups read as before.
- Good, because it also covers schemas outside vector `def`s.
- Bad, because it fixes one rule. The next rule that reads a keyword-headed vector needs a guard of its own.
- Bad, because it does not cover db paths or other data.

### Follow schema roots (deferred)

A vector is data when a Malli root can reach it: a `:malli/schema` value, an argument of `m/schema`, `m/validate`,
`m/=>` or `mu/*`, or a `:registry` map. The search follows the `def`s that a root names, as the `:malli/schema` of `tag`
names `tag-props-schema`.

- Good, because it is precise: a vector is data only when Malli uses it.
- Good, because it covers schemas outside vector `def`s, such as call `def`s and function bodies.
- Bad, because it needs `def` references across files, because a schema often names schemas in other namespaces.
- Bad, because it does not see a schema that is never given to Malli, for example one that only a test uses.
- Bad, because it knows only Malli. A db path is not covered.

### Run Malli on the vectors (rejected)

Compile each candidate vector with Malli, and treat the ones that compile as schemas.

- Good, because Malli itself decides what a schema is.
- Bad, because an entry is not a schema alone. Only its `[:map …]` compiles.
- Bad, because schemas often name vars of other files and local registries, which do not resolve outside the app.
- Bad, because cljs-patrol reads source and never runs it. Malli would be a new dependency of the native binary.

### Skip Malli entry schemas (rejected)

Every vector in a `[:map …]`, `[:map-of …]`, `[:multi …]`, `[:catn …]`, `[:altn …]` or `[:orn …]` is data, for the
groups that read Hiccup.

- Good, because it is a small change, and it fixes the entry above.
- Bad, because it does not see an entry outside a literal `[:map …]`, such as `(into [:map] entries)` or
  `(def entries [[:label …]])`.
- Bad, because it knows only Malli. A db path is not covered.

## Consequences

- A `def` that holds Hiccup needs no change, and the groups keep checking it.
- A data `def` in a shape that the test does not know stays Hiccup, as today. For example
  `(def default-sort [:title "asc"])`, `(def item-path [:items 0 :name])`, a list of colors, test fixtures with map
  heads, a schema with a function, such as `[:fn #(pos? %)]`, and a schema with a function in its properties, such as
  `{:error/fn (fn [_ _] "…")}`, whose parameter vector is no clear data. The guards of each rule still apply to them.
- A Hiccup vector of keywords only, such as `(def greeting [:p :hello])`, is read as data, and the rules lose its
  findings. Such a vector has no props and no element children, so few rules report it.
- A schema with a part that is not Malli all through stays Hiccup, as today. For example a schema with Hiccup in a
  property value, or with a vector in a schema position whose head is no data type keyword. The guards of each rule
  still apply to it.
- A Garden rule headed by `:*` or `:+` stays Hiccup, as today, so a nested element in it keeps its findings.
- An HTML image map or an SVG `<set>` element that also reads as a Malli schema all through is read as data, and the
  rules lose its findings. For example `[:map {:name "nav"} [:a {:href url} label]]`, whose links have symbol, call or
  keyword labels such as `(tr :home)`, or `[:set {:attributeName "fill" :to "red"}]`. Both elements are rare.
- Keyword-only vectors with tag heads inside a set, a map or a call in a property value, such as
  `[:vector {:a #{[:th :name] [:th :email]}} :int]`, are read as data. Such code is unusual.
- A list of keyword-only vectors whose heads are custom elements or tags that `hiccup/html-tags` leaves out, such as
  `[[:my-cell :name] [:my-cell :email]]`, is read as data, because such a head looks like a db path keyword. The rules
  lose its findings.
- The data `def` rule and the lookup of a symbol's `def` read a `def` the same way, with `#_` forms dropped. So a `def`
  with a trailing `#_` form now resolves to its value, a `def` with a `#_` form before its name, such as
  `(def #_old props {…})`, now resolves at all, and a re-`def` whose only value is a `#_` form no longer hides an
  earlier `def` of the same name. A finding on the value, such as `img-alt-missing`, can appear where today's lookup
  reads the `#_` form and finds nothing.
- A schema outside a vector `def`, such as a call `def` `(mu/closed-schema [:map …])` or a schema in a function body, is
  still read as Hiccup. The fallback covers its `[:label …]` case. A Malli property that `names-a-dom-attr?` reads as a
  DOM attribute, such as `:title`, still makes the entry markup for `label-not-associated` outside a vector `def`.
- The `layout` group keeps its findings in Malli `def`s.
- The guards of each rule can stay. The data `def` rule removes the cause for most cases, and the guards cover the rest.

## Confirmation

Before the status changes to `Accepted`:

- Run the scan again with a prototype. Every finding that goes away must be a false positive.
- The fixtures must show each shape of clear data: a Malli schema with each kind of root, such as `:map`, `:=>` and
  `:enum`, entries with and without properties, a property value that is a schema, each Malli type, a db path, a keyword
  list, a list of schemas, and a value with metadata, inner metadata or a `#_` form.
- The fixtures must show the vectors that stay Hiccup: an HTML element, an image map with text links, a schema with
  `[:img …]` in a schema position, in a property value, or nested in a map, set, list or reader conditional inside a
  property value, a custom element over other markup, `:<>` with empty props, a component with no arguments, props named
  by a symbol, a list of elements, a list of keyword-only elements with tag heads, with and without class shorthand, a
  Garden rule, a Garden rule headed by `:*`, a pair such as `[:title "asc"]`, a path with a number, a single keyword
  such as `[:br]`, a Malli type keyword that is also an HTML tag, and an aliased `::m/schema` head.
- The fixtures must show the values that are read as today: a call, a symbol-headed vector, a reader conditional, a
  value with its own metadata, and a `def` inside a reader conditional.
- The fixtures must show that a map `def` with toast Hiccup, a route table with a `:view` function, an action table with
  a `:render-item` function and an `as-element` argument inside a data `def` stay Hiccup.

## Implementation

Do these steps in separate pull requests:

1. Remove the Malli entry schema skip from [pull request #143](https://github.com/olecve/cljs-patrol/pull/143). Keep its
   other fixes: element shells and local components.
2. Add the test for clear data, for a vector and for a `def`, with its fixtures.
3. Add the data `def` rule in `hiccup/enclosing-forms`, as a new kind `:data-def`. A `fn`, `fn*`, `#(…)` or `as-element`
   call removes `:data-def` from the forms under it. `a11y`, `forbid-props` and `reagent` get the kind through
   `inside-non-hiccup-form?`. `react-testing-library-cljs` checks `:data-def` itself, because it calls only
   `inside-unrendered-form?`.
4. Add the Malli types to the label check.

## Revisit when

- A user reports a false positive from a Malli schema outside a vector `def`, beyond the `[:label …]` case. Then the
  Malli types, or schema roots, are needed for all groups that read Hiccup.
- A user reports a false positive in a data `def` that the test does not know, such as `[:title "asc"]`. Then add that
  shape to the test, if markup never has it, or add a marker.
- A user reports a missing finding in a vector of keywords only. Then the keyword-list shape needs a narrower form.
- A rule must read a vector `def` value as Hiccup, for example a rule for icon `def`s. Then that rule needs a way past
  the data `def` rule.
- cljs-patrol starts to keep `def` references across files for another reason. Then schema roots cost less, and they can
  be compared again.

## More Information

- Pull request #143, element shells, local components and the Malli entry schema skip:
  https://github.com/olecve/cljs-patrol/pull/143
- `hiccup-label?`, `renders-something?` and `names-a-dom-attr?` in `src/cljs_patrol/groups/a11y.clj`
- `re-frame-vector?` in `src/cljs_patrol/groups/forbid_props.clj`
- `hiccup/html-tags`, `hiccup/enclosing-forms` and `hiccup/inside-non-hiccup-form?` in `src/cljs_patrol/hiccup.clj`
- Malli function schemas: https://github.com/metosin/malli/blob/0.20.3/docs/function-schemas.md
- Malli registries: https://github.com/metosin/malli/blob/0.20.3/src/malli/core.cljc
- clj-kondo `:lint-as`: https://github.com/clj-kondo/clj-kondo/blob/v2026.10.26/doc/config.md
