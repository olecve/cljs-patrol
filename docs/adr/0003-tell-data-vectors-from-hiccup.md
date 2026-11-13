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

Read a vector `def` as data when its content shows no sign of markup. Keep a Malli type check in `label-not-associated`
as a fallback for schemas outside vector `def`s.

The parts of the decision:

1. **A test tells clear markup.** The test leans toward markup: a vector it reads as markup keeps today's findings, and
   a vector it misses loses them without a sign. A vector is clearly markup when one of these holds:
   - Its head is an HTML tag keyword, with or without class and id shorthand such as `:div.card`, or `:<>`, `:>`, `:f>`
     or `:r>`, and it shows a sign of markup.
   - Its head is a symbol, a component, and it shows a sign of markup.
   - Its head is a vector, and one of its elements, the head included, is clearly markup. A list of Hiccup elements,
     such as `[[:li "a"] [:li "b"]]`, is thus markup, and so is a list of pairs such as `[[:rect "Step"]]`.

   The signs of markup:
   - A string or a number child.
   - A props map with a key that is not a CSS property and not a Malli property key. Unknown keys count as markup, so
     `[:img {:src "logo.png"}]` and `[:svg {:viewBox "0 0 16 16"}]` are markup. The props map is the element after the
     head, or after the component for `:>`, `:f>` and `:r>`. A `#js` map counts, and `#_` forms inside the map are
     skipped. A key that is no keyword or string, such as a symbol or a call, shows no sign. The CSS properties are the
     ones in the `css-order` property tables, without the names that are also element attributes: `src`, which the
     tables list as a `@font-face` descriptor, and the SVG geometry `d`, `r`, `cx`, `cy`, `rx` and `ry`. The Malli
     property keys are `:optional`, `:description`, `:default`, `:title`, `:min`, `:max`, `:closed`, `:registry` and
     `:doc`, and the keys in the `gen`, `error`, `json-schema`, `decode`, `encode`, `swagger` and `openapi` namespaces.
   - A child vector that is clearly markup, written as a child or as what a child form yields: the forms that
     `hiccup/result-locs` reads, such as `when`, `if`, `cond`, `case`, `let`, `when-let`, `do` and `binding`, the body
     of a `for`, and each branch of a `#?` or `#?@` reader conditional. Only a yielded vector counts, so a string
     spliced by `#?@(:cljs ["a"])` shows no sign.

   A `#_` form is no element, and metadata on a vector is looked through. A nested tag with only CSS property maps, such
   as a Garden rule `[:div.paragraph [:br {:margin-top "5px"}]]`, shows no sign.

   A `def` is a Garden rule, and never clearly markup, when a style-declaration macro in the same file uses its name as
   a value. The macros are the ones in `hiccup/style-decl-forms`: `defclass`, `defattrs`, `defglobal`, `defkeyframes`
   and the others. A parameter, a binding or a call head of the same name is no use. A vector of symbols reads as a
   parameter list, so a style macro that takes a vector of symbols as a value does not mark the `def`, and the `def`
   stays markup. One predicate answers both questions for a `def`, so the data `def` rule cannot use one without the
   other.

2. **The data `def` rule.** For `a11y`, `forbid-props`, `reagent` and `react-testing-library-cljs`, the value of a
   top-level `def` is data, with the vectors nested in it, when the value is a vector with a keyword head or a vector
   head, and it is not clearly markup.
   - A `def` is `def` or `defonce`, with or without metadata on the name or a docstring. Its value is the last form. A
     value with metadata, a map, a call, a symbol-headed vector or a reader conditional is read as today.
   - A `fn`, `fn*` or `#(…)` removes the data `def` rule from the forms under it, so a route view stays Hiccup. So does
     the argument of `reagent.core/as-element`, resolved through aliases.
   - Other groups, such as `layout`, read all `def` values as today.

3. **The label-check fallback.** In `hiccup-label?`, a nested vector whose head is a Malli core type keyword does not
   make the label markup. The types are the keyword types of `malli.core/base-schemas`, `type-schemas`,
   `sequence-schemas` and `comparator-schemas`, without `:>`.

[Options](#options) compares this choice with the other options.

## Options

### A vector `def` without clear markup is data (chosen, with the label-check fallback)

The test for clear markup and the data `def` rule above, with the label-check fallback.

- Good, because one rule covers Malli schemas, db paths and keyword lists, in the groups that read Hiccup.
- Good, because a `def` that holds Hiccup stays valid code and stays checked.
- Good, because in a prototype on the scan, the test found every Hiccup `def`. The recursive nested sign keeps Garden
  rules out.
- Good, because unknown props keys count as markup, so a new HTML or SVG attribute never turns Hiccup into data.
- Good, because it needs no references across files and no change to the code under check.
- Bad, because a `def` with markup that the test does not see, such as `[:div (t :hello)]` or
  `[:input {:min 0 :max 10}]`, loses its findings without a sign.
- Bad, because the test needs two lists to stay right: the CSS properties and the Malli property keys.
- Bad, because a data pair with a string child, such as `(def default-sort [:title "asc"])`, stays Hiccup.
- Bad, because test fixtures and color lists have map or string heads, so they stay Hiccup.
- Bad, because the data `def` rule does not cover schemas outside vector `def`s. Only the fallback covers their
  `[:label …]` case.

### Hiccup only in functions, enforced by a rule (rejected)

A new rule, `hiccup-in-def`, reports a vector `def` that is clearly markup. The fix is a component function. Then every
vector `def` could be data.

- Good, because the classification of `def`s becomes simple: every vector `def` is data.
- Good, because it is close to JSX, where markup is syntax and eslint-plugin-react never has to guess.
- Bad, because a `def` is a valid place for Hiccup, as a `defn` and a `defc` are. The rule makes users change valid code
  to help the linter.
- Bad, because it adds no safety: the test for clear markup already keeps a Hiccup `def` checked.
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
- Bad, because the classification does not need it, because a schema is not clearly markup.
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
- A data `def` with a string child and a tag head, such as `(def default-sort [:title "asc"])` or
  `(def default-shape [:rect "Step"])`, is clearly markup. The groups keep reading it as Hiccup, as today. The scan has
  none at the top level.
- A list of pairs, such as `(def shapes [[:rect "Step"] [:line "Line"]])`, is clearly markup by the vector-head sign.
  The groups keep reading it as Hiccup, as today.
- A vector `def` with markup that the test does not see as clear is read as data, and the rules lose its findings. For
  example `[:div (t :hello)]`, with only calls as children, or `[:input {:min 0 :max 10}]` and
  `[:span {:title "Help"}]`, whose props keys are all Malli property keys.
- A data vector with a props map of keys that are neither CSS properties nor Malli property keys stays Hiccup, as today.
- An SVG element whose props keys are all CSS properties, such as `[:rect {:width 1 :height 1 :fill "red"}]`, shows no
  sign, because `width`, `height`, `fill` and `stroke` are CSS properties too. It is markup only when it has another
  sign.
- A schema outside a vector `def`, such as a call `def` `(mu/closed-schema [:map …])` or a schema in a function body, is
  still read as Hiccup. The fallback covers its `[:label …]` case. A Malli property that `names-a-dom-attr?` reads as a
  DOM attribute, such as `:title`, still makes the entry markup for `label-not-associated` outside a vector `def`.
- The `layout` group keeps its findings in Malli `def`s.
- The guards of each rule can stay. The data `def` rule removes the cause for most cases, and the guards cover the rest.

## Confirmation

Before the status changes to `Accepted`:

- Run the scan again with a prototype. The test for clear markup must find every Hiccup `def`, and every finding that
  goes away must be a false positive.
- The fixtures must show each sign of clear markup: a string child, a number child, a props map with an HTML or SVG
  attribute such as `:src` or `:viewBox`, a `#js` props map, a props map with a `#_` form inside, a nested component
  with a sign, and a nested vector yielded by a `when`, a `for` or a reader conditional. They must show `:>` and `:<>`
  roots, a list of Hiccup elements and a list of pairs.
- The fixtures must show the look-alikes: a db path, a keyword list, a Malli schema, a Garden rule in a `def` that a
  `defclass` or a `defglobal` uses, and a Hiccup `def` whose name is only a parameter of a `defclass`, which stays
  Hiccup.
- The fixtures must show that a map `def` with toast Hiccup, a route table with a `:view` function, an action table with
  a `:render-item` function and an `as-element` argument inside a data `def` stay Hiccup.

## Implementation

Do these steps in separate pull requests:

1. Remove the Malli entry schema skip from [pull request #143](https://github.com/olecve/cljs-patrol/pull/143). Keep its
   other fixes: element shells and local components.
2. Add the test for clear markup and the Garden-rule check, behind one predicate for a `def`, with their fixtures.
3. Add the data `def` rule in `hiccup/enclosing-forms`, as a new kind `:data-def`. A `fn`, `fn*`, `#(…)` or `as-element`
   call removes `:data-def` from the forms under it. `a11y`, `forbid-props` and `reagent` get the kind through
   `inside-non-hiccup-form?`. `react-testing-library-cljs` checks `:data-def` itself, because it calls only
   `inside-unrendered-form?`.
4. Add the Malli types to the label check.

## Revisit when

- A user reports a false positive from a Malli schema outside a vector `def`, beyond the `[:label …]` case. Then the
  Malli types, or schema roots, are needed for all groups that read Hiccup.
- A user reports a missing finding in a vector `def` whose children are calls. Then add calls as a sign, or add a
  marker.
- A user reports a finding on data that the test reads as clear markup, such as `[:title "asc"]`. Then the test needs a
  stronger sign, or a marker for that case.
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
