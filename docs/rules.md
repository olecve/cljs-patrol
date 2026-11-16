# What cljs-patrol detects

Every rule, one page per rule group. For the one-line summary of each group, see
[Rule groups](../README.md#rule-groups).

[← back to the README](../README.md)

- **[`re-frame`](rules/re-frame.md)**
- **[`spade`](rules/spade.md)**
- **[`reagent`](rules/reagent.md)**
- **[`typography`](rules/typography.md)**
- **[`focus-ring`](rules/focus-ring.md)**
- **[`forbid-props`](rules/forbid-props.md)**
- **[`a11y`](rules/a11y.md)**
- **[`tests`](rules/tests.md)**
- **[`react-testing-library-cljs`](rules/react-testing-library-cljs.md)**
- **[`docstrings`](rules/docstrings.md)**
- **[`layout`](rules/layout.md)**
- **[`css-order`](rules/css-order.md)**
- **[`idioms`](rules/idioms.md)**
- **[`comments`](rules/comments.md)**

## Data `def`s

The groups that read Hiccup, `a11y`, `forbid-props`, `reagent` and `react-testing-library-cljs`, read the value of a
data `def` as data, not as markup. Other groups, such as `layout`, read it as before.

```clojure
;; data: the groups that read Hiccup skip it
(def tag-props-schema
  [:map {:closed true}
   [:label {:optional true} [:maybe :string]]])

(def settings-path [:settings :style])

;; Hiccup: the groups read it as before
(def logo [:img {:src "logo.png"}])

(def toast {:message "Saved" :description [:img {:src "ok.png"}]})

;; data, with markup under a cut-off: the groups read the [:img :src] as Hiccup
(def item-schema [:vector {:render #(do [:img :src])} :int])
```

A data `def` is a `def` or `defonce` at the top level of the file, with or without metadata on its name or a docstring,
whose value is a vector that is clearly data. Its value is the last form, with `#_` forms dropped. A vector is clearly
data when one of these holds:

- It is a Malli schema all through: its head is a Malli type keyword, then an optional properties map, then children. An
  entry of a `:map`, `:multi`, `:catn`, `:altn`, `:orn` or `:andn` is `[key properties? schema]`. A child of `:enum`,
  `:=` and the other literal types is a literal value: a keyword, a symbol, a string, a number, a boolean, `nil` or a
  regular expression. Every other child is a Malli schema: a keyword, a symbol, a regular expression, a call in which
  every vector is clearly data, or a vector that is a schema all through. Every vector in a key or a value of a
  properties map, at any depth, is clearly data too.
- It has two or more elements, and each element is a keyword, such as the db path `[:settings :style]`.
- Its head is a vector, and each element is clearly data, such as a list of schemas. In such a list, and inside a call
  in a schema position, a vector of keywords whose head is an HTML tag is markup. So `[[:th :name] [:th :email]]` and
  `(def s [:vector (m/schema [:img :src])])` stay Hiccup.

The Malli type keywords that Hiccup or Garden also use, namely `:symbol`, `:>`, `:*` and `:+`, do not make a schema.
`:map` and `:set` do. An aliased `::m/schema` is not resolved, so it does not make a schema, and `:malli.core/schema`
written out does. A `#_` form is no element. Metadata on a vector is looked through, and every vector in that metadata
must be clearly data too. Any other vector is markup, as before: an element with a string, a component, a Garden rule,
or a pair such as `[:title "asc"]`. So is a value with metadata of its own, a map, a call, and a `def` inside another
form or a reader conditional.

The forms under a `#(…)` in the value are markup again, so the `[:img :src]` of `item-schema` keeps its findings. So are
the forms under any form that holds a symbol named `as-element` anywhere inside it, from any namespace: as a head, a
thread step such as `(-> [:img :src] r/as-element)`, an argument such as `(apply r/as-element …)`, or inside a reader
conditional or a nested call. A symbol inside a `#_` form does not count. The name is not resolved, because a `.cljc`
file can require Reagent inside a reader conditional. A `fn` cannot be in a data `def`, because its parameter vector is
not clearly data.

A Hiccup vector of keywords only, such as `(def greeting [:p :hello])`, is read as data, and the rules lose its
findings. An HTML image map or an SVG `<set>` that is also a Malli schema all through, such as
`[:map {:name "nav"} [:a {:href url} label]]` or `[:set {:attributeName "fill" :to "red"}]`, is data too. To keep such
markup checked, put metadata on the value, such as `(def greeting ^:hiccup [:p :hello])`, or write it in a function.

A Malli schema that one part keeps from reading as data gets a note on its findings. The note names that part and asks
to move it into its own `def` or function. Then the schema reads as data, and the findings go away:

```clojure
;; a finding on the :label entry, with a note that names (fn [_ _] "Too long")
(def title-schema
  [:map {:error/fn (fn [_ _] "Too long")}
   [:label {:title "Name"} :string]])

;; the schema reads as data
(defn too-long [_ _] "Too long")

(def title-schema
  [:map {:error/fn too-long}
   [:label {:title "Name"} :string]])
```

The note is given only when the `def` is at the top level of the file, its value is a vector with no metadata of its
own, exactly one part keeps the value from reading as data, and a symbol in place of that part makes it read as data. So
the advice works: the value must also hold no `#(…)` and no `as-element`, which end the data `def` rule, or no note is
given. That part can be a key or a value in a properties map, a form in a schema position that is not a schema, or a
form among the values of `:enum`, `:=` or another literal type that is not a literal. A part that a symbol cannot
replace, such as a whole entry with a computed key, gets no note. A finding inside that part gets no note, because the
part stays markup. The `a11y` and `forbid-props` groups add the note to their findings in the schema, and `reagent` adds
it to its fragment findings. The `react-testing-library-cljs` group adds it to a `test-id-query-on-role` finding when
the one element it counts sits in such a schema.

## Experimental: reading `.clj` files

The tool reads `.cljs` and `.cljc`. Five of its groups are not about ClojureScript at all: `docstrings` enforces the
bbatsov style guide, `tests` enforces conventions on `clojure.test` code, `layout` reports what cljfmt's keypair split
leaves behind, `idioms` reports code a core idiom says more plainly, and `comments` reports code left in comments. All
five hold on `.clj` just as well, so `--experimental-clj` lets them read those files too:

```bash
clojure -M:run --experimental-clj --only docstrings,tests,layout,idioms,comments src test
```

Or in `.cljs-patrol/config.edn`:

```clojure
{:experimental-clj true}
```

The flag widens discovery and nothing else. Every group that reads Hiccup, re-frame or Spade keeps to `.cljs`/`.cljc`,
so a `.clj` file in the tree is read by `docstrings`, `tests`, `layout`, `idioms` and `comments` and skipped by the rest
even with all groups enabled.

It is marked experimental because the rules were written against ClojureScript code and their wording still assumes it
in places: `js-hint-in-test` is about `^js` externs inference, which means nothing in Clojure, and `var-deref-in-test`
flags `@#'private-fn`, which is a more established idiom on the JVM than it is in ClojureScript. Both are `:cleanup`, so
neither blocks CI unless `--fail-on` names it. Narrow the run with `--only` or snapshot what is there with
[`--baseline`](baseline.md) if a rule does not earn its keep on a particular tree.
