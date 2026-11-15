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

(def item-schema [:vector {:render #(do [:img :src])} :int])
```

A data `def` is a `def` or `defonce` at the top level of the file, with or without metadata on its name or a docstring,
whose value is a vector that is clearly data. Its value is the last form, with `#_` forms dropped. A vector is clearly
data when one of these holds:

- It is a Malli schema all through: its head is a Malli type keyword, then an optional properties map, then children. An
  entry of a `:map`, `:multi`, `:catn`, `:altn`, `:orn` or `:andn` is `[key properties? schema]`. A child of `:enum`,
  `:=` and the other comparators is a literal value. Every other child is a Malli schema: a keyword, a symbol, a regular
  expression, a call in which every vector is clearly data, or a vector that is a schema all through. Every vector in a
  properties map is clearly data too.
- It has two or more elements, and each element is a keyword, such as the db path `[:settings :style]`.
- Its head is a vector, and each element is clearly data, such as a list of schemas. In such a list, a vector of
  keywords whose head is an HTML tag, as in `[[:th :name] [:th :email]]`, is markup.

The Malli type keywords that Hiccup or Garden also use, such as `:symbol`, `:>`, `:*` and `:+`, do not make a schema.
Any other vector is markup, as before: an element with a string, a component, a Garden rule, or a pair such as
`[:title "asc"]`. So is a value with metadata of its own, a map, a call, and a `def` inside another form or a reader
conditional.

The forms under a `#(…)` or a `reagent.core/as-element` call in the value are markup again, so the `[:img :src]` of
`item-schema` keeps its findings. A `fn` cannot be in a data `def`, because its parameter vector is not clearly data.

A Hiccup vector of keywords only, such as `(def greeting [:p :hello])`, is read as data, and the rules lose its
findings. An HTML image map that is also a Malli schema all through, such as
`[:map {:name "nav"} [:a {:href url} label]]`, is data too.

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
