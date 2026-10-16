# What cljs-patrol detects

Every rule, one page per rule group. For the one-line summary of each group, see
[Rule groups](../README.md#rule-groups).

[← back to the README](../README.md)

- **[`re-frame`](rules/re-frame.md)**
- **[`spade`](rules/spade.md)**
- **[`reagent`](rules/reagent.md)**
- **[`typography`](rules/typography.md)**
- **[`a11y`](rules/a11y.md)**
- **[`tests`](rules/tests.md)**
- **[`docstrings`](rules/docstrings.md)**
- **[`layout`](rules/layout.md)**
- **[`css-order`](rules/css-order.md)**

## Experimental: reading `.clj` files

The tool reads `.cljs` and `.cljc`. Three of its groups are not about ClojureScript at all: `docstrings` enforces the
bbatsov style guide, `tests` enforces conventions on `clojure.test` code, and `layout` reports what cljfmt's keypair
split leaves behind. All three hold on `.clj` just as well, so `--experimental-clj` lets them read those files too:

```bash
clojure -M:run --experimental-clj --only docstrings,tests,layout src test
```

Or in `.cljs-patrol/config.edn`:

```clojure
{:experimental-clj true}
```

The flag widens discovery and nothing else. Every group that reads Hiccup, re-frame or Spade keeps to `.cljs`/`.cljc`,
so a `.clj` file in the tree is read by `docstrings`, `tests` and `layout` and skipped by the rest even with all groups
enabled.

It is marked experimental because the rules were written against ClojureScript code and their wording still assumes it
in places: `js-hint-in-test` is about `^js` externs inference, which means nothing in Clojure, and `var-deref-in-test`
flags `@#'private-fn`, which is a more established idiom on the JVM than it is in ClojureScript. Both are `:cleanup`, so
neither blocks CI unless `--fail-on` names it. Narrow the run with `--only` or snapshot what is there with
[`--baseline`](baseline.md) if a rule does not earn its keep on a particular tree.
