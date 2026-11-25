# Color rules

[All rule groups](../rules.md) · [Back to the README](../../README.md)

## Color literal

`color-literal` · cleanup · config [`:allowed-namespaces`](#colors-options): a color written as a literal outside the
design-token namespaces.

```clojure
;; config {:colors {:allowed-namespaces #{app.styles.tokens}}}

;; flagged
(def warning-color "#f5a623")

;; a Hiccup attribute
[:circle {:fill "#d7dffe" :r 4}]

;; interop
(set! (.-fillStyle ctx) "#fff")

;; a data map
(def chart-options {:series [{:color "rgba(0, 38, 255, 0.4)"}]})

;; a nested selector
(defclass row-style []
  [:&:hover {:background "#f3f3f3"}])

;; a color inside a larger value
(defclass card-style []
  {:border "1px solid #ddd"})

;; fix
(def warning-color tokens/warning-500)
```

**Left alone:**

```clojure
;; config {:colors {:allowed-namespaces #{app.styles.tokens}}}

;; left alone: an allowed namespace
(ns app.styles.tokens)
(def primary-500 "#0026ff")

;; a docstring
(defn tint
  "Mixes a color such as #0026ff with white."
  [color amount]
  (mix color tokens/global-white amount))

;; an anchor that is not only hex digits
[:a {:href "#main-content"} "Skip to content"]

;; an HTML entity
(str/replace markup "&nbsp;" "&#160;")

;; a named color, not read in v1
(defclass overlay-style []
  {:background "transparent"})

;; code in a comment form
(comment
  (draw! ctx "#ff0000"))
```

A finding names the token when one has the same value, such as "Use app.styles.tokens/global-white." for `"#FFF"`. When
no token has the value, it says "Use a token from app.styles.tokens."

## Colors options

Set `:colors` to turn on this group. Run it alone with `--only colors`.

```clojure
{:colors {:allowed-namespaces #{app.styles.tokens}}}
```

`:allowed-namespaces` must be a set of namespace symbols. Each allowed namespace may hold color literals. The group
reads `.cljs` and `.cljc` files. It skips test files, docstrings, comments, discarded forms, and namespace forms. Test
paths use `:tests :paths`. Without test paths, a namespace that ends in `-test` is test code.

The rule reads hex colors with 3, 4, 6, or 8 digits. A hex color must start the string or follow a space, `(`, or `,`.
It must end the string or precede a space, `)`, `,`, or `;`. The rule also reads `rgb(`, `rgba(`, `hsl(`, and `hsla(` in
any case. Named colors pass. An anchor such as `"#add"` and a reference such as `"url(#beef)"` are reported.

The hint reads exact hex values from `(def name "#hex")` forms in allowed namespaces within the source paths. It
compares values in lower case and expands short hex values. Alpha stays significant. When several tokens match, the hint
names all of them in sorted order. A token that holds `var(--...)` gives no hex match.

Use `--baseline-write` to record current findings. Then use `--baseline` to block new literals. A baseline identity uses
the file, top-level name, and literal text. Equal literals in one top-level form share an identity. A literal that must
stay needs `--baseline-write --allow-new`. See [Baseline](../baseline.md#setup).
