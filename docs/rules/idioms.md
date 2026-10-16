# Idioms rules

The rules of the `idioms` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

- **Nested call chain** (`nested-call-chain`, cleanup) — three or more calls nested one inside another, each taking the
  one before as its only argument: `(vec (sort (distinct ids)))`. The reader starts at the innermost form and works
  outwards; a thread reads top to bottom in the order the calls run. The finding spells out the rewrite,
  `(-> ids distinct sort vec)`.

  It reports only what it can rewrite without a judgement call:
  - **Every step takes one argument.** `->` and `->>` then produce the same form, so the rewrite cannot thread into the
    wrong slot. A call with two or more arguments ends the chain, `(vec (sort (map f xs)))` included, and so does a step
    holding a comment or a `#_` form
  - **Every head is known to be a function** — a keyword, interop (`.method`, `.-field`, `Ctor.`, `js/f`,
    `Class/static`), one of a fixed list of `clojure.core` / `cljs.core` and `clojure.string` functions, or a var the
    project defines with `defn`, `defn-` or `defmulti`, reached unqualified, through an alias or through `:refer`. A
    macro takes its arguments unevaluated, so threading into one can change what it does; a local, a library var the
    scan cannot read and a name the namespace redefines all end the chain

  Left alone as well: a `not`, `boolean`, `some?`, `nil?`, `seq`, `empty?`, `not-empty`, `true?` or `false?` wrapping
  the whole chain, which is not counted, since a test reads worse at the end of a thread; a call on a literal, which
  stays the thread's starting value, so `(:question (:params (request-to "/qa")))` is two steps above
  `(request-to "/qa")` rather than three; a step of a `->`, `doto` or other threading form, which takes an argument it
  does not write out; the list a reader conditional holds; and anything quoted, discarded or inside `(comment …)`

## Idioms options

The `idioms` group reads one key from `.cljs-patrol/config.edn`:

```clojure
{:idioms {:min-calls 3}}
```

The shortest chain reported, counted after a wrapper and a call on a literal are set aside. The default is 3, the
threshold most style guides give for reaching for a thread; anything below 2 falls back to it.
