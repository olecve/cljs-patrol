# Idioms rules

The rules of the `idioms` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

- **Nested call chain** (`nested-call-chain`, cleanup) — three or more calls nested one inside another, each taking the
  one before as its only argument: `(vec (sort (distinct ids)))`. The reader starts at the innermost form and works
  outwards; a thread reads top to bottom in the order the calls run. The finding spells out the rewrite,
  `(-> ids distinct sort vec)`, with the starting value exactly as written. A `#(…)` is read the same way and threads to
  `#(-> % distinct sort vec)`.

  It reports only what it can rewrite without a judgement call:
  - **Every step takes one argument, written out.** `->` and `->>` then produce the same form, and the rewrite expands
    back to exactly the code that was written. A call with two or more arguments ends the chain,
    `(vec (sort (map f xs)))` included, and so does a call holding a comment or a `#_` form. So does a step of `->`,
    `->>`, `some->`, `some->>`, `doto` or `..`, and the step of a `cond->` / `cond->>` pair, which all take an argument
    they do not write out; an `as->` step and a `cond->` test do write theirs, and count
  - **Every head is known to be a function** — a keyword, interop (`.method`, `.-field`, `Ctor.`, `js/f`,
    `Class/static`), one of a fixed list of `clojure.core` / `cljs.core` and `clojure.string` functions written plain,
    through an alias or through `:refer`, or a var the project defines with `defn`, `defn-` or `defmulti`. Threading
    cannot change what a macro does, but it changes how it reads: `(-> xs sort vec delay)` looks like four steps run in
    order while the `delay` defers the other three. So a local, a library var the scan cannot read and a name the
    namespace defines any other way all end the chain. A project var counts only when it is defined under the same
    source directory the run reads, since each directory passed on the command line is analyzed on its own

  Left alone as well: a `not`, `boolean`, `some?`, `nil?`, `seq`, `empty?`, `not-empty`, `true?` or `false?` at the top
  of what would be threaded, which is not counted, since a test reads worse at the end of a thread, so
  `(is (nil? (:id (first xs))))` is not reported; a call on a literal, which stays the thread's starting value, so
  `(:question (:params (request-to "/qa")))` is two steps above `(request-to "/qa")` rather than three; the list a
  reader conditional holds; and anything quoted, discarded or inside `(comment …)`

## Idioms options

The `idioms` group reads one key from `.cljs-patrol/config.edn`:

```clojure
{:idioms {:min-calls 3}}
```

The shortest chain reported, counted after a wrapper and a call on a literal are set aside. The default is 3, the
threshold most style guides give for reaching for a thread; anything below 2 falls back to it.
