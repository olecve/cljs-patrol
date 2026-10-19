# Tests rules

The rules of the `tests` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only tests`. Its settings live under `:tests` in `.cljs-patrol/config.edn`; see
[Test paths](#test-paths).

## Rules

### `deftest` leading article

`deftest-leading-article` · cleanup · config [`:paths`](#test-paths) — A test name opening with `a-`, `an-` or `the-`.

```clojure
;; flagged
(deftest the-panel-opens
  (is (open? (panel))))

;; fix
(deftest panel-opens
  (is (open? (panel))))
```

**Left alone:**

```clojure
(deftest escape-closes-the-modal
  (is (closed? (modal)))) ; an article inside the name is fine
```

<details>
<summary>Details</summary>

A test var whose name opens with `a-`, `an-` or `the-`. A test named after the claim it makes is a sentence, and the
article at the front of one carries nothing an identifier needs: the reader is scanning for the subject, and the first
word is not it. `the-status-label-is-rendered` becomes `status-label-is-rendered`. Only the front is flagged — an
article inside the name is grammar, so `escape-closes-the-modal` and `without-on-select-the-row-is-not-a-button` are
left alone, and a word that merely opens with those letters (`another-claim-holds`, `theme-switches-to-dark`) is not an
article: the hyphen is what tells them apart. The name is read through metadata on the var, so
`(deftest ^:async the-panel-loads-lazily …)` counts. Point the tool at the test directory to use this group. On a
codebase that predates the convention, `--baseline-write` snapshots what is there so only new names are reported.

</details>

### Assertion message inline

`assertion-message-inline` · cleanup · config [`:paths`](#test-paths) — An assertion message on the same line as what it
checks.

```clojure
;; flagged
(deftest totals
  (is (= 4 (total [2 2])) "two items add up"))

;; fix
(deftest totals
  (is (= 4 (total [2 2]))
      "two items add up"))
```

<details>
<summary>Details</summary>

An `(is …)` whose message shares a line with the expression it describes. The two say different things — one is what
ran, the other why the answer matters — and a reader scanning a test body for either has to read past the other to find
it. Put the message on its own line under the expression, which is how the style guide's own examples are written. The
closing line of a multi-line expression counts as sharing, so `(is (= 4\n (+ 2 2)) "…")` is flagged while the same
message on the next line is not. Only a string written out is flagged; a message named by a symbol is not the shape the
convention is about, and an assertion with no message says nothing either way.

</details>

### Conditional assertion

`conditional-assertion` · cleanup · config [`:paths`](#test-paths) — A test that works out what to expect while it runs.

```clojure
;; flagged
(deftest owner-label
  (doseq [owner? [true false]]
    (is (= (if owner? "You" "Them") (label owner?)))))

;; fix
(deftest owner-label
  (doseq [[owner? expected] [[true "You"] [false "Them"]]]
    (is (= expected (label owner?)))))
```

<details>
<summary>Details</summary>

A test that decides what to assert while it runs. Three shapes, one defect: the test does not state what it expects, it
works it out.

- **The expected value is computed** — `(is (= actual (if owner? false true)))` re-derives the answer, so a mistake
  shared with the code under test passes unnoticed, and a reader cannot see the expectation without simulating it
- **A conditional picks between assertions** — `(if owner? (is …) (is …))`, the same thing spread over two branches
- **A guard can skip the assertion** — `(when title-link (is …))`. When it does not hold the test runs no assertion at
  all and reports success having checked nothing

Write a table instead: pair each input with the value it should produce and walk it with `doseq`, so every case states
its own expectation. A conditional inside a function literal is left alone — `#(when (= 1 (:id %)) %)` handed to `some`
branches per element of a collection, not per assertion. Reported rather than blocking, because the guard shape has
defensible uses: a `when` narrowing a `doseq` to the combination under test reads as deliberate. Enforce it with
`--fail-on` where a project wants it.

</details>

### Var deref in test

`var-deref-in-test` · cleanup · config [`:paths`](#test-paths) — A test reaching a var through its var quote.

```clojure
;; flagged
(deftest parses
  (is (= 1 (@#'app.core/parse-count "1"))))

;; fix
(deftest parses
  (is (= 1 (app.core/read-count "1")))) ; the public fn that calls parse-count
```

<details>
<summary>Details</summary>

A test reading a var through its var quote: `@#'ns/x`, `(deref #'ns/x)`, `@(var ns/x)` or `(deref (var ns/x))`, with
metadata on either part read through. Reaching for the var object rather than calling the thing couples the test to how
the namespace is put together, and a var that is `^:private` is reached this way precisely because it was not meant to
be. Call it through the public entry point that uses it, or move it somewhere it can be public.

Only in a namespace whose name ends in `-test`. Every other rule in this group is anchored to a test construct by its
shape; a var deref is legal anywhere, so without an anchor the rule reports production code — and the group runs by
default. The anchor is the namespace rather than an enclosing `deftest`, because the reads that matter sit in a fixture
or a helper beside the tests as often as inside one. Left alone: a var quote that is **not** dereferenced, which is how
`with-redefs-fn` and `use-fixtures` take one; a deref of anything that is not a var quote; a call to another function
whose name merely ends in `deref`, since the head is compared in full; and anything inside a quoted or discarded form,
written out or in reader syntax.

Privacy itself is not checked — nothing here resolves a var across namespaces — so a public var read this way is
reported too. That is deliberate: it is a var deref whichever it is, and the fix is the same.

</details>

### `^js` hint in test

`js-hint-in-test` · cleanup · config [`:paths`](#test-paths) — A `^js` hint in test code, where nothing reads it.

```clojure
;; flagged
(deftest width
  (let [^js el (render-chart)]
    (is (= 10 (.-width el)))))

;; fix
(deftest width
  (let [el (render-chart)]
    (is (= 10 (.-width el)))))
```

<details>
<summary>Details</summary>

A `^js` type hint written in test code. The hint exists so shadow-cljs infers an extern and the Closure compiler leaves
a JS interop property name alone under `:advanced` optimizations (shadow-cljs User's Guide, 14.2.1 _Externs Inference_).
A test build never runs `:advanced`, so nothing reads the hint and nothing would change if it were not there. Remove it.

Both spellings count — `^js` and `^{:tag js}` — wherever they are written: a `defn` or `fn` parameter, a `let`, `loop`
or `doseq` binding, map or vector destructuring, or inline on an expression (`(.-textContent ^js %)`). Left alone:
`^js/Foo`, which names a type rather than asking for inference, and `^clj`, `^boolean`, `^:private` and every other tag;
the symbol `js` anywhere but the tag slot, so `^:private js` and `^{:doc js}` are not hints; and anything inside a
quoted or discarded form.

</details>

## Test paths

The `tests` group only reads test code: its rules are anchored so that pointing the tool at a source tree reports
nothing, since the group runs by default. Out of the box the anchor is the namespace name — one ending in `-test` is
taken as test code. Where that convention does not hold, name the directories instead, in `.cljs-patrol/config.edn`:

```clojure
{:tests {:paths ["test" "spec"]}}
```

A file under any of them is test code whatever its namespace is called, and a configured path replaces the naming
convention rather than adding to it. Paths match a whole segment at a time, so `test` does not match `src/latest/`, and
a path written as several segments (`src/spec`) matches that run wherever it appears. One path may be written on its own
rather than in a vector.
