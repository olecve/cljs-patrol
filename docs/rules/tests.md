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
;; an article inside the name is fine
(deftest escape-closes-the-modal
  (is (closed? (modal))))
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
;; the public fn that calls parse-count
(deftest parses
  (is (= 1 (app.core/read-count "1"))))
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

`js-hint-in-test` · cleanup · config [`:paths`](#test-paths) — A `^js` hint in test code on a value that no interop call
reads.

```clojure
;; flagged
(deftest chart-renders
  (let [^js el (render-chart)]
    (is (some? el))))

;; fix
(deftest chart-renders
  (let [el (render-chart)]
    (is (some? el))))
```

**Left alone:**

```clojure
;; left alone: .getObjects reads canvas, so the hint stops an :infer-warning
(deftest objects
  (let [^js canvas (load-canvas)]
    (is (= 4 (count (.getObjects canvas))))))

;; left alone: inline on the target of the interop call
(deftest first-object
  (is (some? (first (.getObjects ^js (load-canvas))))))

;; left alone: threaded into a property read
(deftest pending-op
  (let [^js el (render-chart)]
    (is (= "2" (-> el .-dataset .-pendingOp)))))

;; left alone: a hint on what a helper returns, which its callers read
(defn- ^js load-canvas [] (render-chart))
```

<details>
<summary>Details</summary>

A `^js` type hint tells shadow-cljs that a value is a JS object. shadow-cljs reads the hint in every build, not only
under `:advanced`: when an interop call reads a value it cannot type, it prints an `:infer-warning` ("Cannot infer
target type"), in a test build too. So a hint on a value that an interop call reads can keep the build log clean, and
the rule leaves it. A hint on a value that nothing reads through interop changes nothing, and the rule flags it.

What counts as read:

- The hinted value is the target of `(.f x)`, `(.-p x)`, `(.. x -p)` or `(. x f)`, also inside a `#(…)` literal.
- The hinted value is threaded into an interop step: the next step of `->` or `some->`, a bare `.-p` or `.f` next step
  of `->>` or `some->>`, any step of `doto` or `cond->`, a bare interop step of `cond->>`, or the name of `as->` when an
  interop call reads it.
- The hint is on an interop call itself, `^js (.-value x)`, which types what the call returns.
- A hint on a bound name — a `let`, `loop` or `doseq` binding, a parameter of `fn`, `defn`, `reify` or `deftype`, a
  `deftype` field, destructuring, or `this-as` — when an interop call reads that name anywhere in the top-level form, or
  reads a local that a `let`-like binding binds to it.
- A hint on the name a `def` or `defn` defines, or on a `defn` arglist (single or multi-arity), in any order with other
  metadata. Callers read it as the type of what a call returns. A `defmethod` or inline `fn` arglist is not one.

Reads in code that never compiles do not count: inside `#_`, a quoted form or `(comment …)`. Names compare as written,
so `other/el` is not the local `el`. Shadowing is not tracked: an inner binding of the same name that an interop call
reads keeps an outer hint. Any other inline hint, such as one on a plain call argument, is flagged unless its own value
is the target.

The rule cannot tell when shadow-cljs would print no warning anyway. It skips the warning for property names it knows
from its externs, such as the DOM's `width`, `textContent`, `value` or `focus`. So a hint read only through such a name
changes nothing, but the rule leaves it, because it reads no externs.

Both spellings count — `^js` and `^{:tag js}`. Left alone: `^js/Foo`, which names a type rather than asking for
inference, and `^clj`, `^boolean`, `^:private` and every other tag; the symbol `js` anywhere but the tag slot, so
`^:private js` and `^{:doc js}` are not hints; and anything inside a quoted or discarded form. Only test code is read:
in source, the rule leaves every hint alone.

</details>

### Literal constant assertion

`literal-constant-assertion` · cleanup — An assertion comparing a string literal with the constant defined as that same
string.

```clojure
;; flagged
(def file-failed-message "We couldn't read this file")

(deftest file-failed-copy
  (is (= "We couldn't read this file" file-failed-message)))

;; fix
(def file-failed-message "We couldn't read this file")

(defn toast-for [{:keys [files]}] {:message (when (seq files) file-failed-message)})

(deftest upload-failure-fires-file-reading-toast
  (is (= file-failed-message (:message (toast-for {:files [:report]})))
      "file-reading heading when submit had files"))
```

**Left alone:**

```clojure
;; a fn's output
(defn describe [title] (str "Search case " (or title "Untitled")))

(deftest describe-falls-back-to-untitled
  (is (= "Search case Untitled" (describe nil))))

;; a computed constant
(def completed-message (str "Smart Search " "completed"))

(deftest completed-copy
  (is (= "Smart Search completed" completed-message)))

;; a local may shadow the constant
(def label "View Results")

(deftest label-copy
  (let [label "View Results"]
    (is (= "View Results" label))))
```

<details>
<summary>Details</summary>

An `(is (= "…" constant))` in test code, in either order and with or without a message, where the constant is a
top-level `(def name "…")` of that same string anywhere in the run, source or test directory. It fails on every edit to
the copy and on nothing else, and the fix is always to update both files, so it records the definition rather than
testing anything. Compare against the constant where the test is about which message the code picks, and assert the
literal where it is about what the user reads, such as `(s/get-by-text "Save")` in a render test. A message does not
exempt the assertion: a deliberate pin on a storage key or an event name repeats its definition all the same, and the
place to record the agreed value is the review that changes it.

A qualified symbol is read through the namespace's aliases, or as a namespace name when no alias matches; an unqualified
one through `:refer`, or as the file's own namespace. The symbol counts only when its name appears nowhere else in its
top-level form, as a symbol or a keyword, qualified or not, since a binding, a `with-redefs` or a `binding` that could
change its value writes the name a second time.

Left alone:

- a constant built by an expression, or a `goog-define` (a build can override its value)
- a `def` behind a reader conditional, a var defined as two different strings, and files without an `ns` form
- a comparison of more than two values, and an `=` not directly inside `is`
- anything in a `comment` block or a quoted or discarded form, and assertions outside [test code](#test-paths)

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
