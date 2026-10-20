# Idioms rules

The rules of the `idioms` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only idioms`. Its settings live under `:idioms` in `.cljs-patrol/config.edn`; see
[Idioms options](#idioms-options).

## Rules

### Nested call chain

`nested-call-chain` · cleanup · config [`:min-calls`](#idioms-options) — Nested calls a thread would read top to bottom.

```clojure
;; flagged
(defn active-users [rows]
  (vec (filter active? (map :user rows))))

(defn top-result [response]
  (select-keys (first (get-in response [:body :results])) [:id :title]))

;; fix
(defn active-users [rows]
  (->> rows (map :user) (filter active?) vec))

(defn top-result [response]
  (-> response (get-in [:body :results]) first (select-keys [:id :title])))
```

**Left alone:**

```clojure
(defn sorted-ids [xs] (vec (sort xs))) ; only two calls

(defn member? [items id] (contains? (set (map :id items)) id)) ; a predicate on top reads worse threaded

(defn draft [db] (get-in db (editor-path :draft))) ; the nested call is the path, not the data
```

<details>
<summary>Details</summary>

Three or more calls nested one inside another, each passing the one below it where a thread would:
`(vec (filter f (map g xs)))` threads to `(->> xs (map g) (filter f) vec)`, and
`(select-keys (first (get-in m [:a])) [:b])` to `(-> m (get-in [:a]) first (select-keys [:b]))`. The reader starts at
the innermost form and works outwards; a thread reads top to bottom in the order the calls run. The finding spells out
the rewrite, with every part exactly as written, and is keyed by the operator it suggests, `->` or `->>`. A `#(…)` is
read the same way and threads to `#(-> % distinct sort vec)`.

It reports only what it can rewrite without a judgement call:

- **Every step passes the call below it in the slot a thread fills,** the first argument for `->` or the last for `->>`,
  so the rewrite expands back to exactly the code that was written. A one-argument call fits either. A call of more
  arguments counts only where the nested call is the function's data: the last argument of `map`, `filter`, `remove`,
  `keep`, `mapcat`, `sort-by`, `group-by`, `take`, `drop`, `partition`, `reduce`, `into`, `cons`, `str/join` and the
  like, or the first of `get`, `get-in`, `assoc`, `update`, `dissoc`, `select-keys`, `merge`, `conj`, `str/split`,
  `str/replace` and the like, and of a `.method` call — each at the argument counts that take data, so the transducer
  `(map f)` and the two-collection `(map f xs ys)` end the chain. `(get-in db (path))` nests a call too, but the call is
  the path, and threading it reads backwards. A name an enclosing form binds — a `let`, a `for` or `doseq` binding, a fn
  parameter of any `def`-headed form or a named fn's own name, a `:keys` entry, a `letfn`, a record field, a method
  parameter, an `amap` or `areduce` binding — is a local, and gets no data slot whatever core function it shares a name
  with. A call holding a comment or a `#_` form ends the chain, and so does a step of `->`, `->>`, `some->`, `some->>`,
  `doto` or `..`, and the step of a `cond->` / `cond->>` pair, which all take an argument they do not write out, also
  under metadata, a reader conditional or a `#?@` splice, and inside a thread that is itself a step of `->`; an `as->`
  step and a `cond->` test do write theirs, and count
- **Every head is known to be a function** — a keyword, interop (`.method`, `.-field`, `Ctor.`, `js/f`, `Class/static`),
  a `clojure.core` / `cljs.core` or `clojure.string` function written plain, through an alias or through `:refer`, or a
  var the project defines with `defn`, `defn-` or `defmulti`. A project var counts only with one argument, since its
  data slot is not known, and only when it is defined under the same source directory the run reads, since each
  directory passed on the command line is analyzed on its own. Threading cannot change what a macro does, but it changes
  how it reads: `(-> xs sort vec delay)` looks like four steps run in order while the `delay` defers the other three. So
  a local, a library var the scan cannot read and a name the namespace defines any other way all end the chain
- **A chain is reported once.** Where its steps stop agreeing on the slot, it splits, and a one-argument step at the
  turn joins the part below: `(get (first (sort (map f (filter g xs)))) :k)` is reported as
  `(->> xs (filter g) (map f) sort first)`. Only the longest part long enough is reported, the innermost on a tie, so
  two threads are never suggested for one form

Left alone as well: a predicate — a name ending in `?`, such as `contains?`, `every?`, `str/includes?` or `nil?` — or a
`not`, `boolean`, `seq`, `not-empty` or `some` at the top of what would be threaded, which is not counted, since a test
reads worse at the end of a thread, so `(is (nil? (:id (first xs))))` and `(contains? (set ids) id)` are not reported; a
call on a literal, which stays the thread's starting value, so `(:question (:params (request-to "/qa")))` is two steps
above `(request-to "/qa")` rather than three, unless it is the collection of a `->>` step,
`(->> [3 1 2] (map inc) sort vec)`; the list a reader conditional holds; and anything quoted, discarded or inside
`(comment …)`.

</details>

### Cond else nil

`cond-else-nil` · cleanup — A `cond` closing on an `:else nil` it returns anyway.

```clojure
;; flagged
(defn badge [status]
  (cond
    (= :done status) "Done"
    (= :failed status) "Failed"
    :else nil))

;; fix
(defn badge [status]
  (cond
    (= :done status) "Done"
    (= :failed status) "Failed"))
```

**Left alone:**

```clojure
(defn badge [status] (cond (= :done status) "Done" :else "Pending")) ; a real default

(defn badge [status] (condp = status :done "Done" nil)) ; condp throws without its default

(defn badge [status] (case status :done "Done" nil)) ; so does case
```

<details>
<summary>Details</summary>

Why: a `cond` no clause matches falls through to `(cond)`, which its docstring says returns `nil`, so the last clause
adds nothing. It still reads as a default someone chose, though, and every reader has to stop and check that it is not
one. Dropping it leaves only the clauses that decide something, the same reason `(when x y)` reads better than
`(if x y nil)`.

Flagged: the last clause of a `cond` whose test no value can fail, `:else` or any other keyword or `true`, paired with a
literal `nil`. Comments and `#_` forms between the clauses change nothing about which clause runs, so they do not stop
it. clj-kondo's `:cond-else` asks for `:else` over `true` or `:default`; this goes one step further when the value is
`nil`.

Left alone, since the scan cannot vouch that it is core's `cond` or that it is called:

- `condp` and `case`, which throw when nothing matches and need their default, and a `cond` holding only the one clause
- a `cond` the file may take from somewhere else: an ns form that writes the symbol `cond` anywhere but as an `:as` or
  `:as-alias` alias, in an `:exclude`, a `:refer`, a `:refer-macros` or a `:rename`, behind a reader conditional too, or
  has a `:use`, a `:refer :all` or a `:refer-clojure :only`; a second ns form, an `in-ns` or a top-level `require`,
  `use`, `refer` or `refer-clojure`; a `def` of `cond` behind a top-level reader conditional; and a file with no ns form
  at all. `clojure.core/cond` and `cljs.core/cond` are core's wherever they are written
- a `cond` the namespace defines itself at the top level, from another library through an alias, or a local: a let-like
  or `for` binding, a fn parameter or a named fn's own name, a `:keys`, `:syms` or `:strs` entry, a record field or a
  method parameter
- a list that only looks like a call, even under metadata or a reader conditional: a method a `reify`, `deftype`,
  `defrecord`, `extend-type`, `extend-protocol`, `proxy`, `specify` or `specify!` defines, a `definterface` or
  `defprotocol` signature, a member of `.` or `..`, and a `case` test constant
- anything quoted, discarded or inside `(comment …)`

</details>

### Negated test

`negated-test` · cleanup — An `if` or `when` whose test is a `not`.

```clojure
;; flagged
(defn retry-label [ready? attempts]
  (if (not ready?) "Waiting" (str attempts " attempts")))

(defn warn! [ready?]
  (when (not ready?)
    (js/console.warn "not ready")))

;; fix
(defn retry-label [ready? attempts]
  (if-not ready? "Waiting" (str attempts " attempts")))

(defn warn! [ready?]
  (when-not ready?
    (js/console.warn "not ready")))
```

**Left alone:**

```clojure
(defn differs? [a b] (if (not= a b) :changed :same)) ; not= is no not

(defn step [v ready?] (-> v (when (not ready?)))) ; a threaded when takes its test from the thread

(defn guarded [not ready?] (when (not ready?) :wait)) ; a local not
```

<details>
<summary>Details</summary>

Why: `if-not` and `when-not` exist for exactly this, and the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide) asks for them over `(if (not ...) ...)` and
`(when (not ...) ...)`. The negation moves into the form's name, so the test reads as written, with one pair of
parentheses less around it. The rewrite behaves the same: the test runs once, the value is the same, and a `recur` stays
in tail position.

Flagged: an `if` with two or three arguments, or a `when` with one or more, whose test is a call of `not` on one
argument. Comments and `#_` forms inside do not stop it. The finding is keyed by `if` or `when`, and the fix is the same
form with `-not` and the test unwrapped.

Left alone, since the scan cannot vouch that the rewrite means the same:

- a test that is no single `not` call: `not=`, `(not a b)`, a bare `not`, or a `not` already under `if-not` or
  `when-not`; and a `not` holding a reader conditional, which may read as `(not)` on one platform
- a step of `->`, `->>`, `some->`, `some->>`, `doto`, `..` or a `cond->` / `cond->>` pair, whose first argument is not
  its test, also under metadata, a reader conditional or a `#?@` splice, and a step of a thread that is itself a step of
  `->`, as the `if` of `(-> m (cond-> flag (if (not x) :a)))`
- an `if`, `when`, `not`, `if-not` or `when-not` that may not be core's: from another library, referred, named anywhere
  in the ns form but as an `:as` alias, bound as a local (a `defui` or `defnc` parameter and an `areduce` result
  included), or defined by the namespace, and every case the [`cond-else-nil`](#cond-else-nil) rule leaves alone for
  `cond`
- anything quoted, discarded or inside `(comment …)`

</details>

### Into empty

`into-empty` · cleanup — An `into` an empty vector or set literal, which `vec` or `set` builds by name.

```clojure
;; flagged
(defn tag-ids [tags]
  (into #{} (map :id tags)))

(defn titles [docs]
  (into [] (map :title docs)))

;; fix
(defn tag-ids [tags]
  (set (map :id tags)))

(defn titles [docs]
  (vec (map :title docs)))
```

**Left alone:**

```clojure
(defn ids [rows] (into [] rows)) ; rows may be an array, which vec would alias

(defn ids [rows] (into #{} rows)) ; rows may be a sorted set, which set would keep sorted

(defn ids [rows] (into [] (map :id) rows)) ; a transducer

(defn ids [base rows] (into base (map :id rows))) ; no empty literal
```

<details>
<summary>Details</summary>

Why: `vec` and `set` say what they build in their name, where `(into [] …)` makes the reader take in a general merge and
an empty literal to see the same thing. For every source the rule accepts, the result has the same elements, in the same
order, and the same type.

Flagged: a two-argument `into` whose target is an empty `[]` or `#{}` literal, comments inside it included. The finding
is keyed by `into` and asks for `vec` or `set`.

`into` always builds a new collection, and `vec` and `set` do not quite: `vec` aliases a Java or JavaScript array, so
mutating the array later changes the vector; `set` returns a set it is given as it is, so a sorted set stays sorted; and
neither reads a reducible that only `reduce` can, such as `(r/map f xs)`. So the source has to be a collection literal,
or a call of a core function that returns a seq or a new plain collection: `map`, `filter`, `keep`, `keys`, `vals`,
`concat`, `distinct`, `sort`, `range`, `vector` and the like. `conj` and `merge` are not among them, since given one
argument they return it, nor are `vec` and `set`.

Left alone, since the scan cannot vouch that the rewrite means the same:

- a source that is a local, a host call, a reader conditional, or a function of another library or of the project

- an `into` with a transducer, a target that is not empty or carries metadata, or a `{}` or `()` target
- a step of a thread, which takes its source from the thread
- an `into`, `vec`, `set` or source function that may not be core's: from another library, named in the ns form, bound
  as a local, or defined by the namespace, and every case the [`cond-else-nil`](#cond-else-nil) rule leaves alone for
  `cond`
- anything quoted, discarded or inside `(comment …)`

</details>

### Plus or minus one

`plus-minus-one` · cleanup — A `+` or `-` of a literal `1`, which `inc` or `dec` names.

```clojure
;; flagged
(defn next-page [page]
  (+ page 1))

(defn last-index [items]
  (- (count items) 1))

(defn bump-all [counts]
  (map #(+ % 1) counts))

;; fix
(defn next-page [page]
  (inc page))

(defn last-index [items]
  (dec (count items)))

(defn bump-all [counts]
  (map inc counts))
```

**Left alone:**

```clojure
(defn next-page [page] (+ 1 page)) ; a leading 1 goes before a string, where inc puts it after

(defn half-step [x] (+ x 1.0)) ; a double, which inc would not add

(def last-day (+ 30 1)) ; arithmetic on numbers, spelled out

(defn total [a b] (+ a b 1)) ; more than one operand

(defn next-page [page] (-> page (+ 1))) ; a threaded step takes its operand from the thread

(defn next-page [inc page] (+ page 1)) ; a local inc
```

<details>
<summary>Details</summary>

Why: `inc` and `dec` name the step, so the reader sees a value move by one without checking the literal, and `#(+ % 1)`
becomes plain `inc`. The rewrite behaves the same: ClojureScript defines `inc` and `dec` as `(+ x 1)` and `(- x 1)`, and
in Clojure they run the same number operations as adding or subtracting `1`, overflow checks and `*unchecked-math*`
included.

Flagged: a `+` or `-` of exactly two arguments whose second is the literal `1`, also inside a `#(…)`. The finding is
keyed by `+` or `-`, and asks for `inc` or `dec`, or for the bare function when the `#(…)` only passes its argument on.

Left alone, since the scan cannot vouch that the rewrite means the same:

- a leading `1`, as in `(+ 1 x)`: ClojureScript's `+` joins strings, so `(+ 1 "a")` is `"1a"` where `(inc "a")` is
  `"a1"`
- a `1.0`, `1N` or `1M`; an operand that is a number, as in `(range 1 (+ 30 1))`, arithmetic spelled out rather than a
  step; and an operand that is a reader conditional, which may read as nothing on one platform
- a step of a thread, which takes its operand from the thread
- a `+`, `-`, `inc` or `dec` that may not be core's: from another library, named in the ns form, bound as a local, or
  defined by the namespace, and every case the [`cond-else-nil`](#cond-else-nil) rule leaves alone for `cond`
- anything quoted, discarded or inside `(comment …)`

</details>

## Idioms options

The `idioms` group reads one key from `.cljs-patrol/config.edn`:

```clojure
{:idioms {:min-calls 3}}
```

The shortest chain reported, counted after a wrapper and a call on a literal are set aside. The default is 3, the
threshold most style guides give for reaching for a thread; anything below 2 falls back to it.
