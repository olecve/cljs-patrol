# Idioms rules

The rules of the `idioms` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

### Nested call chain

`nested-call-chain` · cleanup — Nested calls a thread would read top to bottom.

```clojure
;; flagged
(defn active-users [rows]
  (vec (filter active? (map :user rows))))

(defn first-label [m]
  (select-keys (first (get-in m [:a])) [:label]))

;; fix
(defn active-users [rows]
  (->> rows (map :user) (filter active?) vec))

(defn first-label [m]
  (-> m (get-in [:a]) first (select-keys [:label])))
```

**Left alone:**

```clojure
(defn sorted-ids [xs] (vec (sort xs))) ; only two calls

(defn member? [ids id] (contains? (set (map :id ids)) id)) ; a predicate on top reads worse threaded

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
  parameter, a `letfn` — is a local, and gets no data slot whatever core function it shares a name with. A call holding
  a comment or a `#_` form ends the chain, and so does a step of `->`, `->>`, `some->`, `some->>`, `doto` or `..`, and
  the step of a `cond->` / `cond->>` pair, which all take an argument they do not write out; an `as->` step and a
  `cond->` test do write theirs, and count
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
`(comment …)`

</details>

## Idioms options

The `idioms` group reads one key from `.cljs-patrol/config.edn`:

```clojure
{:idioms {:min-calls 3}}
```

The shortest chain reported, counted after a wrapper and a call on a literal are set aside. The default is 3, the
threshold most style guides give for reaching for a thread; anything below 2 falls back to it.
