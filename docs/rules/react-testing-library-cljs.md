# react-testing-library-cljs rules

The rules of the `react-testing-library-cljs` group: how test code uses the queries of
[react-testing-library-cljs](https://github.com/olecve/react-testing-library-cljs), what each rule flags and what it
deliberately leaves alone. Each rule carries the key that `--fail-on` and the baseline use, and its
[tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only react-testing-library-cljs`. A query is recognized by what its name resolves to
through the namespace's `:require`, so the group reports nothing in code that does not use the library.

## Rules

### `get-by` presence check

`get-by-presence-check` · cleanup — An assertion that a `get-by-*` lookup found something, which it always has.

```clojure
;; flagged
(deftest save-button
  (is (some? (s/get-by-role "button" {:name "Save"}))))

;; fix
(deftest save-button
  (is (some? (s/query-by-role "button" {:name "Save"}))))
```

**Left alone:**

```clojure
(deftest save-button
  (is (= "Save" (.-textContent (s/get-by-role "button"))))) ; asserts what it found

(deftest rows
  (is (= 2 (count (s/get-all-by-role "row"))))) ; asserts how many

(deftest save-button
  (s/get-by-role "button" {:name "Save"})) ; the lookup is the assertion
```

<details>
<summary>Details</summary>

`get-by-*` and `get-all-by-*` in react-testing-library-cljs, in `screen` and in `within`, throw when nothing matches. An
assertion that only asks whether one found something can therefore never fail: on a hit it passes, and on a miss the
lookup throws before the check runs, which `is` reports as an error rather than a failure. The check reads as the test's
claim and decides nothing. Assert presence with the `query-by-*` twin, which returns nil on a miss and fails the
assertion with its message; `query-all-by-*` returns an empty vector instead, which is truthy, so wrap it in `seq`. Or
call the `get-by-*` lookup on its own, outside the `is`, and let its error say what is missing.

Reported when the expression of an `is` is the lookup itself, or the lookup under `some?`, `boolean`, `seq`, `not-empty`
or `(not (nil? …))`. The lookup is recognized by what its head resolves to through the namespace's `:require`, an alias
or a `:refer`, or by naming the namespace in full, so a function of another namespace that happens to be called
`get-by-role` is not this one. Left alone: a lookup the assertion reads further, such as its text, a property or a count
of what `get-all-by-*` found, since that claims more than presence; a `query-by-*` or `find-by-*` lookup; and a lookup
or an `is` whose name an enclosing form binds anew, a `let` or `fn` local, a `letfn` function, a `with-redefs` or a
`binding`, since the name then may not be the library's.

</details>

### Lookup helper name

`lookup-helper-name` · cleanup — A helper returning a lookup, named without the prefix of what that lookup does on a
miss.

```clojure
;; flagged
(defn- query-save-button []
  (s/get-by-role "button" {:name "Save"}))

;; fix
(defn- get-save-button []
  (s/get-by-role "button" {:name "Save"}))
```

**Left alone:**

```clojure
(defn- query-save-button []
  (s/query-by-role "button" {:name "Save"})) ; named for the lookup it returns

(defn- save-button-text []
  (.-textContent (s/get-by-role "button" {:name "Save"}))) ; returns more than the lookup
```

<details>
<summary>Details</summary>

A lookup's prefix says what it does when nothing matches: `get-by-*` throws, `query-by-*` returns nil and `find-by-*`
returns a promise that rejects. A helper that only returns a lookup hands that behavior to its caller, so its name
carries the same prefix: `get-`, `query-` or `find-`. A `query-` helper wrapping `get-by-*` is the costly case: the
caller asserts absence through it, `(is (nil? (query-save-button)))`, and gets an error instead of a pass. The hint
names the rename, keeping the rest of the name.

Reported for a `defn` or `defn-` with one arity whose body is a single call to a lookup of `screen` or `within`,
recognized through the namespace's `:require` as for the [presence check](#get-by-presence-check). The `-all-` forms
take the same prefix, so `get-rows` may return `get-all-by-role`. A nested lookup is judged by the outer call, as in
`(within/get-by-role (s/get-by-role "radiogroup") "radio")`. Left alone: a body doing anything besides the lookup, or
handing it to another function; a helper with several arities; a `defmethod`, or a function bound with `def` or `fn`;
and a lookup whose name a parameter, a `let`, a `letfn`, a `with-redefs` or a `binding` binds anew.

</details>
