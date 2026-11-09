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

### Test id query on role

`test-id-query-on-role` · cleanup — A lookup by test id of an element a role query could find by its role and name.

```clojure
;; flagged
(defn send-button [] [:button {:aria-label "Send" :data-testid "send-button"} [icon]])
(deftest send-test (.click (s/get-by-test-id "send-button")))

;; fix
(defn send-button [] [:button {:aria-label "Send" :data-testid "send-button"} [icon]])
(deftest send-test (.click (s/get-by-role "button" {:name "Send"})))
```

**Left alone:**

```clojure
(defn content [] [:div {:class "scroll-area" :data-testid "content-scroll"} "Body"])
(deftest content-test (.scrollTo (s/get-by-test-id "content-scroll") 0 0)) ; a div has no role

(deftest card-test (.click (s/get-by-test-id (str "card-" 1)))) ; the id is built at run time

(defn banner [] [design-icon {:data-testid "banner-icon"}])
(deftest banner-test (.click (s/get-by-test-id "banner-icon"))) ; a component decides where the id goes
```

<details>
<summary>Details</summary>

Why: a query by role and name checks what a screen reader user gets. A test id skips that check, so a button can lose
its name and the test stays green. Many elements have no role at all, such as layout boxes, scroll containers and icons,
and for those a test id is the only way in, so the rule never asks for a test id to go. It reports only a test id the
scan can prove a role query finds.

Flagged: a `get-by-*`, `query-by-*` or `find-by-*` test id lookup, `-all-` forms included, given a literal string and
nothing after it, when every other mention of that string in the files the run reads, source and tests alike, is the
`:data-testid` of an HTML element written out with a literal attrs map that has:

- a role a query finds it by: a written `:role` such as `"dialog"` or `"tab"`, or else a `:button`, an `:a` with an
  `:href` string, a `:textarea`, a `:select` (`combobox`, or `listbox` with `:multiple true` or a `:size` of 1 or more),
  or an `:input` whose `:type` is written out or left to its text default and has a role (`textbox`, `searchbox`,
  `combobox` with a `:list`, `checkbox`, `radio`, `spinbutton`, `slider`, or `button` for the button types)
- a literal name: a non-blank `:aria-label` string, a `:value` string on a button, submit or reset input, or, for a role
  named by its content such as a button or a link, the text it renders when every child is a literal string or an HTML
  element rendering one
- a role and name no other element written out in the run has without that test id, so a role query finds one element
- nothing that may hide it: `:hidden`, `:aria-hidden`, or a `:style` with `display: none` or `visibility: hidden` in any
  case or spacing, on the element or on an HTML element around it in the same form, nor a closed `:dialog`, a
  `:template` or a `:datalist` around it

A test id is compared after trimming and collapsing whitespace, as Testing Library's default normalizer does. The hint
names the role.

Left alone, since a role query could then miss what the test id finds, or find something else:

- a test id built at run time, one no element in the run sets as a literal, and a lookup with options after the id, such
  as `{:exact false}`
- any other mention of the string: a component's props (its own `:data-testid` may land on any element inside it,
  `:component-aliases` included), a map built with `merge` or `assoc`, a constant, a second element with no role or
  another role, a keyword or quoted test id, and one inside `#_` or `(comment …)`
- a test id a call or a `let`-bound name may build: one a `(str …)` may build from its literal start, end and middle
  parts, and, for any other call, one holding a literal string written in it, or any id when it holds none
- an element with no role, `presentation`, `none` or an empty `:role`, an `:a` whose `:href` is no string, an `:input`
  of a type with no role, such as `file`, `password` or `hidden`, or with an empty `:list`, and any attribute the role
  turns on written as an expression
- an element with no literal name: an icon-only button, text beside a component or a call, an `:aria-label` written as
  an expression, an `:aria-labelledby` (the id it points at may not exist), a `:title` (Testing Library ignores it for
  some roles), and an input named by a `<label>` elsewhere
- a role and name another element in the run shares without that test id, such as two "Save" buttons
- an element that may be hidden: `:hidden`, `:aria-hidden` or a `:style` written as an expression count, and so does an
  HTML element around it whose attrs the scan cannot read, a name in its attrs slot included

The scan cannot see an element hidden by a component around it, such as a closed popover, or by a stylesheet, nor an
element whose name is set at run time sharing the role and name; a role query is affected by those too. It reads only
ClojureScript files, so a test id set in a `.clj` macro is not a mention, and a `:data-testid` passed in as a parameter
is read only where the caller writes it.

</details>
