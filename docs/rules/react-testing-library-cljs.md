# react-testing-library-cljs rules

The rules of the `react-testing-library-cljs` group: how test code uses the queries of
[react-testing-library-cljs](https://github.com/olecve/react-testing-library-cljs), what each rule flags and what it
deliberately leaves alone. Each rule carries the key that `--fail-on` and the baseline use, and its
[tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only react-testing-library-cljs`. A query is recognized by what its name resolves to
through the namespace's `:require`, so the group reports nothing in code that does not use the library. Its settings
live under `:react-testing-library-cljs` in `.cljs-patrol/config.edn`; see [Test id elements](#test-id-elements). The
group reads the value of a [data `def`](../rules.md#data-defs), such as a Malli schema, as data, not as markup.

## Rules

### `get-by` presence check

`get-by-presence-check` · cleanup — An assertion that a `get-by-*` lookup found something, which it always has.

```clojure
;; flagged
(deftest save-button
  (is (some? (s/get-by-role "button" {:name "Save"}))))

;; a helper of the same file whose body is one get-by-* lookup
(defn- get-dialog [] (s/get-by-role "dialog"))

(deftest dialog
  (is (some? (get-dialog))))

;; fix
(deftest save-button
  (is (some? (s/query-by-role "button" {:name "Save"}))))

;; the helper returns the query-by-* twin
(defn- query-dialog [] (s/query-by-role "dialog"))

(deftest dialog
  (is (some? (query-dialog))))
```

**Left alone:**

```clojure
;; asserts what it found
(deftest save-button
  (is (= "Save" (.-textContent (s/get-by-role "button")))))

;; asserts how many
(deftest rows
  (is (= 2 (count (s/get-all-by-role "row")))))

;; the lookup is the assertion
(deftest save-button
  (s/get-by-role "button" {:name "Save"}))

;; the helper's lookup is acted on
(defn- get-dialog [] (s/get-by-role "dialog"))

(deftest dialog
  (.click (get-dialog)))
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
`get-by-role` is not this one. A call of a helper counts as the lookup when the helper is a `defn` or `defn-` of the
same file, at any depth outside quoted or `(comment …)` code, whose one arity returns a single `get-by-*` or
`get-all-by-*` lookup. When the file defines the name again with `def`, `defn` or `defn-`, the last definition counts,
so a later definition that does more replaces the helper. The finding then names the helper and the `query-by-*` lookup
it should return. A helper of another namespace is not followed. Left alone: a lookup the assertion reads further, such
as its text, a property or a count of what `get-all-by-*` found, since that claims more than presence; a `query-by-*` or
`find-by-*` lookup; and a lookup or an `is` whose name an enclosing form binds anew, a `let` or `fn` local, a `letfn`
function, a `with-redefs` or a `binding`, since the name then may not be the library's.

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
;; named for the lookup it returns
(defn- query-save-button []
  (s/query-by-role "button" {:name "Save"}))

;; returns more than the lookup
(defn- save-button-text []
  (.-textContent (s/get-by-role "button" {:name "Save"})))
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

`test-id-query-on-role` · cleanup · config [`:test-id-elements`](#test-id-elements) — A lookup by test id of an element
a role query could find by its role and name.

```clojure
;; flagged
(defn send-button []
  [:button {:aria-label "Send"
            :data-testid "send-button"}
   [icon]])
(deftest send-test (.click (s/get-by-test-id "send-button")))

;; another test's Save button is not on this test's screen
(deftest save-test
  (render! [:button {:data-testid "save"} "Save"])
  (.click (s/get-by-test-id "save")))
(deftest other-save-test
  (render! [:button "Save"]))

;; fix
(defn send-button []
  [:button {:aria-label "Send"
            :data-testid "send-button"}
   [icon]])
(deftest send-test (.click (s/get-by-role "button" {:name "Send"})))

;; the Save button found by its role and name
(deftest save-test
  (render! [:button {:data-testid "save"} "Save"])
  (.click (s/get-by-role "button" {:name "Save"})))
(deftest other-save-test
  (render! [:button "Save"]))
```

**Left alone:**

```clojure
;; a div has no role
(defn content []
  [:div {:class "scroll-area"
         :data-testid "content-scroll"}
   "Body"])
(deftest content-test (.scrollTo (s/get-by-test-id "content-scroll") 0 0))

;; the id is built at run time
(deftest card-test (.click (s/get-by-test-id (str "card-" 1))))

;; a component decides where the id goes
(defn banner [] [design-icon {:data-testid "banner-icon"}])
(deftest banner-test (.click (s/get-by-test-id "banner-icon")))

;; the same test renders a second Save button
(deftest two-saves-test
  (render! [:div [:button "Save"] [:button {:data-testid "save"} "Save"]])
  (.click (s/get-by-test-id "save")))
```

<details>
<summary>Details</summary>

Why: a query by role and name checks what a screen reader user gets. A test id skips that check, so a button can lose
its name and the test stays green. Many elements have no role at all, such as layout boxes, scroll containers and icons,
and for those a test id is the only way in, so the rule never asks for a test id to go. It reports only a test id the
scan can prove a role query finds.

Flagged: a `get-by-*`, `query-by-*` or `find-by-*` test id lookup, `-all-` forms included, given a literal string and
nothing after it, when every other mention of that string in what the test may render is the `:data-testid` of an
element written out with a literal attrs map, an HTML one or one [`:test-id-elements`](#test-id-elements) names, that
has:

- a role a query finds it by: a written `:role` such as `"dialog"` or `"tab"`, or else a `:button`, an `:a` with an
  `:href` string, a `:textarea`, a `:select` (`combobox`, or `listbox` with `:multiple true` or a `:size` of 1 or more),
  or an `:input` whose `:type` is written out or left to its text default and has a role (`textbox`, `searchbox`,
  `combobox` with a `:list`, `checkbox`, `radio`, `spinbutton`, `slider`, or `button` for the button types)
- a literal name: a non-blank `:aria-label` string, a `:value` string on a button, submit or reset input, or, for a role
  named by its content such as a button or a link, the text it renders when every child is a literal string or an
  element rendering one
- a role and name no other element it may render has without that test id, so a role query finds one element
- nothing that may hide it: `:hidden`, `:aria-hidden`, or a `:style` with `display: none` or `visibility: hidden` in any
  case or spacing, on the element or on an element around it in the same form, a component's literal props included, nor
  a closed `:dialog`, a `:template` or a `:datalist` around it

What a test may render, when react-testing-library-cljs's `render!` runs in it before the lookup, is its own body and
everything outside the bodies of other tests that render with `render!`: that render empties what they mounted. Such a
`render!` sits in the test's body itself, or in a `testing`, `let`, `do`, `try`, `binding` or `with-redefs` there,
before the lookup; one in a function, a branch, `#_` or `(comment …)` may not run first. A test is a `deftest` or
`deftest-` form, `deftest-` being clojure.test's or a project's own macro of that name, also inside metadata, a reader
conditional or another form; a helper written next to it stays outside it. Any other lookup, such as one in a helper or
in a test that renders another way, reads the whole run. Known limit: a test that calls `render!` and also mounts markup
another way, such as with `reagent.dom/render`, leaves that markup behind, yet its body still drops out of other tests'
evidence. A test id is compared after trimming and collapsing whitespace, as Testing Library's default normalizer does.
The hint names the role.

Left alone, since a role query could then miss what the test id finds, or find something else:

- a test id built at run time, one no element it may render sets as a literal, and a lookup with options after the id,
  such as `{:exact false}`
- any other mention of the string: a component's props (its own `:data-testid` may land on any element inside it, unless
  [`:test-id-elements`](#test-id-elements) names it), a map built with `merge` or `assoc`, a constant, a second element
  with no role or another role, a keyword or quoted test id, and one inside `#_` or `(comment …)`
- a test id a call or a `let`-bound name may build: one a `(str …)` may build from its literal start, end and middle
  parts, and, for any other call, one holding a literal string written in it, or any id when it holds none
- an element with no role, `presentation`, `none` or an empty `:role`, an `:a` whose `:href` is no string, an `:input`
  of a type with no role, such as `file`, `password` or `hidden`, or with an empty `:list`, and any attribute the role
  turns on written as an expression
- an element with no literal name: an icon-only button, text beside a component or a call, an `:aria-label` written as
  an expression, an `:aria-labelledby` (the id it points at may not exist), a `:title` (Testing Library ignores it for
  some roles), and an input named by a `<label>` elsewhere
- a role and name another element it may render shares without that test id, such as two "Save" buttons. A vector in the
  value of a [data `def`](../rules.md#data-defs), such as the Malli entry `[:button {:aria-label "Save"} :string]`, is
  no element, so it shares no role and name, and its `:data-testid` value, written out or built, is no mention. Every
  other test id string in a data `def`, such as one in `[:enum "save-button"]` or in `{:default "open-button"}`, still
  counts as a mention
- an element that may be hidden: `:hidden`, `:aria-hidden` or a `:style` written as an expression count, and so does an
  HTML or configured element around it whose attrs the scan cannot read, a name in its attrs slot included

The scan cannot see an element hidden by a component around it through anything but literal props, such as a closed
popover, or by a stylesheet, nor an element whose name is set at run time sharing the role and name; a role query is
affected by those too. It reads only ClojureScript files, so a test id set in a `.clj` macro is not a mention, and a
`:data-testid` passed in as a parameter is read only where the caller writes it.

</details>

## Test id elements

[`test-id-query-on-role`](#test-id-query-on-role) reads only HTML elements written out by default: a component decides
where its own `:data-testid` lands. A component known to put it on an element of one tag, built from its props as attrs
and its children as content, can say so in `.cljs-patrol/config.edn`:

```edn
{:react-testing-library-cljs {:test-id-elements
                              {my.ui/button {:tag :button
                                             :unknown-with #{:href :icon-only}}
                               my.ui/text-field :input
                               my.ui/popover-content {:role "dialog"}
                               my.ui.toggles/* :button}}}
```

A call whose head resolves, through the caller's `:as` or `:refer`, to a key is then read as that element:
`[ui/button {:aria-label "Send" :data-testid "send"}]` as `[:button {:aria-label "Send" :data-testid "send"}]`, with the
same role, name, hiding and uniqueness checks. A `some.ns/*` key covers every var of that namespace, and an exact key
wins over it.

`:role` names the role of the element, as a string or a keyword, such as the `dialog` a popover's content renders or a
`tablist`, which no tag gives. A role-only entry needs no `:tag`, and a role has to be one a query finds an element by.
The role a call writes wins, then the entry's `:role`, then the tag's own role. `:unknown-with` names the props the
component reads to change the element it renders or what it shows. A call writing one of them is left alone: a button
rendering an `<a>` once it has an `:href`, or only its icon once it is `:icon-only`. This is not
`:a11y :component-aliases`, which says what a component renders, not where its test id lands. A key that is not a fully
qualified symbol, an entry with neither a tag nor a role, a tag that is not a keyword, a role no query finds, or an
`:unknown-with` that is not a keyword or a collection of them, stops the run with an error.
