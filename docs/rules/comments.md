# Comments rules

The rules of the `comments` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only comments`. It has no settings of its own. Like `docstrings`, `tests`, `layout` and
`idioms`, it reads `.clj` files too under [`--experimental-clj`](../rules.md#experimental-reading-clj-files).

## Rules

### Commented-out code

`commented-out-code` · cleanup — A `;` comment holding code switched off rather than deleted.

```clojure
;; flagged
(defn save [db]
  ;; (js/console.log "saving" db)
  (assoc db :saved? true))

;; fix
(defn save [db]
  (assoc db :saved? true))
```

**Left alone:**

```clojure
;; prose that opens with a parenthesis
(defn save [db]
  ;; (see the save handler) keeps the flag in sync
  (assoc db :saved? true))

;; a shape written to document data
(defn effect [db]
  ;; [:dispatch [::saved args...]]
  {:db db})

;; a REPL example showing its result
(defn double [x]
  ;; (double 2) ;=> 4
  (* 2 x))

;; code kept for the REPL on purpose
(comment
  ;; (save {:id 1})
  (save {}))
```

<details>
<summary>Details</summary>

Code in a comment goes stale while the code around it moves, and version control already keeps what it held. Code kept
on purpose for the REPL belongs in a `(comment …)` form, which this rule does not read.

A comment on a line of its own counts, and consecutive ones at the same column make one block, read as a whole once each
line's `;` markers and the space after them are removed. The block is code when it reads back as Clojure forms, and the
reading is strict, to keep prose out:

- every form in the block is a list, a vector or a map, so prose with a word outside a parenthesis is not code
- one of them is a call, a list headed by a symbol, so a data shape such as `[:div "x"]` is not either
- it holds a call whose head reads as code: an operator, `(= a b)`; a head that is no plain word, such as `defn`, `let`,
  `fn` or `assoc`; a call through an alias the ns form requires, or `js/`; or a head ending in `!` or `?` with arguments
  after it, `(swap! x f)`. Or it holds a `#(…)`, a `#"regex"` or a `::keyword`. A nested form, a deref, a quote, an
  operator between two words or a word ending in `?` alone is as likely a note in parentheses:
  `(returns (nil) when empty)`, `(cc @bob)`, `(draft -> review)`, `(should we cache this?)`
- it holds no placeholder: `...`, `…`, a name ending in `...` such as `args...`, or a lone capital such as `N`
- it holds no word of prose, such as `see`, `the`, `of`, `by`, `e.g.` or `cf.`, and no capitalized word, `(RFC 7231)`; a
  class name, `(catch Exception e …)`, reads as one too
- it holds no `=>`, which marks a REPL example showing a result, and no `,,,`, a placeholder
- the comment just before it, a blank line apart at most, does not end in `:`, as `;; Example:` does

The finding points at the first form in the block, and its baseline identity is every form in it, so two blocks that
open the same way stay apart.

A comment after code on the same line, `(inc x) ; (swap! x f)`, is not read.

</details>

### Restating comment

`restating-comment` · cleanup — A one-line comment that says in words what the next line does.

```clojure
;; flagged
(defn tagged [db]
  ;; Fetch tagged documents
  (fetch db {:source :tagged-documents}))

;; fix
(defn tagged [db]
  (fetch db {:source :tagged-documents}))
```

**Left alone:**

```clojure
;; a reason the line cannot give
(defn tagged [db]
  ;; Fetch documents again because the cache is stale
  (fetch db {:source :tagged-documents}))

;; a label naming the group after it
(def palette
  ;; Button pin
  {:button-pin-pinned "#EF4444"})
```

<details>
<summary>Details</summary>

A comment on a line of its own, with no other comment line next to it, is read against the source line right after it.
It restates that line when:

- it opens with an action verb, such as `create`, `fetch`, `get`, `delete`, `update`, `parse` or `sort`, in any form, so
  a label naming what follows, `;; Button pin` or `;; Personal settings`, is left alone
- it holds six words at most, and no parenthesis, bracket, `#`, `=`, `?`, `:`, backtick or URL
- every word after the verb, `the`, `a`, `of` and the like aside, is a word of the next line, plurals and verb forms
  folded: `;; Create a project` above `project (post-project {:title "x"})`. A reason or a condition brings words the
  line does not hold, so a comment giving one is left alone

The finding's baseline identity is the comment and the line after it, so two `;; Create a project` comments in one file
stay apart when the lines they sit on differ.

</details>
