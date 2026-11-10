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
reading is strict, so that prose is never taken for code:

- every form in the block is a list, a vector or a map, so prose with a word outside a parenthesis is not code
- one of them is a call, a list headed by a symbol, so a data shape such as `[:div "x"]` is not either
- it holds a qualified symbol or a form nested in another; a keyword, a string or a number alone is as likely a note in
  parentheses, `(default 300)` or `(only :dev)`
- it holds no placeholder: `...`, `…`, a name ending in `...` such as `args...`, or a lone capital such as `N`
- it holds no word of prose, such as `see`, `the`, `of`, `by`, `e.g.` or `cf.`, and no capitalized word, `(RFC 7231)`
- it holds no `=>`, which marks a REPL example showing a result
- the comment just before it, a blank line apart at most, does not end in `:`, as `;; Example:` does

The finding points at the first form in the block, and its baseline identity is every form in it, so two blocks that
open the same way stay apart.

A comment after code on the same line, `(inc x) ; (dec x (y))`, is not read.

</details>
