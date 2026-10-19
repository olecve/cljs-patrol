# Layout rules

The rules of the `layout` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only layout`. Its settings live under `:layout` in `.cljs-patrol/config.edn`; see
[Layout options](#layout-options).

## Rules

### Form after split map

`form-after-split-map` · cleanup — A form starting on the line a split map closes on.

```clojure
;; flagged
(def points
  [{:x 0
    :y 0} {:x 10
           :y 0}])

;; fix
(def points
  [{:x 0
    :y 0}
   {:x 10
    :y 0}])
```

**Left alone:**

```clojure
(def points [{:x 0 :y 0} {:x 10 :y 0}]) ; maps on one line cannot split

(defn edit-button [] [button {:size :s
                              :variant :ghost} "Edit"]) ; a short label child
```

<details>
<summary>Details</summary>

A form starts on the line where a map split over several lines closes. cljfmt's `:split-keypairs-over-multiple-lines?`
puts every key-value pair of a multi-key map on its own line, but leaves the elements of a vector, list or call where
they are, so a map sharing a line with a sibling turns into a staircase that `cljfmt check` accepts: `[{:x 0` /
`:y 0} {:x 10` / `:y 0}]`. No cljfmt or clj-kondo option prevents it. Flagged: the next map of a vector, every argument
of a call written after a split map (`(merge {:a 1` / `:b 2} {:c 3})`), every child of a Hiccup element written on the
closing line of its split props map, and any form spanning lines that starts on the closing line of a form ending in a
split map. Left alone: a map that opens and closes on one line, which cannot split; a single-line string child of a
Hiccup element, `[button {…} "Edit"]`, which reads as its label, and for the same reason a lone symbol child with
nothing after it on the line, `[button {…} items-per-page]` or the `int?` of a Malli entry `[:page-size {…} int?]` (a
symbol followed by more forms on that line is still flagged); a single-line form other than a map after a split map in a
vector of data or of bindings, `[{:keys [db]` / `:as cofx} event]`; the second half of a pair — a `let`-like binding, a
`cond`, `case`, `condp` or `cond->` clause, a reader conditional branch; and braces inside strings, comments and
regexes, since forms are read from the parse tree rather than from the text. Put each sibling on its own line, aligned
with the first.

</details>

### Split map after sibling

`split-map-after-sibling` · cleanup · config [`:split-map-after-symbol`](#layout-options) — A split map starting after a
whole sibling on the same line.

```clojure
;; flagged
(defn handler [{:keys [db]} {:keys [id]
                             :as profile}]
  [db id profile])

;; fix
(defn handler [{:keys [db]}
               {:keys [id]
                :as profile}]
  [db id profile])
```

**Left alone:**

```clojure
(defn save [db] (assoc-in db [:a :b] {:x 1
                                      :y 2})) ; a path vector before its value
```

<details>
<summary>Details</summary>

A map split over several lines starts after a whole sibling form on the same line, so its pairs hang off a column far to
the right of the form they belong to: `[{:keys [db]} {:keys [id]` / `:as profile}]` or
`(write-source-note {:db db} {:draft-id id` / `:source source})`. Start the map on its own line, aligned with the
sibling before it. The sibling has to be a map or a vector. Never a sibling: the head of a call or of a Hiccup element
(`(f {:a 1`, `[button {:size :s`); a fn's parameter list or a `let`'s bindings, which are followed by the form's body;
and a path vector of plain tokens outside a binding form, `(assoc-in db [:a :b] {…})`, which is how a target and its
value are passed. A plain symbol, `_` included, counts only inside a parameter list or binding and only when configured
— see [Layout options](#layout-options).

</details>

## Layout options

The `layout` group reads one key from `.cljs-patrol/config.edn`:

```clojure
{:layout {:split-map-after-symbol true}}
```

It widens [Split map after sibling](#split-map-after-sibling) to a map after a plain symbol in a parameter list or
binding — `(fn [cofx [_ {:keys [id]` / `:as user-group}]]`. Off by default, because cljfmt leaves this shape all over a
typical codebase and most read it as fine: on one UI codebase of about 1,700 files it adds 127 findings to the 4
reported without it. A symbol outside a binding form, `(reset! state {…})`, is the target of a call and is not reported
either way.
