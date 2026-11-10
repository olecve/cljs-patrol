# Docstrings rules

The rules of the `docstrings` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only docstrings`.

## Rules

### Docstring summary

`docstring-summary` · cleanup — A multi-line docstring whose first line is not a sentence of its own.

```clojure
;; flagged
(defn total
  "Add up the cart, skipping
  items without a price."
  [items]
  (reduce + (keep :price items)))

;; fix
(defn total
  "Add up the cart.

  Items without a price are skipped."
  [items]
  (reduce + (keep :price items)))
```

<details>
<summary>Details</summary>

The first line of a multi-line docstring has to be a self-contained sentence ending in `.`, `!`, `?` or `:`. Put any
further prose on the lines after it. See the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide#docstring-summary).

</details>

### Docstring indentation

`docstring-indentation` · cleanup — A docstring line indented less than its opening quote.

```clojure
;; flagged
(defn total
  "Add up the cart.
Items without a price are skipped."
  [items]
  (reduce + (keep :price items)))

;; fix
(defn total
  "Add up the cart.
  Items without a price are skipped."
  [items]
  (reduce + (keep :price items)))
```

<details>
<summary>Details</summary>

Continuation lines of a multi-line docstring are indented at least to the column of the opening quote. See the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide#docstring-indentation).

</details>

### Docstring leading/trailing whitespace

`docstring-leading-trailing-whitespace` · cleanup — A docstring starting or ending with whitespace.

```clojure
;; flagged
(defn total
  " Add up the cart. "
  [items]
  (reduce + (keep :price items)))

;; fix
(defn total
  "Add up the cart."
  [items]
  (reduce + (keep :price items)))
```

<details>
<summary>Details</summary>

A docstring that starts or ends with whitespace. See the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide#docstring-leading-trailing-whitespace).

</details>

### Docstring repeats name

`docstring-repeats-name` · cleanup — A docstring that says nothing its var's name does not.

```clojure
;; flagged
(defn get-user-data
  "Gets user data."
  [db id]
  (get-in db [:users id]))

;; fix
(defn get-user-data
  "Looks the user up in the cache only, so a user not loaded yet reads as nil."
  [db id]
  (get-in db [:users id]))
```

**Left alone:**

```clojure
;; a word the name lacks
(defn valid?
  "True when the email has an at sign."
  [email]
  (boolean (re-find #"@" email)))

;; a second line
(defn get-user-data
  "Gets user data.
  Cached per session."
  [db id]
  (get-in db [:users id]))
```

<details>
<summary>Details</summary>

A docstring is read as repeating the name when it is one line of six words at most, and every word left once filler is
dropped is a word of the name. Filler is articles and linking words (`the`, `of`, `when`…) and verbs any function could
claim (`gets`, `returns`, `creates`, `checks`…). The name splits on `-`, `>`, `?`, `!` and `*`, and a plural matches its
singular, so `"Returns the entries."` repeats `entries`. A docstring with nothing left once filler is dropped, such as
`"Gets it."`, is left alone: it gives no evidence either way.

</details>
