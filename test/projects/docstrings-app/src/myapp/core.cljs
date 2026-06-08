(ns myapp.core)

(defn single-line-no-period
  "A short summary without a terminator"
  [x]
  x)

(defn good-multiline
  "This is a good summary.
  And this is the rest of the docstring."
  [x]
  x)

(defn identifier-with-question-mark
  "Return whether x is :dynamic? based on the first element.
  The trailing ? is part of a keyword, not a sentence terminator."
  [x]
  x)

(defn pass-name-title-abbreviation
  "Greets Mr. Smith and Dr. Jones every morning.
  Returns the greeting count."
  [x]
  x)

(defn pass-place-abbreviation
  "Resolves the address in St. Petersburg or similar.
  Returns the canonical form."
  [x]
  x)

(defn fail-eg-transition
  "Returns x. E.g. when foo is bar.
  More details."
  [x]
  x)

(defn fail-parenthetical-run-on
  "Helper for x (the legacy one). Used by foo elsewhere.
  Detailed explanation."
  [x]
  x)

(defn pass-colon-summary
  "Returns these keys:
  - :foo
  - :bar."
  [x]
  x)

(defn summary-fail-no-period
  "This is missing a period
  on the first line."
  [x]
  x)

(defn summary-fail-extra-prose
  "This does X. And also Y on the same line,
  which is bad style."
  [x]
  x)

(defn indent-fail
  "First line.
no indentation on continuation."
  [x]
  x)

(defn whitespace-fail-leading
  " A docstring that starts with a space.
  Second line."
  [x]
  x)

(defn whitespace-fail-trailing
  "A docstring that ends with spaces.
  Second line.   "
  [x]
  x)

(defn whitespace-leading-newline
  "
  Starts with a newline.
  Second line."
  [x]
  x)

(defn ^:private private-meta
  "Bad: extra prose on same line. Private vars are checked too.
  More text."
  [x]
  x)

(defn- private-defn-dash
  "Bad: extra prose on same line. The defn- form is also checked.
  More text."
  [x]
  x)

(defn- compliant-private
  "A compliant private docstring.
  Continuation aligned."
  [x]
  x)

(def value-only "actually a value")

(def documented-value
  "This is a real docstring.
  Second line."
  42)

(defmulti dispatcher
  "Dispatcher summary.
  Second line."
  identity)

(defprotocol Greeter
  "Outer protocol summary.
  Continuation."
  (greet
    [this name]
    "Bad method summary. Run-on Sentence on the same line.
    More details.")
  (farewell
    [this name]
    "Compliant farewell summary.
    Continuation."))
