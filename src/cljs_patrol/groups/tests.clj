(ns cljs-patrol.groups.tests
  "Rule group: naming conventions on test vars."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private deftest-heads
  "Forms that define a test var.

  `deftest-` is cljs.test's private variant, and a project's own wrapper macro is named
  here only when it is spelled the same way — nothing resolves a macro across namespaces."
  #{"deftest" "deftest-"})

(def ^:private leading-articles
  "Articles that carry nothing at the front of an identifier.

  Only at the front: a test named after the claim it makes is a sentence, and an article
  inside one is grammar rather than noise — `escape-closes-the-modal` reads as intended,
  where `the-modal-closes-on-escape` opens on a word that says nothing about the claim."
  ["a" "an" "the"])

(defn- leading-article
  "Return the article a test name opens with, or nil."
  [test-name]
  (some #(when (str/starts-with? test-name (str % "-")) %) leading-articles))

(defn- literal-string-loc? [loc]
  (and loc
       (contains? #{:token :multi-line} (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))))

(defn- end-row [loc]
  (try (first (second (z/position-span loc))) (catch Exception _ nil)))

(defn- inline-assertion-message
  "Return the message loc of an `(is expr \"…\")` that shares a line with the expression.

  Only a string written out counts. A message named by a symbol is not the shape the
  convention is about, and a message the expression spans several lines above already
  reads as its own line."
  [loc]
  (when (= "is" (some-> loc z/down parser/sym-name))
    (let [message (some-> loc z/down z/rightmost)
          expression (some-> message z/left)]
      (when (and (literal-string-loc? message)
                 expression
                 (not (identical? (z/node expression) (z/node (z/down loc))))
                 (= (parser/position-row message) (end-row expression)))
        message))))

(defn- assertion-finding
  "Build the finding for an assertion whose message shares the expression's line.

  `:form` is the whole assertion, not the message alone. Two assertions in one file
  often carry the same message over different expressions — `(is (.-checked checkbox-1)
  \"…\")` beside `(is (.-checked checkbox-3) \"…\")` — and the baseline keys on the form,
  so a message-only form would make them one entry: fix either and the other stays
  suppressed."
  [is-loc message-loc file]
  {:kw (symbol "is")
   :type :assertion-message-inline
   :form (parser/normalize-form (parser/raw is-loc))
   :file file
   :row (parser/position-row message-loc)
   :hint "Move the message to its own line, under the expression."})

(def ^:private conditional-heads
  "Forms whose value depends on a test evaluated while the test body runs."
  #{"if" "if-not" "when" "when-not" "cond" "condp" "case" "if-let" "when-let" "if-some" "when-some"})

(def ^:private assertion-heads #{"is"})

(defn- inside-function-literal?
  "True when a conditional sits in a function handed to something else, up to `stop`.

  `(some #(when (= 1 (:id %)) %) xs)` branches per element of a collection, which is a
  predicate rather than the test choosing what to assert."
  [loc stop]
  (loop [current (z/up loc)]
    (cond
      (or (nil? current) (identical? (z/node current) (z/node stop))) false
      (= :fn (z/tag current)) true
      (and (= :list (z/tag current))
           (contains? #{"fn" "fn*"} (parser/sym-name (z/down current)))) true
      :else (recur (z/up current)))))

(defn- descendant-locs
  "Return the locs strictly inside `loc`, confined to it.

  On the file's own zipper rather than a `z/subzip` of it: a subzip restarts position
  tracking, and a finding reported from one names line 1 of the form instead of its line
  in the file."
  [loc]
  (let [inside? (fn [candidate]
                  (loop [current candidate]
                    (cond
                      (nil? current) false
                      (identical? (z/node current) (z/node loc)) true
                      :else (recur (z/up current)))))]
    (take-while #(and % (not (z/end? %)) (inside? %))
                (rest (iterate #(some-> % z/next) loc)))))

(defn- holds-assertion? [loc]
  (boolean (some #(and (= :list (z/tag %))
                       (contains? assertion-heads (parser/sym-name (z/down %))))
                 (descendant-locs loc))))

(defn- conditional-in-assertion
  "Return the conditional an assertion computes its expectation with, or nil."
  [loc]
  (when (contains? assertion-heads (some-> loc z/down parser/sym-name))
    (first (filter #(and (= :list (z/tag %))
                         (contains? conditional-heads (parser/sym-name (z/down %)))
                         (not (inside-function-literal? % loc)))
                   (descendant-locs loc)))))

(def ^:private branch-heads #{"if" "if-not" "cond" "condp" "case"})

(defn- conditional-around-assertion
  "Return `:picks` or `:skips` when a conditional decides what gets asserted.

  `:picks` for a form with arms, where the test chooses between assertions rather than
  stating one. `:skips` for a guard, which is the worse of the two: when it does not
  hold, the test runs no assertion and passes without having checked anything."
  [loc]
  (when-let [head (some-> loc z/down parser/sym-name)]
    (when (and (contains? conditional-heads head) (holds-assertion? loc))
      (if (contains? branch-heads head) :picks :skips))))

(def ^:private conditional-hints
  {:computes "The expected value is computed here. State it instead, one case per row of a table."
   :picks "The test chooses between assertions. Pair each input with its expected value instead."
   :skips "This guard can skip the assertion, and the test then passes having checked nothing."})

(defn- conditional-finding [loc kind head file]
  {:kw (symbol head)
   :type :conditional-assertion
   :form (parser/normalize-form (parser/raw loc))
   :file file
   :row (parser/position-row loc)
   :hint (get conditional-hints kind)})

(defn- enclosing-list-head
  "Return the head symbol's raw text for the nearest enclosing list satisfying pred."
  [loc pred]
  (loop [current (z/up loc)]
    (cond
      (nil? current) nil
      (and (= :list (z/tag current))
           (let [head (some-> current z/down parser/raw)]
             (and head (pred head)))) (some-> current z/down parser/raw)
      :else (recur (z/up current)))))

(defn- path-segments [path]
  (remove str/blank? (str/split (str path) #"[/\\\\]")))

(defn- under-path?
  "True when `file` lies under `configured`, matched a whole segment at a time.

  Segment-wise so `test` does not match `src/cljs/latest/…`. A configured path may be
  written as one segment or several — `test` and `ui/test` both work — and matches
  wherever that run of segments appears, since the tool is pointed at whatever directory
  the caller chooses and the file paths it reports are relative to that."
  [file configured]
  (let [wanted (path-segments configured)
        actual (path-segments file)]
    (and (seq wanted)
         (boolean (some #(= wanted (take (count wanted) %))
                        (take-while seq (iterate rest actual)))))))

(defn- test-file?
  "True when the file holds test code.

  Configured paths win when a project sets them: the directory a test lives in is a fact,
  where the namespace's name is a convention. With none set the convention is the
  fallback, so the group works pointed at a test directory without any configuration —
  and it has to answer one way or the other, since the group runs by default and the
  rules anchored on this would otherwise report production code."
  [paths {:keys [ns-name]} file]
  (if (seq paths)
    (boolean (some #(under-path? file %) paths))
    (boolean (and ns-name (str/ends-with? ns-name "-test")))))

(defn- enclosing-definition-name
  "Return the name of the nearest enclosing `def`-like form, or nil.

  A deref's own text is often just a var name, so two reads of one var in a file would be
  a single baseline identity. What the read sits in — a test, a fixture, a helper — tells
  them apart and moves with the code, where a row does not."
  [loc]
  (loop [current (z/up loc)]
    (cond
      (nil? current) nil
      (and (= :list (z/tag current))
           (some-> current z/down parser/sym-name (str/starts-with? "def")))
      (some-> (parser/declared-name-loc current) parser/sym-name)
      :else (recur (z/up current)))))

(defn- inside-written-out-quote?
  "True when a `(quote …)` list encloses loc.

  `hiccup/unrendered-form?` reads the reader spellings, `'x` and `#_x`, which are their
  own node types. Written out, a quote is an ordinary list and nothing marks it."
  [loc]
  (some? (enclosing-list-head loc #{"quote"})))

(defn- up-through-meta [loc]
  (loop [current (some-> loc z/up)]
    (if (= :meta (some-> current z/tag)) (recur (z/up current)) current)))

(defn- var-quote-loc
  "Return the var quote `loc` is part of, written either way, or nil.

  `#'x` is a `:var` node around a token; `(var x)` is an ordinary list. Only those two
  node types reach a handler — a `:deref` node reaches none — so the deref is found by
  looking up from the var quote rather than down from the `@`."
  [loc]
  (cond
    (and (= :list (z/tag loc)) (= "var" (some-> loc z/down parser/raw))) loc

    ;; Only from the token the var quotes. `#'^:tag x` holds two tokens — the metadata's
    ;; and the value's — and both climb to the same `:var`, so reporting from either
    ;; reports the one deref twice.
    (let [var-loc (up-through-meta loc)]
      (and (= :var (some-> var-loc z/tag))
           (identical? (z/node loc) (some-> var-loc z/down hiccup/unwrap-meta z/node))))
    (up-through-meta loc)))

(defn- deref-around
  "Return the deref reading this var quote, or nil.

  `@` is a `:deref` node and `(deref …)` a list. The head is compared raw, so
  `helpers/deref` — someone else's function whose name merely ends the same way — is not
  this one."
  [var-loc]
  (let [parent (up-through-meta var-loc)]
    (cond
      (= :deref (some-> parent z/tag)) parent
      (and (= :list (some-> parent z/tag))
           (= "deref" (some-> parent z/down parser/raw))
           (not (identical? (z/node var-loc) (z/node (z/down parent))))) parent)))

(defn- var-deref-finding
  "Build the finding for a var quote read through a deref in a test namespace.

  `:form` is the whole deref and `:test` the definition it sits in. Two reads inside one
  definition still collapse to a single baseline identity, which no key that survives
  reformatting can help."
  [loc test-name file]
  {:kw (symbol "deref")
   :type :var-deref-in-test
   :form (parser/normalize-form (parser/raw loc))
   :test test-name
   :file file
   :row (parser/position-row loc)
   :hint "Call it through the public entry point that uses it, or move it somewhere it can be public."})

(defn- var-deref-in-test
  "Return the finding for a var quote read through a deref in a test namespace, or nil."
  [paths loc ns-info file]
  (when (test-file? paths ns-info file)
    (when-let [var-loc (var-quote-loc loc)]
      (when-let [deref-loc (deref-around var-loc)]
        (when (and (not (hiccup/inside-unrendered-form? deref-loc))
                   (not (inside-written-out-quote? deref-loc)))
          (var-deref-finding deref-loc (enclosing-definition-name deref-loc) file))))))

(def ^:private js-tag "js")

(defn- js-tag-token? [loc]
  (and (= :token (z/tag loc)) (= js-tag (parser/raw loc))))

(defn- leftmost-child? [loc parent]
  (and parent (identical? (z/node loc) (some-> parent z/down z/node))))

(defn- js-type-hint
  "Return the metadata node a bare `^js` hint is written on, or nil.

  Both spellings are the same hint: `^js x` puts the tag symbol straight into the
  metadata node, `^{:tag js} x` puts it under `:tag` in a map there. Only the bare symbol
  counts — `^js/Foo` names a type and is a different thing, and `^clj`, `^boolean` and
  `^:private` are other tags entirely.

  A `:meta` node reaches no handler and neither does a `:map`, so the hint is found from
  the `js` symbol inside it and read upward."
  [loc]
  (when (js-tag-token? loc)
    (let [parent (z/up loc)]
      (cond
        (and (= :meta (some-> parent z/tag)) (leftmost-child? loc parent))
        parent

        (and (= :map (some-> parent z/tag))
             (= :meta (some-> parent z/up z/tag))
             (leftmost-child? parent (z/up parent))
             (identical? (z/node loc) (some-> (hiccup/literal-map parent) :tag z/node)))
        (z/up parent)))))

(defn- js-hint-finding
  "Build the finding for a `^js` hint.

  `:test` is the definition the hint sits in. A hint's own text is short and repeats —
  `^js el` can appear many times in one file — so without it every hint of the same
  shape in a file would be a single baseline identity. Several identical hints inside one
  definition still collapse, which matters little here: the fix removes all of them."
  [hint-loc test-name file]
  (let [[row col] (try (z/position hint-loc) (catch Exception _ [0 1]))]
    {:kw (symbol js-tag)
     :type :js-hint-in-test
     :form (parser/normalize-form (parser/raw hint-loc))
     :test test-name
     :file file
     :row row
     :col col
     :hint "Remove the hint — nothing reads it in a build that is not :advanced."}))

(defn- js-hint-in-test
  "Return the finding for a `^js` hint written in a test namespace, or nil."
  [paths loc ns-info file]
  (when (test-file? paths ns-info file)
    (when-let [hint-loc (js-type-hint loc)]
      (when-not (or (hiccup/inside-unrendered-form? hint-loc)
                    (inside-written-out-quote? hint-loc))
        (js-hint-finding hint-loc (enclosing-definition-name hint-loc) file)))))

(defn- leading-article-finding [loc file]
  (when (contains? deftest-heads (some-> loc z/down parser/sym-name))
    (when-let [name-loc (parser/declared-name-loc loc)]
      (when-let [test-name (parser/sym-name name-loc)]
        (when-let [article (leading-article test-name)]
          {:kw (symbol test-name)
           :type :deftest-leading-article
           :article article
           :file file
           :row (parser/position-row name-loc)
           :hint (str "Drop the leading \"" article "-\": "
                      (subs test-name (inc (count article))))})))))

(defn- handle-list [paths loc ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (when-let [finding (or (leading-article-finding loc file)
                           (when-let [message (inline-assertion-message loc)] (assertion-finding loc message file))
                           (when-let [conditional (conditional-in-assertion loc)]
                             (conditional-finding conditional :computes
                                                  (parser/sym-name (z/down conditional)) file))
                           (when-let [kind (conditional-around-assertion loc)]
                             (conditional-finding loc kind
                                                  (parser/sym-name (z/down loc)) file))
                           (var-deref-in-test paths loc ns-info file))]
      {:decls [finding]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:deftest-leading-article (vec (filter #(= :deftest-leading-article (:type %)) declarations))
   :assertion-message-inline (vec (filter #(= :assertion-message-inline (:type %)) declarations))
   :conditional-assertion (vec (filter #(= :conditional-assertion (:type %)) declarations))
   :var-deref-in-test (vec (filter #(= :var-deref-in-test (:type %)) declarations))
   :js-hint-in-test (vec (filter #(= :js-hint-in-test (:type %)) declarations))})

(defn- summary-lines* [{:keys [deftest-leading-article assertion-message-inline
                               conditional-assertion var-deref-in-test js-hint-in-test]}]
  [["Deftest leading article:" (count deftest-leading-article)]
   ["Assertion message inline:" (count assertion-message-inline)]
   ["Conditional assertion:" (count conditional-assertion)]
   ["Var deref in test:" (count var-deref-in-test)]
   ["Js hint in test:" (count js-hint-in-test)]])

(defn- failed?* [_]
  ;; Nothing here blocks at runtime, and the guard shape has defensible uses — a `when`
  ;; narrowing a doseq to the combination under test reads as deliberate. A project that
  ;; wants any of these enforced opts in with --fail-on.
  false)

(defrecord TestsGroup [paths]
  group/RuleGroup
  (group-id [_] :tests)
  (group-name [_] "Tests")
  (parse-handlers [_] {:handle-list (fn [loc ns-info file] (handle-list paths loc ns-info file))
                       :handle-token (fn [loc ns-info file]
                                       (when-let [finding (or (var-deref-in-test paths loc ns-info file)
                                                              (js-hint-in-test paths loc ns-info file))]
                                         {:decls [finding]
                                          :usages []
                                          :dynamics []}))})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:deftest-leading-article
     (str "A test var opens with \"a-\", \"an-\" or \"the-\". A test named after the claim it "
          "makes is a sentence, and the article at the front of one carries nothing an "
          "identifier needs: the reader is looking for the subject, and the first word is "
          "not it. Drop it — the-status-label-is-rendered becomes "
          "status-label-is-rendered. Only the front is flagged; an article inside the name "
          "is grammar, so escape-closes-the-modal is left alone. Point the tool at the test "
          "directory to use this group, and snapshot an existing codebase with "
          "--baseline-write so only new names are reported.")
     :assertion-message-inline
     (str "An assertion's message shares a line with the expression it describes. The two "
          "say different things — one is what ran, the other why the answer matters — and "
          "a reader scanning a test body for either has to read past the other to find it. "
          "Put the message on its own line under the expression, which is how the style "
          "guide's own examples are written. Only a string written out is flagged; a "
          "message named by a symbol is not the shape the convention is about.")
     :conditional-assertion
     (str "A test decides what to assert while it runs. Three shapes, one defect: the test "
          "does not state what it expects, it works it out. An expected value computed by "
          "a conditional — (is (= actual (if owner? false true))) — re-derives the answer, "
          "so a mistake shared with the code under test passes unnoticed and a reader "
          "cannot see the expectation without simulating it. A conditional choosing "
          "between assertions is the same thing spread over two branches. A guard that can "
          "skip the assertion, (when x (is …)), is the worst of the three: when it does not "
          "hold, the test runs no assertion at all and passes having checked nothing. "
          "Write a table instead — pair each input with the value it should produce and "
          "walk it with doseq, so every case states its own expectation. A conditional "
          "inside a function literal is left alone: #(when (= 1 (:id %)) %) handed to some "
          "is a predicate over a collection, not the test choosing. Nor is a guard always "
          "wrong: a when narrowing a doseq to the combination under test reads as "
          "deliberate, which is why this reports rather than blocks. Enforce it with "
          "--fail-on where a project wants it.")
     :js-hint-in-test
     (str "A `^js` type hint in a test namespace does nothing. The hint exists so that "
          "shadow-cljs infers an extern and the Closure compiler leaves a JS interop "
          "property name alone under :advanced optimizations (shadow-cljs User's Guide, "
          "14.2.1 Externs Inference). A test build never runs :advanced, so nothing reads "
          "the hint and nothing would change if it were not there. Remove it. Both "
          "spellings count, ^js and ^{:tag js}, wherever they are written — a parameter, "
          "a let or destructuring binding, or inline on an expression. `^js/Foo` names a "
          "type rather than asking for inference and is left alone, as are `^clj`, "
          "`^boolean` and every other tag. Only test namespaces: a hint in source can be "
          "load-bearing, and only a release build's infer warnings can say which.")
     :var-deref-in-test
     (str "A test reads a var through its var quote — @#'ns/x, (deref #'ns/x), @(var ns/x) "
          "or (deref (var ns/x)). Reaching for the var object rather than calling the thing "
          "couples the test to how the namespace is put together, and a var that is "
          "^:private is reached this way precisely because it was not meant to be. Call it "
          "through the public entry point that uses it, or move it somewhere it can be "
          "public. Only in a namespace whose name ends in -test: a var deref is legal "
          "anywhere, and without an anchor the rule would report production code. The "
          "anchor is the namespace rather than an enclosing deftest, since the reads that "
          "matter sit in a fixture or a helper as often as inside a test. A var quote that "
          "is not "
          "dereferenced is left alone — with-redefs-fn and use-fixtures take one — as is a "
          "deref of anything else, a call to another function whose name merely ends in "
          "deref, and anything inside a quoted or discarded form. Privacy itself is not "
          "checked, since nothing here resolves a var across namespaces, so a public var "
          "read this way is reported too; the fix is the same either way.")})
  (rule->tier [_]
    {:deftest-leading-article :cleanup
     :assertion-message-inline :cleanup
     :conditional-assertion :cleanup
     :var-deref-in-test :cleanup
     :js-hint-in-test :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(defn make-group
  "Return a tests RuleGroup configured with the given map.

  Supported keys:
    :paths [\"test\" …] — directories holding test code. A file under any of them is test
      code whatever its namespace is called. One path may be written on its own rather
      than in a vector. With none set, a namespace whose name ends in `-test` is taken as
      the test, which is what the group does out of the box."
  ([] (make-group nil))
  ([{:keys [paths]}]
   (->TestsGroup (cond
                   (string? paths) [paths]
                   (coll? paths) (vec paths)
                   :else []))))

(def group (make-group))
