(ns cljs-patrol.groups.tests
  "Rule group: conventions of test code."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
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

(defn- holds-assertion? [loc]
  (boolean (some #(and (= :list (z/tag %))
                       (contains? assertion-heads (parser/sym-name (z/down %))))
                 (parser/descendant-locs loc))))

(defn- conditional-in-assertion
  "Return the conditional an assertion computes its expectation with, or nil."
  [loc]
  (when (contains? assertion-heads (some-> loc z/down parser/sym-name))
    (->> loc
         parser/descendant-locs
         (filter #(and (= :list (z/tag %))
                       (contains? conditional-heads (parser/sym-name (z/down %)))
                       (not (inside-function-literal? % loc))))
         first)))

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
          (var-deref-finding deref-loc (parser/enclosing-definition-name deref-loc) file))))))

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
        (js-hint-finding hint-loc (parser/enclosing-definition-name hint-loc) file)))))

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

(def ^:private no-ns-name
  "The namespace `parser` gives a file without an `ns` form, which every such file shares."
  "unknown")

(defn- string-constant-usage
  "Record the var and value of a top-level `(def name \"…\")`, docstring or not, or nil."
  [loc {:keys [ns-name]}]
  (when (and (= "def" (some-> loc z/down parser/sym-name))
             (= :forms (some-> loc z/up z/tag))
             (not= no-ns-name ns-name))
    (let [children (parser/child-locs loc)
          value (last children)
          var-name (some-> (parser/declared-name-loc loc) parser/sym-name)]
      (when (and var-name (literal-string-loc? value))
        {:type ::string-constant
         :var (keyword ns-name var-name)
         :value (z/sexpr value)}))))

(defn- find-literal-and-symbol
  "Split an `(is (= \"…\" sym))`, in either order, into its literal and symbol locs, or nil."
  [is-loc]
  (when (= "is" (some-> is-loc z/down parser/sym-name))
    (let [comparison (some-> is-loc z/down z/right)]
      (when (and (= :list (some-> comparison z/tag))
                 (= "=" (some-> comparison z/down parser/sym-name)))
        (let [[_ left right :as children] (parser/child-locs comparison)]
          (when (= 3 (count children))
            (cond
              (and (literal-string-loc? left) (parser/sym-name right)) [left right]
              (and (literal-string-loc? right) (parser/sym-name left)) [right left])))))))

(defn- strip-to-name
  "Drop a token's leading colons and namespace, so `:label`, `::label` and `msgs/label` all read `label`."
  [token-text]
  (-> token-text (str/replace #"^:+" "") (str/replace #"^[^/]+/(?=.)" "")))

(defn- count-name-in-top-level-form [loc token-name]
  (->> (tree-seq n/inner? n/children (z/node (parser/top-level-loc loc)))
       (filter #(and (= :token (n/tag %)) (= token-name (strip-to-name (n/string %)))))
       count))

(defn- resolve-constant-ref
  "Resolve the var a symbol names, or nil when anything in its top-level form could rebind it.

  A binding, a `with-redefs` or a `binding` writes the name a second time, as a symbol or a
  keyword, qualified or not, so the symbol counts only when its name appears once."
  [sym-loc {:keys [ns-name aliases refers]}]
  (let [sym-raw (parser/raw sym-loc)
        slash (str/index-of sym-raw "/")
        var-name (if slash (subs sym-raw (inc slash)) sym-raw)]
    (when (= 1 (count-name-in-top-level-form sym-loc var-name))
      (if slash
        (let [prefix (subs sym-raw 0 slash)]
          (keyword (get aliases prefix prefix) var-name))
        (keyword (get refers sym-raw ns-name) sym-raw)))))

(defn- literal-constant-usage
  "Record a test-code assertion comparing a literal with a constant, outside `comment` and `quote`, or nil."
  [paths loc ns-info file]
  (when-let [[literal-loc sym-loc] (find-literal-and-symbol loc)]
    (when-let [var-kw (and (test-file? paths ns-info file)
                           (nil? (enclosing-list-head loc #{"comment"}))
                           (not (inside-written-out-quote? loc))
                           (resolve-constant-ref sym-loc ns-info))]
      {:type ::literal-constant-assertion
       :var var-kw
       :literal (z/sexpr literal-loc)
       :kw (symbol "is")
       :form (parser/normalize-form (parser/raw loc))
       :file file
       :row (parser/position-row loc)
       :hint (str (name var-kw) " is defined as this same string, so the assertion repeats its definition. "
                  "Assert on what the code does with it.")})))

(defn- find-list-finding [paths loc ns-info file]
  (or (leading-article-finding loc file)
      (when-let [message (inline-assertion-message loc)] (assertion-finding loc message file))
      (when-let [conditional (conditional-in-assertion loc)]
        (conditional-finding conditional :computes (parser/sym-name (z/down conditional)) file))
      (when-let [kind (conditional-around-assertion loc)]
        (conditional-finding loc kind (parser/sym-name (z/down loc)) file))
      (var-deref-in-test paths loc ns-info file)))

(defn- handle-list [paths loc ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (let [finding (find-list-finding paths loc ns-info file)
          usage (or (string-constant-usage loc ns-info)
                    (literal-constant-usage paths loc ns-info file))]
      (when (or finding usage)
        {:decls (if finding [finding] [])
         :usages (if usage [usage] [])
         :dynamics []}))))

(defn- find-literal-constant-assertions
  "Keep the assertions of `usages` whose literal is the one string their constant is defined as in `usages-in-run`."
  [usages usages-in-run]
  (let [var->values (->> usages-in-run
                         (filter #(= ::string-constant (:type %)))
                         (reduce (fn [acc {:keys [var value]}] (update acc var (fnil conj #{}) value)) {}))]
    (->> usages
         (filter #(= ::literal-constant-assertion (:type %)))
         (filter #(= #{(:literal %)} (var->values (:var %))))
         (map #(dissoc % :type :var :literal))
         vec)))

(defn- analyze* [{:keys [declarations usages usages-in-run]}]
  {:literal-constant-assertion (find-literal-constant-assertions usages (or usages-in-run usages))
   :deftest-leading-article (vec (filter #(= :deftest-leading-article (:type %)) declarations))
   :assertion-message-inline (vec (filter #(= :assertion-message-inline (:type %)) declarations))
   :conditional-assertion (vec (filter #(= :conditional-assertion (:type %)) declarations))
   :var-deref-in-test (vec (filter #(= :var-deref-in-test (:type %)) declarations))
   :js-hint-in-test (vec (filter #(= :js-hint-in-test (:type %)) declarations))})

(defn- summary-lines* [{:keys [deftest-leading-article assertion-message-inline
                               conditional-assertion var-deref-in-test js-hint-in-test
                               literal-constant-assertion]}]
  [["Deftest leading article:" (count deftest-leading-article)]
   ["Assertion message inline:" (count assertion-message-inline)]
   ["Conditional assertion:" (count conditional-assertion)]
   ["Var deref in test:" (count var-deref-in-test)]
   ["Js hint in test:" (count js-hint-in-test)]
   ["Literal constant assertion:" (count literal-constant-assertion)]])

(defn- failed?* [_]
  ;; Nothing here blocks at runtime, and the guard shape has defensible uses — a `when`
  ;; narrowing a doseq to the combination under test reads as deliberate. A project that
  ;; wants any of these enforced opts in with --fail-on.
  false)

(defrecord TestsGroup [paths extensions]
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
     (str "A test name opens with \"a-\", \"an-\" or \"the-\". Drop the article: "
          "the-panel-opens becomes panel-opens.")
     :assertion-message-inline
     (str "An assertion message shares a line with the expression it describes. "
          "Move the message to its own line under the expression.")
     :conditional-assertion
     (str "A test works out what to expect while it runs, with a conditional in the expected value, "
          "around assertions, or as a guard that can skip them. Write a table of inputs and expected "
          "values and walk it with doseq.")
     :js-hint-in-test
     (str "A ^js type hint in a test namespace does nothing, because a test build never runs "
          ":advanced optimizations. Remove it.")
     :literal-constant-assertion
     (str "An assertion compares a string literal with the constant defined as that same string, so it repeats the "
          "definition. Assert on what the code does with the constant instead.")
     :var-deref-in-test
     (str "A test reads a var through its var quote, such as @#'ns/x or (deref (var ns/x)). "
          "Call it through the public function that uses it, or move it where it can be public.")})
  (docs-anchors [_]
    {:assertion-message-inline "assertion-message-inline"
     :conditional-assertion "conditional-assertion"
     :deftest-leading-article "deftest-leading-article"
     :js-hint-in-test "js-hint-in-test"
     :literal-constant-assertion "literal-constant-assertion"
     :var-deref-in-test "var-deref-in-test"})
  (rule->tier [_]
    {:deftest-leading-article :cleanup
     :assertion-message-inline :cleanup
     :conditional-assertion :cleanup
     :var-deref-in-test :cleanup
     :js-hint-in-test :cleanup
     :literal-constant-assertion :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return a tests RuleGroup configured with the given map.

  Supported keys:
    :paths [\"test\" …] — directories holding test code. A file under any of them is test
      code whatever its namespace is called. One path may be written on its own rather
      than in a vector. With none set, a namespace whose name ends in `-test` is taken as
      the test, which is what the group does out of the box.

  `clj?` is the experimental `.clj` mode. It is a separate argument rather than a key in
  the map because the map comes straight from the config file and may be anything the
  user wrote there."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([{:keys [paths]} clj?]
   (->TestsGroup (cond
                   (string? paths) [paths]
                   (coll? paths) (vec paths)
                   :else [])
                 (group/extensions-for clj?))))

(def group (make-group))
