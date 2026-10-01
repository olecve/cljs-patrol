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
   :form (str/replace (str/trim (parser/raw is-loc)) #"\s+" " ")
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
   :form (str/replace (str/trim (parser/raw loc)) #"\s+" " ")
   :file file
   :row (parser/position-row loc)
   :hint (get conditional-hints kind)})

(defn- var-deref-finding
  "Build the finding for a var quote that is dereferenced.

  `:form` carries the whole deref so two sites in one file keep separate baseline
  identities, the way the assertion findings do."
  [loc file]
  {:kw (symbol "deref")
   :type :private-var-deref
   :form (str/replace (str/trim (parser/raw loc)) #"\s+" " ")
   :file file
   :row (parser/position-row loc)
   :hint "Test it through the public entry point that calls it, or move it to its own namespace."})

(defn- written-out-var-deref
  "Return the `(deref #'ns/fn)` form written out in full, or nil.

  The reader spelling `@#'ns/fn` is a `:deref` node and reaches [[handle-token]] instead;
  this is the same thing said the long way."
  [loc file]
  (when (and (= "deref" (some-> loc z/down parser/sym-name))
             (= :var (some-> loc z/down z/right z/tag)))
    (var-deref-finding loc file)))

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

(defn- handle-list [loc _ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (when-let [finding (or (leading-article-finding loc file)
                           (when-let [message (inline-assertion-message loc)] (assertion-finding loc message file))
                           (when-let [conditional (conditional-in-assertion loc)]
                             (conditional-finding conditional :computes
                                                  (parser/sym-name (z/down conditional)) file))
                           (when-let [kind (conditional-around-assertion loc)]
                             (conditional-finding loc kind
                                                  (parser/sym-name (z/down loc)) file))
                           (written-out-var-deref loc file))]
      {:decls [finding]
       :usages []
       :dynamics []})))

(defn- handle-token
  "Emit a finding for the `@#'ns/fn` reader spelling of a var deref.

  A var quote is a `:var` node and the `@` around it a `:deref`, so the pair is found from
  the token inside rather than from a list head."
  [loc _ns-info file]
  (let [var-loc (z/up loc)
        deref-loc (some-> var-loc z/up)]
    (when (and (= :var (some-> var-loc z/tag))
               (= :deref (some-> deref-loc z/tag))
               (not (hiccup/inside-unrendered-form? deref-loc)))
      {:decls [(var-deref-finding deref-loc file)]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:deftest-leading-article (vec (filter #(= :deftest-leading-article (:type %)) declarations))
   :assertion-message-inline (vec (filter #(= :assertion-message-inline (:type %)) declarations))
   :conditional-assertion (vec (filter #(= :conditional-assertion (:type %)) declarations))
   :private-var-deref (vec (filter #(= :private-var-deref (:type %)) declarations))})

(defn- summary-lines* [{:keys [deftest-leading-article assertion-message-inline
                               conditional-assertion private-var-deref]}]
  [["Deftest leading article:" (count deftest-leading-article)]
   ["Assertion message inline:" (count assertion-message-inline)]
   ["Conditional assertion:" (count conditional-assertion)]
   ["Private var deref:" (count private-var-deref)]])

(defn- failed?* [_]
  ;; Nothing here blocks at runtime, and the guard shape has defensible uses — a `when`
  ;; narrowing a doseq to the combination under test reads as deliberate. A project that
  ;; wants any of these enforced opts in with --fail-on.
  false)

(defrecord TestsGroup []
  group/RuleGroup
  (group-id [_] :tests)
  (group-name [_] "Tests")
  (parse-handlers [_] {:handle-list handle-list
                       :handle-token handle-token})
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
     :private-var-deref
     (str "A test reads a var through its var quote — @#'some.ns/fn or (deref #'some.ns/fn). "
          "That reaches past a namespace's public surface to something deliberately not "
          "exported, so the test is coupled to an implementation detail and a refactor that "
          "breaks nothing real breaks the test. Test the thing through the public entry "
          "point that calls it, or move it to its own namespace where it can be public. A "
          "plain var quote handed to with-redefs or a fixture is left alone, as is a deref "
          "of an atom or a subscription: only a deref of a var quote is flagged.")})
  (rule->tier [_]
    {:deftest-leading-article :cleanup
     :assertion-message-inline :cleanup
     :conditional-assertion :cleanup
     :private-var-deref :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->TestsGroup))
