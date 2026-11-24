(ns cljs-patrol.integration.tests-naming-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.groups.tests :as tests]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test-projects/tests-app/src/webapp")

(defn- result []
  (-> (core/run (disk/typed-path fixture-dir) [tests/group]) :group-results first))

(defn- findings []
  (:deftest-leading-article (result)))

(deftest deftest-leading-article-fixture-test
  (let [found (findings)
        by-name (into {} (map (juxt (comp str :kw) identity)) found)]

    (testing "flags a name opening with an article"
      (is (contains? by-name "the-status-label-is-rendered"))
      (is (contains? by-name "a-viewer-cannot-change-relevance"))
      (is (contains? by-name "an-empty-list-renders-a-hint")))

    (testing "reads the name through metadata on the var"
      (is (contains? by-name "the-panel-loads-lazily")
          "^:async sits between deftest and the name"))

    (testing "an article inside the sentence is grammar, not noise"
      (is (not (contains? by-name "escape-closes-the-modal")))
      (is (not (contains? by-name "without-on-select-the-row-is-not-a-button"))))

    (testing "a word that merely starts with those letters is not an article"
      (is (not (contains? by-name "another-claim-holds"))
          "\"another\" opens with an, and the hyphen is what tells them apart")
      (is (not (contains? by-name "theme-switches-to-dark"))
          "\"theme\" opens with the"))

    (testing "leaves a name carrying no article alone"
      (is (not (contains? by-name "status-label-is-rendered")))
      (is (not (contains? by-name "divide-test"))))

    (testing "names the article and what the name becomes without it"
      (is (= "Drop the leading \"the-\": status-label-is-rendered"
             (:hint (by-name "the-status-label-is-rendered")))))

    (testing "flags exactly the bad names"
      (is (= 4 (count found))
          "four names open with an article in the fixture, no more"))))

(deftest baseline-identity-test
  (testing "a test var is identified by its name and file, not its row"
    (let [issue (first (findings))
          identity-of (fn [row] (baseline/issue->identity :deftest-leading-article
                                                          (assoc issue :row row)
                                                          (disk/path fixture-dir)))]
      (is (= (identity-of 26) (identity-of 94))
          "the row moves whenever anything above the var does")
      (is (= #{:rule :name :file} (set (keys (identity-of 26))))))))

(deftest assertion-message-inline-fixture-test
  (let [found (:assertion-message-inline (result))
        by-row (into {} (map (juxt :row identity)) found)]

    (testing "flags a message sharing the expression's line"
      (is (contains? by-row 41)
          "a single-line expression with the message bolted on the end"))

    (testing "flags a message on the closing line of a multi-line expression"
      (is (contains? by-row 49)
          "the expression spans lines, but the message still shares its last one"))

    (testing "leaves a message on its own line alone"
      (is (not (contains? by-row 39)))
      (is (not (contains? by-row 46))
          "a multi-line expression whose message follows it on a new line"))

    (testing "says nothing about an assertion carrying no message"
      (is (not (contains? by-row 42))))

    (testing "only a string written out is the shape this is about"
      (is (not (contains? by-row 44))
          "a message named by a symbol is left alone"))

    (testing "names what to do"
      (is (= "Move the message to its own line, under the expression."
             (:hint (by-row 41)))))

    (testing "flags exactly the inline messages"
      (is (= 2 (count found))
          "two of the six assertions in the fixture share a line"))))

(deftest conditional-assertion-fixture-test
  (let [found (:conditional-assertion (result))
        by-row (into {} (map (juxt :row identity)) found)]

    (testing "flags an expected value computed by a conditional"
      (is (contains? by-row 58))
      (is (= "The expected value is computed here. State it instead, one case per row of a table."
             (:hint (by-row 58)))))

    (testing "flags a conditional choosing between assertions"
      (is (contains? by-row 60))
      (is (= "The test chooses between assertions. Pair each input with its expected value instead."
             (:hint (by-row 60)))))

    (testing "flags a guard that can skip the assertion"
      (is (contains? by-row 63))
      (is (= "This guard can skip the assertion, and the test then passes having checked nothing."
             (:hint (by-row 63)))))

    (testing "a conditional inside a function literal is a predicate"
      (is (not (contains? by-row 53))
          "#(when (= 1 (:id %)) %) handed to some branches per element, not per assertion"))

    (testing "leaves an assertion stating its expectation alone"
      (is (not (contains? by-row 56))))

    (testing "reports the row in the file, not in the form"
      (is (every? #(> (:row %) 1) found)
          "a z/subzip walk would name line 1 of the assertion instead"))

    (testing "flags exactly the three shapes"
      (is (= 3 (count found))))))

(deftest assertion-identity-separates-same-message-test
  (testing "two assertions sharing a message but not an expression stay apart in a baseline"
    (let [finding (fn [expression]
                    {:kw (symbol "is")
                     :form (str "(is " expression " \"the row is shown\")")
                     :file "src/views_test.cljs"
                     :row 10})
          identity-of #(baseline/issue->identity :assertion-message-inline (finding %))]
      (is (not= (identity-of "(.-checked checkbox-1)")
                (identity-of "(.-checked checkbox-3)"))
          "the identity keys on the whole assertion, not the message alone")
      (is (= (identity-of "(.-checked checkbox-1)")
             (identity-of "(.-checked checkbox-1)"))
          "the same assertion twice in one file is one identity, which no stable key can split"))))

(deftest assertion-identity-survives-a-reformat-test
  (testing "re-wrapping the expression across lines does not change the identity"
    (let [identity-of (fn [form row]
                        (baseline/issue->identity :assertion-message-inline
                                                  {:kw (symbol "is")
                                                   :form form
                                                   :file "src/views_test.cljs"
                                                   :row row}))]
      (is (= (identity-of "(is (= 2 (block-count surface)) \"the document reached the editor\")" 10)
             (identity-of "(is (= 2\n         (block-count surface))\n      \"the document reached the editor\")" 10))
          "whitespace is collapsed when the identity is built, not only when the finding is")
      (is (= (identity-of "(is (nil? (palette)) \"gone\")" 10)
             (identity-of "(is (nil? (palette)) \"gone\")" 94))
          "the row is not part of it, so a line moving above the finding changes nothing")
      (is (not-any? #{:row :line} (keys (identity-of "(is x \"m\")" 10)))
          "no row-shaped key reaches the stored identity"))))

(deftest var-deref-in-test-fixture-test
  (let [found (:var-deref-in-test (result))
        by-row (into {} (map (juxt :row identity)) found)]

    (testing "reads every spelling of a var quote through a deref"
      (is (contains? by-row 71)
          "@#'ns/x, the reader spelling")
      (is (contains? by-row 73)
          "@(var ns/x), the var written out")
      (is (contains? by-row 75)
          "(deref #'ns/x), the deref written out")
      (is (contains? by-row 77)
          "@^:tag #'ns/x, metadata between the two"))

    (testing "reports one finding per deref, not one per token"
      (is (= 1 (count (filter #(= 77 (:row %)) found)))
          "a metadata node holds two tokens that climb to the same var"))

    (testing "reads a helper beside the tests, where most real reads sit"
      (is (some #(= "reads-a-var-in-a-helper" (str (:test %))) found)
          "anchoring on deftest would miss the fixtures and helpers in a test namespace"))

    (testing "says nothing in a production namespace"
      (is (not-any? #(str/ends-with? (:file %) "views.cljs") found)
          "webapp.views does not end in -test, so the rule does not run there"))

    (testing "leaves a var quote that is not dereferenced alone"
      (is (not (contains? by-row 81))
          "with-redefs-fn takes var quotes, and that is how a test replaces a dependency"))

    (testing "leaves a deref that is not of a var alone"
      (is (not (contains? by-row 84))
          "an atom")
      (is (not (contains? by-row 86))
          "a function whose name merely ends in deref"))

    (testing "says nothing about markup that does not render"
      (is (not (contains? by-row 88))
          "a written-out (quote …)"))

    (testing "names the test each finding sits in"
      (is (= "var-deref-spellings" (str (:test (by-row 71))))))

    (testing "flags exactly the four spellings"
      (is (= 5 (count found))
          "four spellings in the tests, plus the one in the helper"))))

(deftest var-deref-identity-separates-two-tests-test
  (testing "the same var read in two tests of one file keeps two identities"
    (let [identity-of (fn [test-name]
                        (baseline/issue->identity :var-deref-in-test
                                                  {:kw (symbol "deref")
                                                   :form "@#'other.ns/seen-ids"
                                                   :test test-name
                                                   :file "src/views_test.cljs"
                                                   :row 10}))]
      (is (not= (identity-of "reads-it-once") (identity-of "reads-it-again"))
          "the form alone is just a var name, so the test it sits in is what tells them apart")
      (is (= (identity-of "reads-it-once") (identity-of "reads-it-once"))
          "two reads in one test still collapse, which no stable key can split"))))

(deftest js-hint-in-test-fixture-test
  (let [all-found (:js-hint-in-test (result))
        found (filter #(str/ends-with? (:file %) "naming_test.cljs") all-found)
        by-row (frequencies (map :row found))]

    (testing "flags a hint that no interop call reads, wherever it is written"
      (is (= 1 (by-row 93))
          "a map destructuring key read only by a keyword lookup")
      (is (= 2 (by-row 94))
          "both hints in a vector destructuring, read by nothing")
      (is (= 1 (by-row 95))
          "the ^{:tag js} spelling")
      (is (= 1 (by-row 132))
          "a let binding read only by a plain call")
      (is (= 1 (by-row 138))
          "inline on an argument of a plain call")
      (is (= 1 (by-row 145))
          "a defn parameter read only by a plain call")
      (is (= 1 (by-row 92))
          "a let binding whose only read carries its own ^js"))

    (testing "leaves a hint that an interop call reads, because it stops an :infer-warning"
      (is (not (by-row 96))
          "inline on the target of .-textContent")
      (is (not (by-row 97))
          "inline on the target of ..")
      (is (not (by-row 100))
          "a defn parameter read by .-value")
      (is (not (by-row 128))
          "a let binding read by a method call")
      (is (not (by-row 129))
          "a let binding threaded into a property read")
      (is (not (by-row 130))
          "a let binding that doto passes to a method call")
      (is (not (by-row 131))
          "a let binding read through the . special form")
      (is (not (by-row 140))
          "a hint on the defined name, which callers read")
      (is (not (by-row 148))
          "a hint on an arglist, which callers read")
      (is (not (by-row 151))
          "a let binding read by a method call inside a #() literal")
      (is (not (by-row 152))
          "inline on the target of a property read inside a #() literal")
      (is (not (by-row 143))
          "a this-as binding read by .."))

    (testing "leaves every other tag alone"
      (is (not (by-row 109))
          "^js/Foo names a type rather than asking for inference")
      (is (not (by-row 110))
          "^clj")
      (is (not (by-row 111))
          "^boolean"))

    (testing "says nothing about text that merely reads like a hint"
      (is (not-any? #(= "\"^js\"" (:form %)) found)
          "a string containing ^js")
      (is (not (by-row 101))
          "a comment containing ^js"))

    (testing "says nothing about a form that does not render"
      (is (not (by-row 105))
          "a written-out (quote …)")
      (is (not (by-row 124))
          "a discarded deftest"))

    (testing "the symbol js is only a hint in the tag slot"
      (is (not (by-row 116))
          "^:private js — js is the value, not the tag")
      (is (not (by-row 117))
          "^{:doc js} — js is a map value under :doc, not :tag"))

    (testing "says nothing in a production namespace"
      (is (not-any? #(str/ends-with? (:file %) "views.cljs") all-found)
          "a hint in source can be load-bearing"))

    (testing "names the hint and what to do"
      (is (= "Remove the hint — no interop call reads this value, so shadow-cljs has nothing to infer."
             (:hint (first found)))))

    (testing "flags exactly the unread hints"
      (is (= 8 (count found))))))

(deftest js-hint-in-test-reading-shapes-test
  (let [found (filter #(str/ends-with? (:file %) "js_hints_test.cljs") (:js-hint-in-test (result)))
        flagged (set (map (comp str :test) found))]

    (testing "leaves a hint that shadow-cljs reads"
      (doseq [[definition shape] [["stacked-def" "a return hint under ^:private"]
                                  ["stacked-defn" "a defn return hint under ^:private"]
                                  ["multi-arity" "arglist hints of a multi-arity defn"]
                                  ["reify-fake" "a reify method parameter read by .-type"]
                                  ["FakeNode" "a deftype field read by .-x"]
                                  ["thread-last-read" "->> with a bare interop step"]
                                  ["some-thread-read" "some-> with an interop step"]
                                  ["cond-thread-read" "cond-> with an interop step"]
                                  ["as-thread-read" "an as-> name read by .-x"]
                                  ["doto-later-step" "doto where the interop step is not the first"]
                                  ["alias-read" "a local bound to the hinted local, read by .-x"]
                                  ["as-thread-inline-read" "inline on the value an as-> name takes"]
                                  ["cond-thread-last-read" "cond->> with a bare interop step"]
                                  ["destructured-read" "a destructured key read outside its map"]
                                  ["inline-let-value-read" "inline on a let value whose name .-pendingOp reads"]
                                  ["init-def" "inline on a def value whose var .-pendingOp reads"]
                                  ["interop-call-result-read" "a hint on an interop call whose bound result is read"]
                                  ["MutableNode" "a deftype field hint written before ^:mutable"]
                                  ["catch-read" "a catch name read by .-response"]
                                  ["thread-last-call-step-read" "->> with an interop call step and no arguments"]
                                  ["cond-thread-last-call-step-read" "cond->> with an interop call step"]
                                  ["comment-hint" "a hint inside a (comment …) block"]
                                  ["unquoted-read" "a read under ~ inside a syntax-quote"]
                                  ["doseq-let-modifier-read" "a doseq :let name read by .-pendingOp"]
                                  ["letfn-param-read" "a letfn param read by .-type"]
                                  ["handle-pair" "a defmethod param after a vector dispatch value"]
                                  ["discarded-binding-shift" "a binding after a discarded #_#_ pair"]
                                  ["tail-hint" "the last form of a defn body, which types its return"]
                                  ["branch-hints" "if branches whose result a name takes"]
                                  ["as-thread-name-hint" "a hint on the as-> name"]
                                  ["thread-into-macro-step" "a value threaded into a doto step"]
                                  ["library-let-read" "a binding of a project's own let macro"]
                                  ["doc-def" "a def value after a docstring"]
                                  ["let-value-symbol" "inline on a symbol a let binds to a read name"]
                                  ["project-doto" "a project's own m/doto, which may thread like doto"]
                                  ["are-binding-read" "a value are pastes into an interop read"]
                                  ["are-value-read" "inline on a value are pastes into an interop read"]
                                  ["make-multi" "the last form of each arity"]
                                  ["letfn-tail-read" "the last form of a letfn function"]
                                  ["schema-load" "an arglist hint of a project's own m/defn"]
                                  ["async-name-read" "the name async binds"]
                                  ["namespaced-keys-read" "a namespaced :keys entry, which binds the bare name"]
                                  ["with-redefs-tail-read" "the last form of with-redefs"]
                                  ["testing-tail-read" "the last form of testing"]
                                  ["project-with-macro-tail-read" "the last form of a project's own with- macro"]
                                  ["time-tail-read" "the last form of time, a core macro"]
                                  ["macro-alias-read" "an argument of a :require-macros alias"]
                                  ["referred-macro-read" "an argument of a :refer-macros name"]
                                  ["cljs-only-macro-read" "a binding of simple-benchmark, a cljs-only macro"]
                                  ["nil-tag-read" "a read whose own ^{:tag nil} gives no tag"]]]
        (is (not (contains? flagged definition))
            shape)))

    (testing "leaves a hint in a shape it does not follow, though nothing reads it"
      (doseq [[definition shape] [["handle-unread-return" "a defmethod arglist"]
                                  ["inline-fn-return" "an inline fn arglist"]
                                  ["hint-on-interop-call" "a let value whose name only a plain call reads"]
                                  ["hinted-vector-value" "a def's vector value"]
                                  ["vector-literal-use" "a use in a vector right after a head"]
                                  ["body-vector-not-alias" "a body vector under let"]
                                  ["dynamic-binding" "a value given to binding"]
                                  ["as-thread-rebound" "a value an as-> name takes"]
                                  ["hinted-defonce-value" "a defonce vector value"]
                                  ["body-vector" "a vector in a defn body"]
                                  ["rebound-name-not-read" "a name bound again"]]]
        (is (not (contains? flagged definition))
            shape)))

    (testing "flags a hint that nothing reads"
      (doseq [[definition shape] [["inline-arg-hint" "an inline hint on a plain call argument"]
                                  ["read-only-when-discarded" "a read only inside #_"]
                                  ["read-only-in-comment" "a read only inside (comment …)"]
                                  ["namespaced-namesake" "other/el, which is not the local el"]
                                  ["read-with-its-own-map-tag" "a binding whose only read carries ^{:tag js}"]
                                  ["read-with-its-own-named-tag" "a binding whose only read carries ^js/HTMLElement"]
                                  ["interop-argument-only" "a binding read only as an interop argument, not its target"]
                                  ["js-literal-only" "a param read only as a #js literal value"]
                                  ["discarded-before-unread-binding" "a binding after #_, read only by a plain call"]
                                  ["catch-unread" "a catch name read only by a plain call"]]]
        (is (contains? flagged definition)
            shape)))

    (testing "flags exactly those"
      (is (= 10 (count found))))))

(deftest js-hint-in-test-reader-conditional-test
  (let [flagged (->> (:js-hint-in-test (result))
                     (filter #(str/ends-with? (:file %) "js_hints_cljc_test.cljc"))
                     (map (comp str :test))
                     set)]
    (is (= #{"clj-branch-read" "cljr-branch-read" "branch-use-unread"} flagged)
        "a read only in a branch for another platform, which shadow-cljs never compiles, under :refer :all")
    (is (not (contains? flagged "default-beside-cljs-read"))
        "the reader takes :default when it comes first")
    (is (not (contains? flagged "custom-feature-read"))
        "a build may add :node to its reader features")
    (is (not (contains? flagged "branch-use-read"))
        "a use in the :cljs branch is where the conditional stands")
    (is (not (contains? flagged "absent-branch-before-target"))
        "a :clj-only conditional reads as nothing, so el stays the interop target")))

(def ^:private paths-fixture-dir "test-projects/tests-paths-app")

(defn- paths-result [paths]
  (-> (core/run (disk/typed-path paths-fixture-dir) [(tests/make-group {:paths paths})])
      :group-results first :js-hint-in-test))

(deftest test-paths-config-test
  (testing "with no paths configured the namespace name decides"
    (let [found (-> (core/run (disk/typed-path paths-fixture-dir) [(tests/make-group)])
                    :group-results first :js-hint-in-test)]
      (is (empty? found)
          "neither fixture namespace ends in -test, so neither is read as test code")))

  (testing "a configured path decides instead of the namespace"
    (let [found (paths-result ["spec"])]
      (is (= 2 (count found))
          "spec/ and src/spec/ both hold that segment, and neither namespace ends in -test")
      (is (some #(str/ends-with? (:file %) "widget_spec.cljs") found))
      (is (some #(str/ends-with? (:file %) "nested_spec.cljs") found))))

  (testing "a path matches whole segments, not substrings"
    (is (empty? (paths-result ["late"]))
        "src/latest/ holds late as a prefix, which is not a segment")
    (is (= 1 (count (paths-result ["latest"])))
        "the whole segment does match"))

  (testing "every configured path counts, not only the first"
    (is (= 3 (count (paths-result ["spec" "latest"])))
        "the union of both, where each alone matches fewer"))

  (testing "one path may be written on its own"
    (is (= (map :file (paths-result ["latest"]))
           (map :file (-> (core/run (disk/typed-path paths-fixture-dir) [(tests/make-group {:paths "latest"})])
                          :group-results first :js-hint-in-test)))
        "a bare string reads as a one-element vector"))

  (testing "a path written as several segments matches that run"
    (is (= 1 (count (paths-result ["src/spec"])))
        "src/spec matches only the nested one, where plain spec matched both")
    (is (empty? (paths-result ["spec/src"]))
        "a run of segments has to match in order")))
