(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [clojure.java.io]
   [clojure.java.shell]
   [clojure.test :refer [deftest is]]))

(def ^:private source
  (str "(ns app.core)\n"
       "(defn ^:private visible? [x] (when (seq x) true))\n"
       "(defn count-up [n] (comment (when n 1)) (inc n))\n"
       "(comment (when x y))\n"
       "#_(defn gone [] (when x y))\n"
       "(def quoted '(when x y))\n"
       "(def plain {:a 1})\n"
       "(def ^{:private true} hidden 1)\n"
       "(defonce cache (when x y))\n"
       "(extend-type Foo P (m [x] (when x 1)))\n"
       "(defn hinted [xs] ^String (first xs))\n"
       "(defn attrs {:private true} [] (if x 1 2))\n"
       "(defn platform [] #?(:clj (when x 1) :cljs (when y 2)))\n"
       "(defn body-map [] {:a (if x 1 2)})\n"
       "(defn two-lines [x]\n"
       "  (when x 1))\n"))

(defn- sample-ns
  "Create a namespace that refers clojure.core, as a file's ns form would."
  [ns-sym]
  (binding [*ns* (create-ns ns-sym)]
    (refer-clojure))
  ns-sym)

(defn- swaps-of [mutants] (mapv (juxt :def :row :from #(pr-str (:to %))) mutants))

(deftest mutants-swap-each-operator-of-each-definition
  (is (= [["visible?" 2 "when" "when-not"]
          ["visible?" 2 "seq" "empty?"]
          ["visible?" 2 "true" "false"]
          ["count-up" 3 "inc" "dec"]
          ["hinted" 11 "first" "last"]
          ["attrs" 12 "if" "if-not"]
          ["platform" 13 "when" "when-not"]
          ["body-map" 14 "if" "if-not"]
          ["two-lines" 16 "when" "when-not"]]
         (swaps-of (mutate/mutants (mutate/top-level-forms source))))
      (str "ns form, comment, discarded, quoted, metadata, attr map and cljs-branch code skipped; a hinted form"
           " mutated; forms loaded once and other forms left out")))

(deftest scope-names-what-is-not-mutated-and-unknown-definitions
  (let [{:keys [loaded-once other-forms]} (mutate/scope source {})]
    (is (= ["cache (defonce)"] loaded-once))
    (is (= ["extend-type@10"] other-forms)))
  (is (= ["count-up"] (:defs-in-scope (mutate/scope source {:lines #{3}})))
      "changed lines keep only definitions they touch")
  (let [{:keys [defs-in-scope selected]} (mutate/scope source {:lines #{15}})]
    (is (= ["two-lines"] defs-in-scope)
        "one changed line of several brings definition in")
    (is (empty? selected)
        "mutant on unchanged line of changed definition is left out"))
  (is (= [16] (map :row (:selected (mutate/scope source {:lines #{16}}))))
      "mutant on changed line is kept")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No top-level definition named nope"
                        (mutate/scope source {:defs ["nope"]}))))

(deftest changed-lines-read-git-diff-hunks
  (is (= #{3 7 8 9 11}
         (mutate/changed-lines (str "@@ -3 +3 @@\n-old\n+new\n"
                                    "@@ -10,0 +7,3 @@\n+a\n+b\n+c\n"
                                    "@@ -20,2 +11,0 @@\n-x\n-y\n")))
      "one changed line, three added lines, and the line a deletion follows"))

(deftest summary-counts-each-definition-and-names-the-rest
  (let [summary (mutate/summarize {:defs-in-scope ["a" "b" "c" "e"]
                                   :results [{:def "a"
                                              :outcome :failed}
                                             {:def "a"
                                              :outcome :passed}
                                             {:def "b"
                                              :outcome :unviable}
                                             {:def "b"
                                              :outcome :timeout}]
                                   :loaded-once ["d (defmulti)"]
                                   :unreached [{:def "e"}]})]
    (is (= [{:def "a"
             :mutants 2
             :killed 1
             :survived 1
             :no-coverage 0
             :unviable 0
             :timeout 0}
            {:def "b"
             :mutants 2
             :killed 0
             :survived 0
             :no-coverage 0
             :unviable 1
             :timeout 1}]
           (:definitions summary)))
    (is (= ["c"] (:not-mutated summary))
        "definition with no mutant is named, one never reached is not")
    (is (= [{:def "e"
             :mutants 1}]
           (:unreached summary)))
    (is (== 1/2 (rationalize (:score summary)))
        "unviable and timed-out mutants are not scored")))

(deftest load-outcome-tells-unviable-from-killed
  (sample-ns 'cljs-patrol.dev.mutate-test.load)
  (try
    (is (= :unviable (mutate/load-outcome #(mutate/load-text 'cljs-patrol.dev.mutate-test.load "(foo")))
        "unreadable")
    (is (= :unviable (mutate/load-outcome #(mutate/load-text 'cljs-patrol.dev.mutate-test.load
                                                             "(defn f [x] (case x true :a true :b))")))
        "uncompilable")
    (is (= :failed (mutate/load-outcome #(mutate/load-text 'cljs-patrol.dev.mutate-test.load "(def x (/ 1 0))")))
        "throws while it loads")
    (is (nil? (mutate/load-outcome #(mutate/load-text 'cljs-patrol.dev.mutate-test.load "(def y 1)"))))
    (finally (remove-ns 'cljs-patrol.dev.mutate-test.load))))

(deftest reader-conditionals-load-as-clojure
  (sample-ns 'cljs-patrol.dev.mutate-test.sample)
  (try
    (mutate/load-text 'cljs-patrol.dev.mutate-test.sample "(def platform #?(:clj :jvm :cljs :js))")
    (is (= :jvm @(ns-resolve 'cljs-patrol.dev.mutate-test.sample 'platform)))
    (finally (remove-ns 'cljs-patrol.dev.mutate-test.sample))))

(def ^:private validation-tests
  "A test namespace whose only assertion catches every exception, as `(thrown? Throwable …)` does."
  (str "(ns cljs-patrol.dev.mutate-test.thrown (:require [clojure.test :refer [deftest is]]))\n"
       "(def validate (fn [x] (when-not (pos? x) (throw (ex-info \"no\" {})))))\n"
       "(deftest rejects-zero (is (thrown? Throwable (validate 0))))\n"))

(deftest failure-inside-thrown-assertion-still-kills
  (sample-ns 'cljs-patrol.dev.mutate-test.thrown)
  (try
    (mutate/load-text 'cljs-patrol.dev.mutate-test.thrown validation-tests)
    (is (= :passed (:outcome (mutate/run-tests ['cljs-patrol.dev.mutate-test.thrown] 10000))))
    (mutate/load-text 'cljs-patrol.dev.mutate-test.thrown "(def validate (fn [_] nil))")
    (is (= :failed (:outcome (mutate/run-tests ['cljs-patrol.dev.mutate-test.thrown] 10000)))
        "thrown? assertion that fails is recorded before its catch can swallow it")
    (finally (remove-ns 'cljs-patrol.dev.mutate-test.thrown))))

(def ^:private wrapped-source
  (str "(ns app.wrapped)\n"
       "^:foo (defn tagged [] (when q 1))\n"
       "#?(:clj (defn on-clj [] (when r 1)))\n"
       "#?(:cljs (defn on-cljs [] (when r 1)))\n"
       "(defn spliced [] #?@(:cljs [(when y 1)] :clj [(if z 1 2)]))\n"
       "(defn defaulted [] #?(:clj (when a 1) :default (when b 2)))\n"
       "(def table {:a (when x 1)})\n"
       "#?@(:cljs [(defn cljs-only [] (when x 1))] :clj [(defn spliced-a [] (when x 1)) (defn spliced-b [] (if x 1 2))])\n"))

(deftest mutants-read-forms-as-clojure-loads-them
  (is (= [["tagged" 2 "when" "when-not"]
          ["on-clj" 3 "when" "when-not"]
          ["spliced" 5 "if" "if-not"]
          ["defaulted" 6 "when" "when-not"]
          ["table" 7 "when" "when-not"]
          ["spliced-a" 8 "when" "when-not"]
          ["spliced-b" 8 "if" "if-not"]]
         (swaps-of (mutate/mutants (mutate/top-level-forms wrapped-source))))
      (str "definition under metadata or in a clj branch kept; only branch Clojure reads mutated; a def's value"
           " map is no attribute map")))

(def ^:private target-source
  (str "(ns cljs-patrol.dev.mutate-test.target)\n"
       "(defn ^:checked positive? [x] (> x 0))\n"
       "(defn label [x] (if (positive? x) :up :down))\n"
       "(defn unused [x] (when x 1))\n"))

(def ^:private target-tests
  (str "(ns cljs-patrol.dev.mutate-test.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate-test.target :as target]))\n"
       "(deftest labels (is (= :up (target/label 1))))\n"))

(defn- with-target
  ([tests f] (with-target target-source tests f))
  ([source tests f]
   (sample-ns 'cljs-patrol.dev.mutate-test.target)
   (sample-ns 'cljs-patrol.dev.mutate-test.target-test)
   (try
     (mutate/load-text 'cljs-patrol.dev.mutate-test.target source)
     (mutate/load-text 'cljs-patrol.dev.mutate-test.target-test tests)
     (f)
     (finally
       (remove-ns 'cljs-patrol.dev.mutate-test.target)
       (remove-ns 'cljs-patrol.dev.mutate-test.target-test)))))

(defn- run-target
  ([options] (run-target target-source options))
  ([source options]
   (binding [*out* (java.io.StringWriter.)]
     (mutate/mutate-source 'cljs-patrol.dev.mutate-test.target "target.clj" source
                           ['cljs-patrol.dev.mutate-test.target-test] options))))

(deftest run-kills-and-keeps-mutants-and-restores-the-namespace
  (with-target target-tests
    (fn []
      (let [{:keys [totals not-covered]} (run-target {})]
        (is (= ["unused"] not-covered))
        (is (= {:mutants 3
                :killed 1
                :survived 1
                :no-coverage 1
                :unviable 0
                :timeout 0}
               totals)
            "boundary at 0 is never tested, so > -> >= survives; no test calls unused"))
      (is (true? (:checked (meta (ns-resolve 'cljs-patrol.dev.mutate-test.target 'positive?)))))
      (is (= :up ((ns-resolve 'cljs-patrol.dev.mutate-test.target 'label) 1))
          "original code is back after the run"))))

(deftest failing-tests-stop-the-run-before-any-mutation
  (with-target (str target-tests "(deftest broken (is (= 1 2)))\n")
    (fn []
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The tests fail before any mutation"
                            (run-target {:timeout 5000}))
          "also with a timeout given"))))

(deftest exit-code-tells-survivors-from-a-stopped-run
  (is (= 0 (mutate/exit-code {:totals {:survived 0}})))
  (is (= 1 (mutate/exit-code {:totals {:survived 2}})))
  (is (= 2 (mutate/exit-code {:totals {:survived 2}
                              :stopped "looping"}))))

(defn- sh! [dir & args]
  (let [{:keys [exit err]} (apply clojure.java.shell/sh (concat args [:dir dir]))]
    (assert (zero? exit) err)))

(deftest lines-since-reads-the-branch-changes-from-the-merge-base
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "mutate" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (clojure.java.io/file dir "core.clj")]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "user.email" "t@example.com")
    (sh! dir "git" "config" "user.name" "t")
    (spit file "a\nb\nc\n")
    (sh! dir "git" "add" ".")
    (sh! dir "git" "commit" "-q" "-m" "base")
    (sh! dir "git" "switch" "-q" "-c" "topic")
    (spit file "a\nB\nc\n")
    (is (= #{2} (mutate/lines-since (str file) "main")))
    (spit (clojure.java.io/file dir "fresh.clj") "x\n")
    (is (= :all-lines (mutate/lines-since (str (clojure.java.io/file dir "fresh.clj")) "main"))
        "file the merge base lacks counts in full")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No git commit named nope"
                          (mutate/lines-since (str file) "nope")))))

(deftest coverage-records-calls-made-on-worker-threads
  (with-target (str target-tests
                    "(deftest on-thread (let [p (promise)]"
                    " (.start (Thread. #(deliver p (target/label 0))))"
                    " (is (= :down @p))))\n")
    (fn []
      (let [{:keys [tests recorded]} (mutate/coverage 'cljs-patrol.dev.mutate-test.target
                                                      ["positive?" "label" "unused"]
                                                      ['cljs-patrol.dev.mutate-test.target-test]
                                                      10000)
            test-var #(ns-resolve 'cljs-patrol.dev.mutate-test.target-test %)]
        (is (= #{"positive?" "label" "unused"} recorded))
        (is (= #{(test-var 'labels) (test-var 'on-thread)} (get tests "label"))
            "call on a raw thread is recorded")
        (is (nil? (get tests "unused")))
        (is (= :up ((ns-resolve 'cljs-patrol.dev.mutate-test.target 'label) 1))
            "vars are restored")
        (is (fn? (:test (meta (test-var 'labels))))
            "tests are restored")))))

(deftest killed-mutant-shows-its-definition-is-reached
  (is (= [:killed :survived]
         (map :outcome (:mutants (mutate/summarize {:defs-in-scope ["a"]
                                                    :results [{:def "a"
                                                               :outcome :failed}
                                                              {:def "a"
                                                               :outcome :no-coverage}]}))))
      "passing mutant of a definition a test reached is a survivor, not uncovered"))

(deftest exit-code-counts-mutants-without-coverage
  (is (= 1 (mutate/exit-code {:totals {:survived 0
                                       :no-coverage 1}}))))

(defmulti ^:private dispatcher identity)

(defn- counter ^long [^long n] (inc n))

(defn- partly-hinted
  ([^long n _] n)
  ([x] x))

(deftest wrappable-means-a-plain-function
  (is (true? (mutate/wrappable? #'mutate/exit-code)))
  (is (false? (mutate/wrappable? #'dispatcher))
      "multimethod")
  (is (false? (mutate/wrappable? #'counter))
      "primitive hint")
  (is (false? (mutate/wrappable? #'partly-hinted))
      "primitive hint on one parameter of one arity")
  (is (false? (mutate/wrappable? #'source))
      "data"))

(deftest not-covered-definitions-and-covered-score
  (let [{:keys [not-covered score covered-score]}
        (mutate/summarize {:defs-in-scope ["a" "b"]
                           :results [{:def "a"
                                      :outcome :failed}
                                     {:def "a"
                                      :outcome :passed}
                                     {:def "b"
                                      :outcome :no-coverage}
                                     {:def "b"
                                      :outcome :unviable}]})]
    (is (= ["b"] not-covered)
        "definition with an uncovered mutant is named, whatever its other outcomes")
    (is (< (abs (- score 1/3)) 1e-9))
    (is (== 1/2 (rationalize covered-score))
        "covered score leaves uncovered mutants out")))

(def ^:private coverage-tests
  (str target-tests
       "(defn setup-once [f] (target/positive? 2) (f))\n"
       "(clojure.test/use-fixtures :once setup-once)\n"
       "(deftest all-labels (labels) (is (= :up (target/label 2))))\n"))

(defn- coverage-of []
  (mutate/coverage 'cljs-patrol.dev.mutate-test.target ["positive?" "label" "unused"]
                   ['cljs-patrol.dev.mutate-test.target-test] 10000))

(deftest coverage-tells-fixture-calls-and-nested-tests
  (with-target coverage-tests
    (fn []
      (let [{:keys [tests reached-outside hooked?]} (coverage-of)
            test-var #(ns-resolve 'cljs-patrol.dev.mutate-test.target-test %)]
        (is (contains? reached-outside "positive?")
            "call from a fixture is reached outside a test")
        (is (contains? (get tests "label") (test-var 'all-labels))
            "call after a nested test returns is still the outer test's")
        (is (false? hooked?))))))

(def ^:private state-tests
  (str target-tests
       "(def state (atom nil))\n"
       "(deftest needs-state (is (= 1 @state)))\n"))

(deftest test-plan-runs-every-test-when-selection-is-unsafe
  (with-target state-tests
    (fn []
      (let [test-var #(ns-resolve 'cljs-patrol.dev.mutate-test.target-test %)
            context (fn [covered] {:test-nss ['cljs-patrol.dev.mutate-test.target-test]
                                   :timeout-ms 10000
                                   :covered covered
                                   :plans (atom {})})
            covered {:tests {"label" #{(test-var 'labels)}
                             "unused" #{(test-var 'needs-state)}
                             "positive?" #{(test-var 'labels)}}
                     :recorded #{"label" "unused" "positive?" "never"}
                     :reached-outside #{"positive?"}}]
        (is (= {:kind :subset
                :tests #{(test-var 'labels)}}
               (mutate/test-plan (context covered) "label")))
        (is (= {:kind :every} (mutate/test-plan (context covered) "unused"))
            "selected tests that fail on their own run every test")
        (is (= {:kind :every} (mutate/test-plan (context covered) "positive?"))
            "definition reached outside a test runs every test")
        (is (= {:kind :uncovered} (mutate/test-plan (context covered) "never")))
        (is (= {:kind :every} (mutate/test-plan (context {:recorded #{}}) "label"))
            "definition not recorded runs every test")
        (is (= {:kind :every} (mutate/test-plan (context (assoc covered :hooked? true)) "label"))
            "test-ns-hook runs every test")))))

(deftest mutants-run-only-the-tests-that-call-them
  (with-target (str target-tests
                    "(def unrelated-runs (atom 0))\n"
                    "(deftest unrelated (swap! unrelated-runs inc) (is true))\n")
    (fn []
      (let [{:keys [tests-run]} (run-target {:defs ["positive?" "label"]})]
        (is (= {"positive?" :subset
                "label" :subset}
               tests-run)
            "plan is made on the unmutated namespace, so a killed first mutant keeps the selection"))
      (is (= 1 @@(ns-resolve 'cljs-patrol.dev.mutate-test.target-test 'unrelated-runs))
          "test that calls nothing of the target runs only in the first run"))))

(deftest reached-rule-counts-test-failures-and-timeouts-only
  (let [outcomes #(map :outcome (:mutants (mutate/summarize {:defs-in-scope ["a"]
                                                             :results %})))]
    (is (= [:timeout :survived]
           (outcomes [{:def "a"
                       :outcome :timeout}
                      {:def "a"
                       :outcome :no-coverage}])))
    (is (= [:killed :no-coverage]
           (outcomes [{:def "a"
                       :outcome :failed
                       :at-load? true}
                      {:def "a"
                       :outcome :no-coverage}]))
        "mutant that threw while it loaded shows nothing about tests")))

(def ^:private source-with-handlers (str target-source "(def handlers {:label label})\n"))

(deftest recording-that-breaks-the-tests-turns-selection-off
  (with-target source-with-handlers
    (str target-tests "(deftest same-function (is (= target/label (:label target/handlers))))\n")
    (fn []
      (let [{:keys [note totals]} (run-target source-with-handlers {})]
        (is (re-find #"every mutant runs every test" note))
        (is (zero? (:no-coverage totals))
            "nothing is reported as not covered without the recording")))))

(deftest coverage-notices-a-test-ns-hook
  (with-target (str target-tests "(defn test-ns-hook [] (labels))\n")
    (fn []
      (sample-ns 'cljs-patrol.dev.mutate-test.plain-test)
      (try
        (is (true? (:hooked? (mutate/coverage 'cljs-patrol.dev.mutate-test.target ["label"]
                                              ['cljs-patrol.dev.mutate-test.plain-test
                                               'cljs-patrol.dev.mutate-test.target-test]
                                              10000)))
            "one hooked namespace among several is enough")
        (finally (remove-ns 'cljs-patrol.dev.mutate-test.plain-test))))))
