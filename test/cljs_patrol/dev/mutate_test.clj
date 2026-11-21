(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [clojure.java.io]
   [clojure.java.shell]
   [clojure.string :as str]
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
                                   :unreached [{:def "e"}]
                                   :plans {"a" {:kind :subset
                                                :tests [#'clojure.core/inc]}
                                           "b" {:kind :every}}})]
    (is (= [{:def "a"
             :mutants 2
             :killed 1
             :survived 1
             :no-coverage 0
             :unviable 0
             :timeout 0
             :tests ["clojure.core/inc"]}
            {:def "b"
             :mutants 2
             :killed 0
             :survived 0
             :no-coverage 0
             :unviable 1
             :timeout 1
             :tests :every}]
           (:definitions summary)))
    (is (= {"a" :subset
            "b" :every}
           (:tests-run summary)))
    (is (not-any? #(contains? % :killed-by) (:mutants summary))
        "no killing test, no key")
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
        (is (= #{(test-var 'labels) (test-var 'all-labels)} (get tests "positive?"))
            "call inside a nested test also belongs to the test that called it")
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
                                   :select? true
                                   :covered covered
                                   :plans (atom {})})
            covered {:tests {"label" #{(test-var 'labels)}
                             "unused" #{(test-var 'needs-state)}
                             "positive?" #{(test-var 'labels)}}
                     :recorded #{"label" "unused" "positive?" "never"}
                     :reached-outside #{"positive?"}}]
        (let [plan (mutate/test-plan (context covered) "label")]
          (is (= {:kind :subset
                  :tests #{(test-var 'labels)}}
                 (select-keys plan [:kind :tests])))
          (is (< (:timeout-ms plan) 10000)
              "subset is timed by its own run"))
        (is (= 10000 (:timeout-ms (mutate/test-plan (assoc (context covered) :fixed-timeout? true) "label")))
            "given timeout is kept")
        (is (= {:kind :every} (mutate/test-plan (assoc (context covered) :select? false) "label"))
            "selection turned off runs every test")
        (is (= {:kind :every} (mutate/test-plan (context covered) "unused"))
            "selected tests that fail on their own run every test")
        (is (= {:kind :every} (mutate/test-plan (context covered) "positive?"))
            "definition reached outside a test runs every test")
        (is (= {:kind :uncovered} (mutate/test-plan (context covered) "never")))
        (is (= {:kind :every} (mutate/test-plan (context (assoc covered :test-count 1)) "label"))
            "definition every test calls runs every test, with no extra check")
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

(deftest test-plan-notices-tests-that-loop-without-a-mutation
  (with-target (str target-tests
                    "(def release (atom false))\n"
                    "(deftest spins (while (not @release)) (is true))\n")
    (fn []
      (let [release @(ns-resolve 'cljs-patrol.dev.mutate-test.target-test 'release)
            spins (ns-resolve 'cljs-patrol.dev.mutate-test.target-test 'spins)]
        (try
          (is (= {:kind :stuck}
                 (mutate/test-plan {:test-nss ['cljs-patrol.dev.mutate-test.target-test]
                                    :timeout-ms 200
                                    :select? true
                                    :covered {:tests {"label" #{spins}}
                                              :recorded #{"label"}}
                                    :plans (atom {})}
                                   "label")))
          (finally (reset! release true)))))))

(deftest all-tests-option-turns-selection-off
  (with-target target-tests
    (fn []
      (is (= {"positive?" :every
              "label" :every}
             (:tests-run (run-target {:defs ["positive?" "label"]
                                      :all-tests true})))))))

(deftest run-writes-the-html-report
  (with-target target-tests
    (fn []
      (let [file (java.io.File/createTempFile "mutate" ".html")]
        (try
          (run-target {:html (str file)})
          (is (re-find #"<h1>The tests caught 1 of 3 planted bugs \(33%\)</h1>" (slurp file)))
          (finally (.delete file)))))))

(deftest relative-path-starts-from-the-working-directory
  (is (= "src/app.clj" (mutate/relative-path "src/app.clj")))
  (is (= (.getCanonicalPath (clojure.java.io/file "/tmp/app.clj")) (mutate/relative-path "/tmp/app.clj"))
      "file outside the working directory keeps its whole path"))

(deftest run-names-killing-test-and-tests-that-ran
  (with-target (str target-tests "(deftest unrelated (is true))\n")
    (fn []
      (let [{:keys [mutants definitions]} (run-target {:defs ["label"]})]
        (is (= ["cljs-patrol.dev.mutate-test.target-test/labels"]
               (keep :killed-by mutants)))
        (is (= [["label" ["cljs-patrol.dev.mutate-test.target-test/labels"]]]
               (map (juxt :def :tests) definitions)))))))

(deftest run-tests-names-innermost-failing-test
  (with-target (str target-tests
                    "(deftest inner (is (= :down (target/label 1))))\n"
                    "(deftest outer (inner))\n")
    (fn []
      (is (= "cljs-patrol.dev.mutate-test.target-test/inner"
             (-> (mutate/run-tests ['cljs-patrol.dev.mutate-test.target-test]
                                   5000
                                   [(ns-resolve 'cljs-patrol.dev.mutate-test.target-test 'outer)])
                 :killed-by
                 symbol
                 str))
          "failure inside nested test names that test"))))

(def ^:private marked-source
  (str "(ns app.marked)\n"
       "(defn pick [xs] (first xs)) ; mutate: skip first: xs holds one item\n"
       "(defn both [x]\n"
       "  ; mutate: skip when: x is never nil here\n"
       "  ; mutate: skip =: two checks for one case\n"
       "\n"
       "  (when (= x 1) :one))\n"
       "(defn ^{:mutate/skip \"wraps a JDK call\"} wrapped [x] (if x 1 2))\n"
       "(defn stale [x] (if x 1 2)) ; mutate: skip first: no first here any more\n"
       "(defn told [x]\n"
       "  ; mutate: skip when: r1\n"
       "  (when (str \"; mutate: skip only words\") 1) ; mutate: skip str: r2\n"
       "  (when x 2))\n"
       "; mutate: skip if: nothing follows\n"))

(deftest skip-markers-read-trailing-and-own-line-comments
  (is (= [[2 2 "first" "xs holds one item"]
          [4 7 "when" "x is never nil here"]
          [5 7 "=" "two checks for one case"]
          [9 9 "first" "no first here any more"]
          [11 12 "when" "r1"]
          [12 12 "str" "r2"]
          [14 nil "if" "nothing follows"]]
         (map (juxt :line :row :operator :reason) (mutate/skip-markers marked-source)))
      "own line skips next code past blank lines and comments, also next to trailing marker; string is no marker")
  (doseq [text ["(inc 1) ; mutate: skip because\n" "(inc 1) ; mutate:skip inc: r\n" "(inc 1) ; Mutate: skip inc: r\n"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"line 1 reads like a skip marker"
                          (mutate/skip-markers text))
        text)))

(deftest scope-leaves-skipped-mutants-out-with-reason
  (let [{:keys [selected ignored unused-markers]} (mutate/scope marked-source {})]
    (is (= [["pick" "first" "xs holds one item"]
            ["both" "when" "x is never nil here"]
            ["both" "=" "two checks for one case"]
            ["wrapped" "if" "wraps a JDK call"]
            ["told" "when" "r1"]]
           (map (juxt :def :from :skip-reason) ignored)))
    (is (= [["stale" "if"]
            ["told" "when"]]
           (map (juxt :def :from) selected))
        "only unmarked mutants run")
    (is (= ["line 9 (first)" "line 12 (str)" "line 14 (if)"] unused-markers)
        "marker naming no mutant on its line, or with no code after it, is reported"))
  (is (= [] (:unused-markers (mutate/scope marked-source {:defs ["pick"]})))
      "markers outside scope are not checked")
  (is (= [7 7] (map :row (:ignored (mutate/scope marked-source {:lines #{7}}))))
      "changed lines limit ignored mutants too")
  (doseq [metadata ["^:mutate/skip" "^{:mutate/skip false}" "^{:mutate/skip \"\"}"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on f needs a reason"
                          (mutate/scope (str "(ns app.x)\n(defn " metadata " f [x] (when x 1))\n") {}))
        metadata))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on first needs a reason"
                        (mutate/scope (str "(ns app.x)\n(defn ^:mutate/skip first [x] (when x 1))\n"
                                           "(defn ^:mutate/skip second [x] (when x 1))\n")
                                      {}))
      "first bad definition named"))

(deftest summary-lists-ignored-mutants-apart-from-not-mutated
  (let [summary (mutate/summarize {:defs-in-scope ["a" "b"]
                                   :results [{:def "a"
                                              :outcome :failed}]
                                   :ignored [{:def "b"
                                              :row 3
                                              :col 4
                                              :from "if"
                                              :to 'if-not
                                              :skip-reason "wraps a JDK call"}]})]
    (is (= [{:def "b"
             :row 3
             :col 4
             :from "if"
             :to "if-not"
             :reason "wraps a JDK call"}]
           (:ignored summary)))
    (is (= [] (:not-mutated summary))
        "definition with only ignored mutants is no unmutated one")
    (is (= 1 (:mutants (:totals summary)))
        "ignored mutants are not counted")))

(deftest run-skips-marked-mutants
  (with-target target-tests
    (fn []
      (let [marked (str/replace target-source
                                "(> x 0))\n"
                                "(> x 0)) ; mutate: skip >: the boundary is checked elsewhere\n")
            marked (str marked "; mutate: skip if: stale\n")
            summary (atom nil)
            output (with-out-str
                     (reset! summary (mutate/mutate-source 'cljs-patrol.dev.mutate-test.target "target.clj" marked
                                                           ['cljs-patrol.dev.mutate-test.target-test] {})))]
        (is (= 2 (:mutants (:totals @summary)))
            "marked mutant does not run")
        (is (= [["positive?" ">" "the boundary is checked elsewhere"]]
               (map (juxt :def :from :reason) (:ignored @summary))))
        (is (str/includes? output "Warning: the skip marker on line 5 (if) matches no mutant"))
        (is (str/includes? output "2 mutants of cljs-patrol.dev.mutate-test.target against"))
        (is (str/includes? output "target-test, 1 ignored, timeout"))
        (is (str/includes? output
                           "Ignored, with a skip marker:\n  2:32  > -> >=  in positive?: the boundary is checked elsewhere")
            "console names ignored mutant and its reason")))))

(deftest skip-markers-ignore-prose-that-mentions-them
  (is (= []
         (mutate/skip-markers (str "(ns app.x)\n"
                                   ";; see ^{:mutate/skip ...} on wrapped\n"
                                   "; never mutate; skip the copy\n"
                                   "(defn f [x] (when x 1))\n"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"line 1 reads like a skip marker"
                        (mutate/skip-markers "; mutate:skip first: x\n(ns app.x)\n"))
      "comment before first form is read"))

(deftest skip-markers-stay-inside-their-form
  (is (= [[3 nil] [6 8]]
         (map (juxt :line :row)
              (mutate/skip-markers (str "(defn f [x]\n"
                                        "  (g x)\n"
                                        "  ; mutate: skip when: last in its form\n"
                                        "  )\n"
                                        "(defn h [x]\n"
                                        "  ; mutate: skip when: past a #_ form\n"
                                        "  #_(old-call x)\n"
                                        "  (when x 1))\n"))))
      "no code after marker in its form gives no row; #_ form is no code"))

(deftest scope-reads-skip-reason-from-attribute-map
  (is (= [["f" "if" "wraps a JDK call"]]
         (map (juxt :def :from :skip-reason)
              (:ignored (mutate/scope "(ns app.x)\n(defn f \"Doc.\" {:mutate/skip \"wraps a JDK call\"} [x] (if x 1 2))\n"
                                      {})))))
  (is (= ["if"]
         (map :from (:selected (mutate/scope "(ns app.x)\n(def m {:mutate/skip \"value\" :a (if x 1 2)})\n" {}))))
      "map value of def is no attribute map"))

(deftest scope-checks-skip-reason-of-every-definition
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on helper needs a reason"
                        (mutate/scope "(ns app.x)\n(defn ^:mutate/skip helper [x] (str x))\n" {}))
      "definition with no mutants")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on helper needs a reason"
                        (mutate/scope "(ns app.x)\n(defn ^:mutate/skip helper [x] (when x 1))\n(defn g [x] (if x 1 2))\n"
                                      {:defs ["g"]}))
      "definition outside -d")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on f needs a reason as a string"
                        (mutate/scope (str "(ns app.x)\n"
                                           "(defn ^{:mutate/skip #?(:clj \"jdk\" :cljs \"js\")} f [x] (when x 1))\n")
                                      {}))
      "reader conditional is no reason string"))

(deftest scope-reports-markers-in-forms-that-are-not-mutated
  (is (= ["line 2 (when)" "line 4 (if)"]
         (:unused-markers (mutate/scope (str "(ns app.x)\n"
                                             "(:require [a]) ; mutate: skip when: stale\n"
                                             "(comment\n"
                                             "  (if x 1 2)) ; mutate: skip if: stale\n"
                                             "(defn f [x] (when x 1))\n")
                                        {})))))

(deftest skip-markers-leave-other-mutate-comments-alone
  (is (= []
         (mutate/skip-markers "(ns app.x)\n;; Mutate the atom in place for speed\n; mutate! is only for tests\n")))
  (is (= [] (mutate/skip-markers ""))
      "empty source"))

(deftest scope-reads-attribute-map-only-before-parameters-or-after-arities
  (is (= ["if"]
         (map :from (:selected (mutate/scope "(ns app.x)\n(defn f ^String [x] {:mutate/skip (if x \"a\" \"b\")})\n" {}))))
      "map after hinted parameter vector is body")
  (is (= [["f" "when" "wraps a JDK call"] ["f" "if" "wraps a JDK call"]]
         (map (juxt :def :from :skip-reason)
              (:ignored (mutate/scope (str "(ns app.x)\n"
                                           "(defn f ([x] (when x 1)) ([x y] (if x y 2)) {:mutate/skip \"wraps a JDK call\"})\n")
                                      {}))))
      "trailing attribute map after arities")
  (is (= ["r"]
         (map :skip-reason
              (:ignored (mutate/scope "(ns app.x)\n(defn ^:private f {:mutate/skip \"r\"} [x] (if x 1 2))\n" {}))))
      "name under metadata is no parameter vector"))

(deftest scope-rejects-skip-metadata-that-does-nothing-or-cannot-be-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"m is a defmacro, which the runner does not mutate"
                        (mutate/scope "(ns app.x)\n(defmacro ^{:mutate/skip \"r\"} m [x] x)\n" {})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata of f cannot be read: No value supplied"
                        (mutate/scope "(ns app.x)\n(defn ^{:mutate/skip} f [x] (when x 1))\n" {}))))
