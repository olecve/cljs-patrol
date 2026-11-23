(ns cljs-patrol.dev.mutate.run-test
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.forms :as forms]
   [cljs-patrol.dev.mutate.guard :as guard]
   [cljs-patrol.dev.mutate.run :as run]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(deftest load-outcome-tells-unviable-from-killed
  (fixtures/sample-ns 'cljs-patrol.dev.mutate.fixtures.load)
  (try
    (is (= :unviable (run/load-outcome #(run/load-text 'cljs-patrol.dev.mutate.fixtures.load "(foo")))
        "unreadable")
    (is (= :unviable (run/load-outcome #(run/load-text 'cljs-patrol.dev.mutate.fixtures.load
                                                       "(defn f [x] (case x true :a true :b))")))
        "uncompilable")
    (is (= :failed (run/load-outcome #(run/load-text 'cljs-patrol.dev.mutate.fixtures.load "(def x (/ 1 0))")))
        "throws while it loads")
    (is (nil? (run/load-outcome #(run/load-text 'cljs-patrol.dev.mutate.fixtures.load "(def y 1)"))))
    (finally (remove-ns 'cljs-patrol.dev.mutate.fixtures.load))))

(deftest reader-conditionals-load-as-clojure
  (fixtures/sample-ns 'cljs-patrol.dev.mutate.fixtures.sample)
  (try
    (run/load-text 'cljs-patrol.dev.mutate.fixtures.sample "(def platform #?(:clj :jvm :cljs :js))")
    (is (= :jvm @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.sample 'platform)))
    (finally (remove-ns 'cljs-patrol.dev.mutate.fixtures.sample))))

(def ^:private validation-tests
  "A test namespace whose only assertion catches every exception, as `(thrown? Throwable …)` does."
  (str "(ns cljs-patrol.dev.mutate.fixtures.thrown (:require [clojure.test :refer [deftest is]]))\n"
       "(def validate (fn [x] (when-not (pos? x) (throw (ex-info \"no\" {})))))\n"
       "(deftest rejects-zero (is (thrown? Throwable (validate 0))))\n"))

(deftest failure-inside-thrown-assertion-still-kills
  (fixtures/sample-ns 'cljs-patrol.dev.mutate.fixtures.thrown)
  (try
    (run/load-text 'cljs-patrol.dev.mutate.fixtures.thrown validation-tests)
    (is (= :passed (:outcome (run/run-tests ['cljs-patrol.dev.mutate.fixtures.thrown] 10000))))
    (run/load-text 'cljs-patrol.dev.mutate.fixtures.thrown "(def validate (fn [_] nil))")
    (is (= :failed (:outcome (run/run-tests ['cljs-patrol.dev.mutate.fixtures.thrown] 10000)))
        "thrown? assertion that fails is recorded before its catch can swallow it")
    (finally (remove-ns 'cljs-patrol.dev.mutate.fixtures.thrown))))

(deftest coverage-records-calls-made-on-worker-threads
  (fixtures/with-target (str fixtures/target-tests
                             "(deftest on-thread (let [p (promise)]"
                             " (.start (Thread. #(deliver p (target/label 0))))"
                             " (is (= :down @p))))\n")
    (fn []
      (let [{:keys [tests recorded]} (run/coverage 'cljs-patrol.dev.mutate.fixtures.target
                                                   ["positive?" "label" "unused"]
                                                   ['cljs-patrol.dev.mutate.fixtures.target-test]
                                                   10000
                                                   nil)
            test-var #(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test %)]
        (is (= #{"positive?" "label" "unused"} recorded))
        (is (= #{(test-var 'labels) (test-var 'on-thread)} (get tests "label"))
            "call on a raw thread is recorded")
        (is (nil? (get tests "unused")))
        (is (= :up ((ns-resolve 'cljs-patrol.dev.mutate.fixtures.target 'label) 1))
            "vars are restored")
        (is (fn? (:test (meta (test-var 'labels))))
            "tests are restored")))))

(defmulti ^:private dispatcher identity)

(defn- plain [x] x)

(defn- counter ^long [^long n] (inc n))

(defn- partly-hinted
  ([^long n _] n)
  ([x] x))

(deftest wrappable-means-a-plain-function
  (is (true? (run/wrappable? #'plain)))
  (is (false? (run/wrappable? #'dispatcher))
      "multimethod")
  (is (false? (run/wrappable? #'counter))
      "primitive hint")
  (is (false? (run/wrappable? #'partly-hinted))
      "primitive hint on one parameter of one arity")
  (is (false? (run/wrappable? #'fixtures/target-source))
      "data"))

(def ^:private coverage-tests
  (str fixtures/target-tests
       "(defn setup-once [f] (target/positive? 2) (f))\n"
       "(clojure.test/use-fixtures :once setup-once)\n"
       "(deftest all-labels (labels) (is (= :up (target/label 2))))\n"))

(defn- coverage-of []
  (run/coverage 'cljs-patrol.dev.mutate.fixtures.target ["positive?" "label" "unused"]
                ['cljs-patrol.dev.mutate.fixtures.target-test] 10000 nil))

(deftest coverage-tells-fixture-calls-and-nested-tests
  (fixtures/with-target coverage-tests
    (fn []
      (let [{:keys [tests reached-outside hooked?]} (coverage-of)
            test-var #(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test %)]
        (is (contains? reached-outside "positive?")
            "call from a fixture is reached outside a test")
        (is (contains? (get tests "label") (test-var 'all-labels))
            "call after a nested test returns is still the outer test's")
        (is (= #{(test-var 'labels) (test-var 'all-labels)} (get tests "positive?"))
            "call inside a nested test also belongs to the test that called it")
        (is (false? hooked?))))))

(def ^:private state-tests
  (str fixtures/target-tests
       "(def state (atom nil))\n"
       "(deftest needs-state (is (= 1 @state)))\n"))

(deftest test-plan-runs-every-test-when-selection-is-unsafe
  (fixtures/with-target state-tests
    (fn []
      (let [test-var #(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test %)
            context (fn [covered] {:test-nss ['cljs-patrol.dev.mutate.fixtures.target-test]
                                   :timeout-ms 10000
                                   :select? true
                                   :covered covered
                                   :plans (atom {})})
            covered {:tests {"label" #{(test-var 'labels)}
                             "unused" #{(test-var 'needs-state)}
                             "positive?" #{(test-var 'labels)}}
                     :recorded #{"label" "unused" "positive?" "never"}
                     :reached-outside #{"positive?"}}]
        (let [plan (run/test-plan (context covered) "label")]
          (is (= {:kind :subset
                  :tests #{(test-var 'labels)}}
                 (select-keys plan [:kind :tests])))
          (is (< (:timeout-ms plan) 10000)
              "subset is timed by its own run"))
        (is (= 10000 (:timeout-ms (run/test-plan (assoc (context covered) :fixed-timeout? true) "label")))
            "given timeout is kept")
        (is (= {:kind :every} (run/test-plan (assoc (context covered) :select? false) "label"))
            "selection turned off runs every test")
        (is (= {:kind :every} (run/test-plan (context covered) "unused"))
            "selected tests that fail on their own run every test")
        (is (= {:kind :every} (run/test-plan (context covered) "positive?"))
            "definition reached outside a test runs every test")
        (is (= {:kind :uncovered} (run/test-plan (context covered) "never")))
        (is (= {:kind :every} (run/test-plan (context (assoc covered :test-count 1)) "label"))
            "definition every test calls runs every test, with no extra check")
        (is (= {:kind :every} (run/test-plan (context {:recorded #{}}) "label"))
            "definition not recorded runs every test")
        (is (= {:kind :every} (run/test-plan (context (assoc covered :hooked? true)) "label"))
            "test-ns-hook runs every test")))))

(deftest coverage-notices-a-test-ns-hook
  (fixtures/with-target (str fixtures/target-tests "(defn test-ns-hook [] (labels))\n")
    (fn []
      (fixtures/sample-ns 'cljs-patrol.dev.mutate.fixtures.plain-test)
      (try
        (is (true? (:hooked? (run/coverage 'cljs-patrol.dev.mutate.fixtures.target ["label"]
                                           ['cljs-patrol.dev.mutate.fixtures.plain-test
                                            'cljs-patrol.dev.mutate.fixtures.target-test]
                                           10000
                                           nil)))
            "one hooked namespace among several is enough")
        (finally (remove-ns 'cljs-patrol.dev.mutate.fixtures.plain-test))))))

(deftest test-plan-notices-tests-that-loop-without-a-mutation
  (fixtures/with-target (str fixtures/target-tests
                             "(def release (atom false))\n"
                             "(deftest spins (while (not @release)) (is true))\n")
    (fn []
      (let [release @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'release)
            spins (ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'spins)]
        (try
          (is (= {:kind :stuck}
                 (run/test-plan {:test-nss ['cljs-patrol.dev.mutate.fixtures.target-test]
                                 :timeout-ms 200
                                 :select? true
                                 :covered {:tests {"label" #{spins}}
                                           :recorded #{"label"}}
                                 :plans (atom {})}
                                "label")))
          (finally (reset! release true)))))))

(deftest run-tests-names-innermost-failing-test
  (fixtures/with-target (str fixtures/target-tests
                             "(deftest inner (is (= :down (target/label 1))))\n"
                             "(deftest outer (inner))\n")
    (fn []
      (is (= "cljs-patrol.dev.mutate.fixtures.target-test/inner"
             (-> (run/run-tests ['cljs-patrol.dev.mutate.fixtures.target-test]
                                5000
                                [(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'outer)])
                 :killed-by
                 symbol
                 str))
          "failure inside nested test names that test"))))

(defn- with-changed-guard
  "Call `f` with a guard on a new folder, after a file was added to the folder that no mutant wrote."
  [f]
  (fixtures/with-temp-dir
    (fn [dir]
      (let [snapshot (guard/snapshot dir #{})
            file-guard (guard/start dir #{} snapshot snapshot #{})]
        (spit (java.io.File. dir "left.txt") "x")
        (f file-guard)))))

(deftest run-all-stops-at-mutant-whose-tests-keep-running
  (fixtures/with-target (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
                             "(defn done? [] true)\n")
    (str "(ns cljs-patrol.dev.mutate.fixtures.target-test (:require [clojure.test :refer [deftest is]]"
         " [cljs-patrol.dev.mutate.fixtures.target :as target]))\n"
         "(def release (atom false))\n"
         "(deftest waits (while (not (or (target/done?) @release))) (is true))\n")
    (fn []
      (let [release @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'release)
            source (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
                        "(defn done? [] true)\n")]
        (try
          (let [{:keys [stopped unreached stop-reason]}
                (with-changed-guard
                  (fn [file-guard]
                    (binding [*out* (java.io.StringWriter.)]
                      (run/run-all {:target 'cljs-patrol.dev.mutate.fixtures.target
                                    :file "target.clj"
                                    :test-nss ['cljs-patrol.dev.mutate.fixtures.target-test]
                                    :timeout-ms 200
                                    :select? false
                                    :covered {}
                                    :plans (atom {})
                                    :guard file-guard}
                                   (:selected (forms/scope source {}))))))]
            (is (str/includes? stopped "\nA last look also found that the tests of a mutant the guard cannot name")
                "file written before stop is told")
            (is (str/starts-with? stopped "the tests of the mutant at 2:16 body still run after the timeout")
                "message names position and operator")
            (is (= :stuck stop-reason))
            (is (= ["true"] (map :from unreached))
                "later mutant at same position left unreached"))
          (finally (reset! release true)))))))

(deftest run-all-stops-at-definition-whose-tests-loop-without-mutation
  (fixtures/with-target (str fixtures/target-tests
                             "(def release (atom false))\n"
                             "(deftest spins (while (not @release)) (is true))\n")
    (fn []
      (let [release @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'release)
            spins (ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'spins)]
        (try
          (let [{:keys [stop-reason results unreached stopped]}
                (with-changed-guard
                  (fn [file-guard]
                    (binding [*out* (java.io.StringWriter.)]
                      (run/run-all {:target 'cljs-patrol.dev.mutate.fixtures.target
                                    :file "target.clj"
                                    :test-nss ['cljs-patrol.dev.mutate.fixtures.target-test]
                                    :timeout-ms 200
                                    :select? true
                                    :covered {:tests {"label" #{spins}}
                                              :recorded #{"label"}
                                              :test-count 2}
                                    :plans (atom {})
                                    :guard file-guard}
                                   (filter #(= "label" (:def %)) (:selected (forms/scope fixtures/target-source {})))))))]
            (is (str/includes? stopped "\nA last look also found that "))
            (is (= :plan-stuck stop-reason))
            (is (empty? results))
            (is (= 2 (count unreached))))
          (finally (reset! release true)))))))

(deftest recorded-plans-name-tests-coverage-saw-call-each-definition
  (let [covered {:tests {"a" #{#'clojure.core/inc}
                         "b" #{}
                         "c" #{#'clojure.core/inc}
                         "e" #{#'clojure.core/inc}}
                 :recorded #{"a" "b" "e" "f"}
                 :reached-outside #{"e" "f"}
                 :test-count 2}]
    (is (= {"a" {:kind :subset
                 :tests #{#'clojure.core/inc}}}
           (run/recorded-plans covered ["a" "b"] true))
        "no call gives no test list")
    (is (= {"c" {:kind :every}
            "d" {:kind :every}}
           (run/recorded-plans covered ["c" "d"] true))
        "definition pass could not record runs every test")
    (is (= {"e" {:kind :every}
            "f" {:kind :every}}
           (run/recorded-plans covered ["e" "f"] true))
        "call outside test runs every test, also with no test calling it")
    (is (= {:kind :every} (get (run/recorded-plans covered ["a"] false) "a"))
        "selection off")
    (is (= {:kind :every} (get (run/recorded-plans (assoc covered :hooked? true) ["a"] true) "a"))
        "test-ns-hook")
    (is (= {:kind :every} (get (run/recorded-plans (assoc covered :test-count 1) ["a"] true) "a"))
        "every test calls it"))
  (is (= {} (run/recorded-plans nil ["a"] true))
      "no baseline, no test lists"))

(defn- baseline-writers [guard-root]
  (-> (run/baseline 'cljs-patrol.dev.mutate.fixtures.target ["label"] ['cljs-patrol.dev.mutate.fixtures.target-test]
                    10000 guard-root)
      :covered
      :writers))

(deftest baseline-with-guard-root-records-tests-that-write-files
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target (str fixtures/target-tests
                                 "(deftest writes (Thread/sleep 20)"
                                 " (spit (java.io.File. " (pr-str (str dir)) " \"note.txt\") \"x\")"
                                 " (is (= :up (target/label 1))))\n"
                                 "(deftest makes-folder (.mkdir (java.io.File. " (pr-str (str dir)) " \"made\"))"
                                 " (is (= :up (target/label 1))))\n")
        (fn []
          (is (= #{(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'writes)
                   (ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'makes-folder)}
                 (baseline-writers (str dir)))
              "new folder writes no file, but adds name")
          (is (nil? (baseline-writers nil))
              "no guard, nothing recorded")
          (is (not (contains? (run/coverage 'cljs-patrol.dev.mutate.fixtures.target ["label"]
                                            ['cljs-patrol.dev.mutate.fixtures.target-test] 10000 nil)
                              :recording))))))))

(deftest baseline-counts-fixture-write-for-every-test
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target (str fixtures/target-tests
                                 "(defn setup [f] (spit (java.io.File. " (pr-str (str dir)) " \"note.txt\") \"x\")"
                                 " (Thread/sleep 10) (f))\n"
                                 "(clojure.test/use-fixtures :once setup)\n"
                                 "(deftest other (is (= :down (target/label 0))))\n")
        (fn []
          (is (= #{(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'labels)
                   (ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'other)}
                 (baseline-writers (str dir)))))))))

(deftest baseline-without-flight-recorder-records-no-writers
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (with-redefs [guard/recorder-available? (constantly false)]
            (is (nil? (baseline-writers (str dir))))))))))

(def ^:private arming-source
  (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
       "(def armed (atom false))\n"
       "(def step (do (when @armed (throw (ex-info \"restore fails\" {}))) inc))\n"))

(defn- arming-tests [root]
  (str "(ns cljs-patrol.dev.mutate.fixtures.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate.fixtures.target :as target]))\n"
       "(deftest arms (reset! target/armed true) (spit (java.io.File. " (pr-str (str root)) " \"note.txt\") \"x\")"
       " (is true))\n"))

(defn- run-arming
  "Run the mutants of `step` with a guard on `root` whose baseline saw `writers` write, and return the stop."
  [root writers]
  (let [snapshot (guard/snapshot root #{})
        file-guard (guard/start root #{} snapshot snapshot writers)]
    (binding [*out* (java.io.StringWriter.)]
      (run/run-all {:target 'cljs-patrol.dev.mutate.fixtures.target
                    :file "target.clj"
                    :test-nss ['cljs-patrol.dev.mutate.fixtures.target-test]
                    :timeout-ms 5000
                    :select? false
                    :covered {}
                    :plans (atom {})
                    :guard file-guard}
                   (cache/with-ids (filter #(= "step" (:def %)) (:selected (forms/scope arming-source {}))))))))

(deftest run-all-tells-changed-files-and-failed-restore-of-one-mutant
  (fixtures/with-temp-dir
    (fn [root]
      (fixtures/with-target arming-source (arming-tests root)
        (fn []
          (let [{:keys [stop-reason stopped]} (run-arming root #{(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test
                                                                             'arms)})]
            (is (= :files-changed stop-reason))
            (is (str/starts-with? stopped "the tests of the mutant step/3:"))
            (is (str/includes? stopped "  added note.txt\n"))
            (is (str/includes? stopped "\nAlso, restoring step failed, so the namespace stays mutated: "))))))))

(deftest run-all-takes-last-look-when-restore-fails
  (fixtures/with-temp-dir
    (fn [root]
      (fixtures/with-target arming-source (arming-tests root)
        (fn []
          (let [{:keys [stop-reason stopped]} (run-arming root #{})]
            (is (= :restore-error stop-reason)
                "no test wrote in baseline, so mutant got no snapshot")
            (is (str/starts-with? stopped "restoring step failed"))
            (is (str/includes? stopped "\nA last look also found that the tests of the mutant step/3:"))
            (is (str/includes? stopped "  added note.txt\n"))))))))
