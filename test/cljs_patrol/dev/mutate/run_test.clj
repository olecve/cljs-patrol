(ns cljs-patrol.dev.mutate.run-test
  (:require
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.run :as run]
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
                                                   10000)
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
                ['cljs-patrol.dev.mutate.fixtures.target-test] 10000))

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
                                           10000)))
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
