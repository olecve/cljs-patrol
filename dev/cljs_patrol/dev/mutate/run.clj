(ns cljs-patrol.dev.mutate.run
  "Load mutants into the target namespace and run its tests on each: coverage, test plans, timeouts and stuck threads."
  (:require
   [cljs-patrol.dev.mutate.error :as error]
   [cljs-patrol.dev.mutate.forms :as forms]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [clojure.test :as t]
   [rewrite-clj.zip :as z]))

(defn load-text
  "Read and evaluate every form of `text` in `target`, reader conditionals read for Clojure.

  Line numbers start at `line`, so the vars keep the position of their source."
  ([target text] (load-text target text {}))
  ([target text {:keys [file line]}]
   (binding [*ns* (the-ns target)
             *file* (or file *file*)]
     (let [reader (doto (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. text))
                    (.setLineNumber (int (or line 1))))]
       (loop []
         (let [form (read {:read-cond :allow
                           :eof ::eof}
                          reader)]
           (when-not (= ::eof form)
             (eval form)
             (recur))))))))

(def ^:private unviable-phases
  "Phases of a Clojure error that come before the code runs: the mutant could not be read or compiled."
  #{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check :compilation})

(defn load-outcome
  "Return nil when loading succeeds, :unviable for a read or compile error, and :failed for one the code threw."
  [load!]
  (try
    (load!)
    nil
    (catch Throwable e
      (if (or (instance? clojure.lang.LispReader$ReaderException e)
              (contains? unviable-phases (:clojure.error/phase (ex-data e))))
        :unviable
        :failed))))

(defn run-tests
  "Run the test namespaces quietly in a thread of their own, and return {:outcome … :killed-by … :output … :thread …}.

  Given `test-vars`, only those tests run, with their namespaces' fixtures. The outcome is
  :passed, :failed or :timeout, and :killed-by is the test that failed first. A failure is
  recorded before the run is cut short, so an assertion that catches exceptions, such as
  `thrown?`, cannot hide it, and a run that failed before it timed out counts as failed. A
  timed-out thread is interrupted, but a loop that never checks for it keeps running."
  ([test-nss timeout-ms] (run-tests test-nss timeout-ms nil))
  ([test-nss timeout-ms test-vars]
   (let [failed (atom false)
         killed-by (atom nil)
         output (java.io.StringWriter.)
         default-report (.getRawRoot #'t/report)
         report (fn [{:keys [type]
                      :as event}]
                  (default-report event)
                  (when (contains? #{:fail :error} type)
                    (compare-and-set! killed-by nil (first t/*testing-vars*))
                    (reset! failed true)
                    (throw (AssertionError. "A test failed; the mutant is killed."))))
         result (promise)
         thread (Thread. ^Runnable (fn []
                                     (deliver result
                                              (try
                                                (binding [t/*test-out* output
                                                          *out* output
                                                          t/report report]
                                                  (if (seq test-vars)
                                                    (t/test-vars test-vars)
                                                    (apply t/run-tests test-nss)))
                                                (if @failed :failed :passed)
                                                (catch Throwable _ :failed)))))]
     (.setDaemon thread true)
     (.start thread)
     (.join thread (long timeout-ms))
     (if (realized? result)
       {:outcome @result
        :killed-by @killed-by
        :output (str output)}
       (do (.interrupt thread)
           {:outcome (if @failed :failed :timeout)
            :killed-by @killed-by
            :thread thread})))))

(defn wrappable?
  "True when the var holds a plain function whose calls a wrapper can record.

  A multimethod is no plain function, and a function with a primitive type hint is called
  through an interface a wrapper does not have."
  [v]
  (let [value @v]
    (and (fn? value)
         (not (some #(some (comp #{'long 'double} :tag meta) (cons % %)) (:arglists (meta v)))))))

(defn test-vars-of [test-nss]
  (for [test-ns test-nss
        v (vals (ns-interns test-ns))
        :when (:test (meta v))]
    v))

(defn coverage
  "Run the tests once with each function of `def-names` wrapped, and record which tests call which function.

  Return the run with :tests {name #{test-var}}, :recorded, the names whose calls could be
  recorded, :reached-outside, the names called while no test ran, such as from a fixture, and
  :hooked?, true when a test namespace runs its tests through `test-ns-hook`. The running test
  is kept where every thread sees it, since the code under test may run on worker threads, and
  the tests run one at a time. A call is recorded under every test that is running, so a test
  that calls another one, inside its own `binding` or `with-redefs`, is selected too. A call
  made without the var, such as through a value the namespace stored when it loaded, is not
  seen, and a call that a background thread makes after its test ended goes to the test that
  runs then."
  [target def-names test-nss timeout-ms]
  (let [calls (atom {})
        reached-outside (atom #{})
        running-tests (atom [])
        vars (->> def-names
                  distinct
                  (keep (fn [def-name]
                          (when-let [v (ns-resolve target (symbol def-name))]
                            (when (wrappable? v) [def-name v @v]))))
                  vec)
        tests (mapv (fn [v] [v (:test (meta v))]) (test-vars-of test-nss))
        record (fn [def-name]
                 (let [running @running-tests]
                   (if (seq running)
                     (when-not (every? (get @calls def-name #{}) running)
                       (swap! calls update def-name (fnil into #{}) running))
                     (when-not (contains? @reached-outside def-name)
                       (swap! reached-outside conj def-name)))))]
    (try
      (doseq [[test-var test-fn] tests]
        (alter-meta! test-var assoc :test (fn []
                                            (swap! running-tests conj test-var)
                                            (try (test-fn) (finally (swap! running-tests pop))))))
      (doseq [[def-name v original] vars]
        (alter-var-root v (constantly (fn [& args]
                                        (record def-name)
                                        (apply original args)))))
      (assoc (run-tests test-nss timeout-ms)
             :tests @calls
             :recorded (set (map first vars))
             :reached-outside @reached-outside
             :hooked? (boolean (some #(contains? (ns-interns %) 'test-ns-hook) test-nss))
             :test-count (count tests))
      (finally
        (doseq [[_ v original] vars]
          (alter-var-root v (constantly original)))
        (doseq [[test-var test-fn] tests]
          (alter-meta! test-var assoc :test test-fn))))))

(defn default-timeout-ms
  "Return the timeout for a run that took `ms` without a mutation: 1.5 times as long, plus 2 s."
  [ms]
  (long (+ 2000 (* 1.5 ms))))

(defn- timed
  "Call `run!`, which returns a test run, and add how many milliseconds it took as :ms."
  [run!]
  (let [started (System/nanoTime)
        run (run!)]
    (assoc run :ms (/ (- (System/nanoTime) started) 1e6))))

(def ^:private stuck-grace-ms
  "How long an interrupted test thread may take to end before the run counts it as stuck."
  500)

(defn- stuck?
  "True when a timed-out test thread still runs after a short grace, and would disturb every later run."
  [^Thread thread]
  (boolean (when thread
             (.join thread (long stuck-grace-ms))
             (.isAlive thread))))

(defn test-plan
  "Decide which tests the mutants of `def-name` run, and their timeout, before any of them is loaded.

  Return {:kind :subset :tests … :timeout-ms …} for the tests that called it, {:kind
  :uncovered} when the coverage pass saw no call of it, in a test or outside one, {:kind
  :every} when every test must run, and {:kind :stuck} when the tests that call it keep running
  past the timeout without any mutation. Every test runs when selection is off, when the
  definition could not be recorded, when something called it outside a test, when a test
  namespace orders its tests with `test-ns-hook`, when every test calls it anyway, and when its
  tests fail on their own, since one may rely on what an earlier test left behind. That last
  check runs once for each definition, on the unmutated namespace, and its time sets the
  subset's timeout unless one was given."
  [{:keys [test-nss timeout-ms fixed-timeout? select? covered plans]} def-name]
  (let [{:keys [tests recorded reached-outside hooked? test-count]} covered
        def-tests (get tests def-name)
        decide (fn []
                 (cond
                   (or (not (contains? recorded def-name)) (contains? reached-outside def-name)) {:kind :every}
                   (empty? def-tests) {:kind :uncovered}
                   (or hooked? (not select?) (= test-count (count def-tests))) {:kind :every}
                   :else (let [{:keys [outcome thread ms]} (timed #(run-tests test-nss timeout-ms def-tests))]
                           (cond
                             (stuck? thread) {:kind :stuck}
                             (= :passed outcome) {:kind :subset
                                                  :tests def-tests
                                                  :timeout-ms (if fixed-timeout? timeout-ms (default-timeout-ms ms))}
                             :else {:kind :every}))))]
    (or (get @plans def-name)
        (get (swap! plans assoc def-name (decide)) def-name))))

(defn- run-mutant
  "Apply a mutant, run the tests, restore the namespace, and return the mutant with its :outcome.

  The mutant runs the tests that called its definition in the coverage pass, when that is safe.
  A definition no test called runs every test, and a mutant of it that they all pass has no
  coverage, so a call the pass could not see cannot hide a test. A form that cannot be read or
  compiled is unviable. One that throws while it loads, such as a `def` whose value now fails,
  was caught by loading it, and is killed. A restore that fails is returned as :restore-error,
  since the namespace is then left mutated. A definition whose own tests keep running without a
  mutation is returned as :plan-stuck?, with no outcome, since the mutant never ran."
  [{:keys [target file test-nss timeout-ms]
    :as context}
   {:keys [form-loc wrapper]
    :as mutant}]
  (let [line (first (parser/position (or wrapper form-loc)))
        original (z/string form-loc)
        in-wrapper (fn [form-text]
                     (if wrapper
                       (let [wrapper-text (z/string wrapper)
                             start (str/last-index-of wrapper-text original)]
                         (str (subs wrapper-text 0 start) form-text (subs wrapper-text (+ start (count original)))))
                       form-text))
        mutated-text (in-wrapper (forms/mutated-text mutant))
        {:keys [kind tests]
         :as plan} (test-plan context (:def mutant))]
    (if (= :stuck kind)
      (assoc mutant :plan-stuck? true)
      (let [load-result (load-outcome #(load-text target mutated-text {:file file
                                                                       :line line}))
            result (if load-result
                     (assoc mutant
                            :outcome load-result
                            :at-load? true)
                     (let [{:keys [outcome thread killed-by]} (run-tests test-nss (:timeout-ms plan timeout-ms) tests)]
                       (assoc mutant
                              :outcome (if (and (= :passed outcome) (= :uncovered kind)) :no-coverage outcome)
                              :killed-by killed-by
                              :stuck? (stuck? thread))))]
        (try
          (load-text target (in-wrapper original) {:file file
                                                   :line line})
          result
          (catch Throwable e
            (assoc result :restore-error (ex-message e))))))))

(def ^:private baseline-limit-ms 600000)

(defn baseline
  "Run the tests once on the unmutated namespace, recording which tests call each definition.

  Return {:ms … :covered … :note …}, or throw when the tests fail or run past the limit. When
  only the recording makes the tests fail, such as a test that compares a function with the one
  a value stored, or makes them run past a timeout that was given, the tests run again without
  it. The run then goes on without test selection, times its mutants by that plain run, and
  the note says so. A recorded run whose thread still runs after its timeout stops everything,
  since the plain run would share its state."
  [target def-names test-nss timeout-ms]
  (let [limit (or timeout-ms baseline-limit-ms)
        too-slow #(error/setup-error (str "The tests ran past " limit " ms before any mutation. Raise --timeout."))
        {:keys [outcome thread]
         :as recorded-run} (timed #(coverage target def-names test-nss limit))]
    (cond
      (= :passed outcome) {:ms (:ms recorded-run)
                           :covered (select-keys recorded-run [:tests :recorded :reached-outside :hooked? :test-count])}
      (or (stuck? thread) (and (= :timeout outcome) (not timeout-ms))) (throw (too-slow))
      :else (let [plain-run (timed #(run-tests test-nss limit))]
              (case (:outcome plain-run)
                :passed {:ms (:ms plain-run)
                         :covered {:recorded #{}}
                         :note (str "Recording which tests call each function made the tests fail or run too long, so"
                                    " every mutant runs every test and none is reported as having no coverage.")}
                :timeout (throw (too-slow))
                (throw (error/setup-error (str "The tests fail before any mutation; fix them first.\n"
                                               (:output plain-run)))))))))

(defn run-all
  "Run each mutant in turn, stopping at the first test run that keeps looping or the first restore that fails.

  The context's :on-result, when given, gets each result as soon as it is known, also the one
  the run stops at. A run that stops says why as :stop-reason: :plan-stuck, when the tests of a
  definition keep running without a mutation, :stuck, when those of a mutant do, or
  :restore-error."
  [context selected]
  (loop [[mutant & more] selected
         results []]
    (if (nil? mutant)
      {:results results}
      (let [{:keys [stuck? restore-error plan-stuck?]
             :as result} (run-mutant context mutant)]
        (if plan-stuck?
          {:results results
           :unreached (cons mutant more)
           :stop-reason :plan-stuck
           :stopped (str "the tests that call " (:def mutant) " keep running past the timeout without any mutation,"
                         " and would disturb every later run. Fix them, or raise --timeout.")}
          (let [results (conj results result)]
            (when-let [on-result (:on-result context)]
              (on-result result))
            (print (case (:outcome result) :passed "S" :no-coverage "N" :failed "." :unviable "U" :timeout "T"))
            (flush)
            (cond
              stuck? {:results results
                      :unreached more
                      :stop-reason :stuck
                      :stopped (str "the tests of the mutant at " (:row result) ":" (:col result) " " (:from result)
                                    " still run after the timeout, and would disturb every later run. Fix the loop,"
                                    " or raise --timeout.")}
              restore-error {:results results
                             :unreached more
                             :stop-reason :restore-error
                             :stopped (str "restoring " (:def result) " failed, so the namespace stays mutated: "
                                           restore-error)}
              :else (recur more results))))))))
