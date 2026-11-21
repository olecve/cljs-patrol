(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.report :as report]
   [cljs-patrol.dev.mutate.run :as run]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- run-target
  ([options] (run-target fixtures/target-source options))
  ([source options]
   (binding [*out* (java.io.StringWriter.)]
     (mutate/mutate-source 'cljs-patrol.dev.mutate.fixtures.target "target.clj" source
                           ['cljs-patrol.dev.mutate.fixtures.target-test] options))))

(deftest run-kills-and-keeps-mutants-and-restores-the-namespace
  (fixtures/with-target fixtures/target-tests
    (fn []
      (let [{:keys [totals not-covered mutants]} (run-target {})]
        (is (= ["unused"] not-covered))
        (is (= [["positive?" "0" "1" :killed] ["unused" "1" "0" :no-coverage]]
               (->> mutants
                    (filter #(re-matches #"\d+" (:from %)))
                    (map (juxt :def :from :to :outcome))))
            "number mutant runs with new number in its form")
        (is (= [["positive?" :killed] ["label" :killed] ["unused" :no-coverage]]
               (->> mutants
                    (filter #(= :body (:swap %)))
                    (map (juxt :def :outcome))))
            "body replaced by nil runs, and test that checks what function returns catches it")
        (is (= {:mutants 8
                :killed 4
                :survived 1
                :no-coverage 3
                :unviable 0
                :timeout 0}
               totals)
            "boundary at 0 is never tested, so > -> >= survives while 0 -> 1 is caught; no test calls unused"))
      (is (true? (:checked (meta (ns-resolve 'cljs-patrol.dev.mutate.fixtures.target 'positive?)))))
      (is (= :up ((ns-resolve 'cljs-patrol.dev.mutate.fixtures.target 'label) 1))
          "original code is back after the run"))))

(deftest failing-tests-stop-the-run-before-any-mutation
  (fixtures/with-target (str fixtures/target-tests "(deftest broken (is (= 1 2)))\n")
    (fn []
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The tests fail before any mutation"
                            (run-target {:timeout 5000}))
          "also with a timeout given"))))

(deftest mutants-run-only-the-tests-that-call-them
  (fixtures/with-target (str fixtures/target-tests
                             "(def unrelated-runs (atom 0))\n"
                             "(deftest unrelated (swap! unrelated-runs inc) (is true))\n")
    (fn []
      (let [{:keys [tests-run]} (run-target {:defs ["positive?" "label"]})]
        (is (= {"positive?" :subset
                "label" :subset}
               tests-run)
            "plan is made on the unmutated namespace, so a killed first mutant keeps the selection"))
      (is (= 1 @@(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'unrelated-runs))
          "test that calls nothing of the target runs only in the first run"))))

(def ^:private source-with-handlers (str fixtures/target-source "(def handlers {:label label})\n"))

(deftest recording-that-breaks-the-tests-turns-selection-off
  (fixtures/with-target source-with-handlers
    (str fixtures/target-tests "(deftest same-function (is (= target/label (:label target/handlers))))\n")
    (fn []
      (let [{:keys [note totals]} (run-target source-with-handlers {})]
        (is (re-find #"every mutant runs every test" note))
        (is (zero? (:no-coverage totals))
            "nothing is reported as not covered without the recording")))))

(deftest all-tests-option-turns-selection-off
  (fixtures/with-target fixtures/target-tests
    (fn []
      (is (= {"positive?" :every
              "label" :every}
             (:tests-run (run-target {:defs ["positive?" "label"]
                                      :all-tests true})))))))

(deftest run-writes-the-html-report
  (fixtures/with-target fixtures/target-tests
    (fn []
      (let [file (java.io.File/createTempFile "mutate" ".html")]
        (try
          (run-target {:html (str file)})
          (is (re-find #"<h1>The tests caught 4 of 8 planted bugs \(50%\)</h1>" (slurp file)))
          (finally (.delete file)))))))

(deftest run-names-killing-test-and-tests-that-ran
  (fixtures/with-target (str fixtures/target-tests "(deftest unrelated (is true))\n")
    (fn []
      (let [{:keys [mutants definitions]} (run-target {:defs ["label"]})]
        (is (= ["cljs-patrol.dev.mutate.fixtures.target-test/labels" "cljs-patrol.dev.mutate.fixtures.target-test/labels"]
               (keep :killed-by mutants)))
        (is (= [["label" ["cljs-patrol.dev.mutate.fixtures.target-test/labels"]]]
               (map (juxt :def :tests) definitions)))))))

(deftest run-skips-marked-mutants
  (fixtures/with-target fixtures/target-tests
    (fn []
      (let [marked (str/replace fixtures/target-source
                                "(> x 0))\n"
                                "(> x 0)) ; mutate: skip >: the boundary is checked elsewhere\n")
            marked (str marked "; mutate: skip if: stale\n")
            summary (atom nil)
            output (with-out-str
                     (reset! summary (mutate/mutate-source 'cljs-patrol.dev.mutate.fixtures.target "target.clj" marked
                                                           ['cljs-patrol.dev.mutate.fixtures.target-test] {})))]
        (is (= 7 (:mutants (:totals @summary)))
            "marked mutant does not run")
        (is (= [["positive?" ">" "the boundary is checked elsewhere"]]
               (map (juxt :def :from :reason) (:ignored @summary))))
        (is (str/includes? output "Warning: the skip marker on line 5 (if) matches no mutant"))
        (is (str/includes? output "7 mutants of cljs-patrol.dev.mutate.fixtures.target against"))
        (is (str/includes? output "target-test, 1 ignored, timeout"))
        (is (str/includes? output
                           "Ignored, with a skip marker:\n  2:32  > -> >=  in positive?: the boundary is checked elsewhere")
            "console names ignored mutant and its reason")))))

(deftest console-shows-how-to-apply-survivor
  (fixtures/with-target fixtures/target-tests
    (fn []
      (is (str/includes? (with-out-str
                           (mutate/mutate-source 'cljs-patrol.dev.mutate.fixtures.target "target.clj" fixtures/target-source
                                                 ['cljs-patrol.dev.mutate.fixtures.target-test] {:defs ["positive?"]}))
                         (str "To watch a new test fail on a survivor, apply it to the file, such as:\n"
                              "  clojure -M:test:mutate cljs-patrol.dev.mutate.fixtures.target --apply '2:32:>'"))))))

(deftest usage-errors-keep-apply-and-restore-apart-from-runs
  (is (= [] (mutate/usage-errors {:test ['a-test]} 'a)))
  (is (= [] (mutate/usage-errors {:apply "3:8:>"} 'a)))
  (is (= [] (mutate/usage-errors {:restore true} nil))
      "restore needs no target")
  (is (= ["Name the target namespace."] (mutate/usage-errors {:test ['a-test]} nil)))
  (is (= ["Name a test namespace with -t."] (mutate/usage-errors {} 'a)))
  (is (= ["Give --apply or --restore, not both."] (mutate/usage-errors {:apply "3:8:>"
                                                                        :restore true}
                                                                       'a)))
  (is (= ["Give --fresh or --iterate, not both."] (mutate/usage-errors {:test ['a-test]
                                                                        :fresh true
                                                                        :iterate true}
                                                                       'a)))
  (doseq [[option value] [[:test ['a-test]] [:def ["f"]] [:since "main"] [:timeout 10] [:report "r.edn"]
                          [:html "r.html"] [:all-tests true] [:fresh true] [:iterate true]]]
    (is (= ["--apply and --restore run no tests, so they take no -t, -d, -s, --timeout, -r, --html, -a, --fresh or --iterate."]
           (mutate/usage-errors {:apply "3:8:>"
                                 option value}
                                'a))
        (str option))
    (is (= ["--apply and --restore run no tests, so they take no -t, -d, -s, --timeout, -r, --html, -a, --fresh or --iterate."]
           (mutate/usage-errors {:restore true
                                 option value}
                                nil))
        (str option))))

(deftest run-cli-applies-restores-and-refuses-run-while-applied
  (fixtures/with-temp-dir
    (fn [dir]
      (let [kept (str (io/file dir "applied.edn"))
            run (fn [& args]
                  (let [code (atom nil)
                        output (with-out-str (reset! code (mutate/run-cli args kept (str (io/file dir "cache")))))]
                    [@code output]))]
        (is (= [2 "No mutant is applied, so there is nothing to restore.\n"] (run "--restore")))
        (let [[code output] (run "cljs-patrol.dev.mutate-html" "--apply" "999:1:if")]
          (is (= 2 code))
          (is (str/starts-with? output "No mutant at 999:1.")))
        (let [[code output] (run "app.core" "app.other" "-t" "app.core-test")]
          (is (= 2 code))
          (is (str/starts-with? output "Give one target namespace, not app.core app.other.")))
        (let [[code output] (run "app.core")]
          (is (= 2 code))
          (is (str/starts-with? output "Name a test namespace with -t.")))
        (spit kept (pr-str {:file (str (io/file dir "core.clj"))
                            :original "a"
                            :applied "b"}))
        (let [[code output] (run "cljs-patrol.dev.mutate.fixtures.target" "-t" "cljs-patrol.dev.mutate.fixtures.target-test")]
          (is (= 2 code))
          (is (str/includes? output "A mutant is still applied to") output)
          (is (str/includes? output "Put the code back with --restore before a run. If --restore cannot finish, delete")))))))

(defn- run-cached
  "Run the fixture target with a cache in `file` under a key made from `tests`, and return [summary output]."
  ([file tests] (run-cached file tests {}))
  ([file tests options] (run-cached fixtures/target-source file tests options))
  ([source file tests options]
   (let [summary (atom nil)
         output (with-out-str
                  (reset! summary
                          (mutate/mutate-source 'cljs-patrol.dev.mutate.fixtures.target "target.clj" source
                                                ['cljs-patrol.dev.mutate.fixtures.target-test]
                                                (merge {:cache {:file file
                                                                :make-key #(cache/run-key
                                                                            {:sources (cache/text-hash (str source tests))
                                                                             :runner "runner"})}}
                                                       options))))]
     [@summary output])))

(defn- without-progress [summary]
  (-> summary
      (dissoc :progress)
      (update :mutants (partial mapv #(dissoc % :since-last-run)))))

(def ^:private counted-tests
  (str fixtures/target-tests
       "(def runs (atom 0))\n"
       "(deftest counted (swap! runs inc) (is (keyword? (target/label 1))))\n"))

(deftest run-caches-outcomes-and-next-run-reuses-them-without-tests
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target counted-tests
        (fn []
          (let [file (io/file dir "cache" "target.edn")
                runs @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'runs)
                [first-run] (run-cached file counted-tests)
                runs-after-first @runs
                [second-run output] (run-cached file counted-tests)]
            (is (= 8 (count (:results (edn/read-string (slurp file))))))
            (is (every? :id (:mutants first-run)))
            (is (= "positive?/2:32/>->>=" (->> first-run :mutants (filter #(= :survived (:outcome %))) first :id)))
            (is (str/starts-with? output "All 8 mutants come from the last run, so no test runs.\n"))
            (is (not (str/includes? output ", timeout "))
                "no baseline, so no timeout")
            (is (= runs-after-first @runs)
                "no test runs, baseline included")
            (is (= first-run (without-progress second-run))
                "same report as run that ran every mutant")
            (is (= {:closed []
                    :new 0
                    :still-open 4
                    :not-run-before 0}
                   (:progress second-run)))))))))

(deftest run-goes-on-from-mutants-earlier-run-finished
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                _ (run-cached file fixtures/target-tests {:defs ["positive?"]})
                [summary output] (run-cached file fixtures/target-tests {})]
            (is (str/includes? output "Reused the last run's outcome for 3 of 8 mutants; running the rest."))
            (is (= {:mutants 8
                    :killed 4
                    :survived 1
                    :no-coverage 3
                    :unviable 0
                    :timeout 0}
                   (:totals summary))
                "same counts as run of every mutant")
            (is (= ["positive?" "label" "unused"] (->> summary :mutants (map :def) distinct))
                "mutants keep source order")))))))

(def ^:private looping-source
  (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
       "(defn done? [] true)\n"))

(def ^:private looping-tests
  (str "(ns cljs-patrol.dev.mutate.fixtures.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate.fixtures.target :as target]))\n"
       "(def release (atom false))\n"
       "(deftest waits (while (not (or (target/done?) @release))) (is true))\n"))

(deftest run-runs-mutant-whose-tests-kept-running-again-last
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target looping-source looping-tests
        (fn []
          (let [release @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'release)
                file (io/file dir "target.edn")
                run (fn [] (run-cached looping-source file looping-tests {:timeout 300}))]
            (try
              (let [[first-run first-output] (run)
                    _ (reset! release true)
                    [second-run second-output] (run)
                    [third-run third-output] (run)]
                (is (str/ends-with? (:stopped first-run)
                                    (str "The cache keeps the mutants that finished, so the next run runs the rest,"
                                         " and this one last.")))
                (is (= [["done?/2:16/body->nil" :timeout true]]
                       (map (juxt :id :outcome :stuck?) (:mutants first-run))))
                (is (str/includes? first-output "The next run runs it again, after every other mutant."))
                (is (not (str/includes? second-output "Reused"))
                    "loop not reused")
                (is (= [["done?/2:16/body->nil" :survived nil] ["done?/2:16/true->false" :survived nil]]
                       (map (juxt :id :outcome :stuck?) (:mutants second-run)))
                    "mutant that no longer loops gets its real outcome")
                (is (nil? (:stopped second-run)))
                (is (str/includes? third-output "All 2 mutants come from the last run, so no test runs."))
                (is (= (:totals second-run) (:totals third-run))))
              (finally (reset! release true)))))))))

(deftest stopped-run-without-cache-promises-no-resume
  (fixtures/with-target looping-source looping-tests
    (fn []
      (let [release @(ns-resolve 'cljs-patrol.dev.mutate.fixtures.target-test 'release)]
        (try
          (is (str/ends-with? (:stopped (binding [*out* (java.io.StringWriter.)]
                                          (mutate/mutate-source 'cljs-patrol.dev.mutate.fixtures.target "target.clj"
                                                                looping-source
                                                                ['cljs-patrol.dev.mutate.fixtures.target-test]
                                                                {:timeout 300})))
                              "or raise --timeout."))
          (finally (reset! release true)))))))

(deftest run-with-iterate-runs-only-gaps-and-shows-closed-ones
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                new-test "(deftest zero-is-not-positive (is (false? (target/positive? 0))))\n"
                _ (run-cached file fixtures/target-tests {})
                _ (run/load-text 'cljs-patrol.dev.mutate.fixtures.target-test new-test)
                [summary output] (run-cached file (str fixtures/target-tests new-test) {:iterate true})]
            (is (str/includes? output (str "Reused the last run's outcome for 4 of 8 mutants (4 caught, 0 did not compile),"
                                           " as --iterate asks; running the rest.")))
            (is (= {:closed ["positive?/2:32/>->>="]
                    :new 0
                    :still-open 3
                    :not-run-before 0}
                   (:progress summary)))
            (is (str/includes? output "Since the last run: 1 gap closed, 0 new, 3 still open.\n  Closed: positive?/2:32/>->>="))
            (is (= {:mutants 8
                    :killed 5
                    :survived 0
                    :no-coverage 3
                    :unviable 0
                    :timeout 0}
                   (:totals summary)))))))))

(deftest run-with-fresh-runs-every-mutant-again
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                _ (run-cached file fixtures/target-tests {})
                [summary output] (run-cached file fixtures/target-tests {:fresh true})]
            (is (str/starts-with? output "Running every mutant again, as --fresh asks.\n8 mutants of"))
            (is (= 4 (:still-open (:progress summary)))
                "progress still compares with last run")))))))

(deftest run-from-cache-keeps-note-of-last-baseline
  (let [tests (str fixtures/target-tests "(deftest same-function (is (= target/label (:label target/handlers))))\n")]
    (fixtures/with-temp-dir
      (fn [dir]
        (fixtures/with-target source-with-handlers tests
          (fn []
            (let [file (io/file dir "target.edn")
                  [first-run] (run-cached source-with-handlers file tests {})
                  [cached-run output] (run-cached source-with-handlers file tests {})]
              (is (re-find #"every mutant runs every test" (:note first-run)))
              (is (str/includes? output "so no test runs"))
              (is (= (:note first-run) (:note cached-run))
                  "note of baseline that did not run comes from cache"))))))))

(deftest run-with-iterate-reruns-only-definitions-edit-touched
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                marked (str/replace fixtures/target-source
                                    "(defn unused [x] (when x 1))\n"
                                    "(defn unused [x]\n  ; mutate: skip when: never called\n  (when x 1))\n")
                _ (run-cached file fixtures/target-tests {})
                [summary output] (run-cached marked file fixtures/target-tests {:iterate true})
                [_ plain-output] (run-cached marked file fixtures/target-tests {})]
            (is (str/includes? output (str "Reused the last run's outcome for 4 of 7 mutants (4 caught, 0 did not compile),"
                                           " as --iterate asks; running the rest. --iterate trusts the caught outcomes")))
            (is (= {:mutants 7
                    :killed 4
                    :survived 1
                    :no-coverage 2
                    :unviable 0
                    :timeout 0}
                   (:totals summary)))
            (is (= {"positive?" :every
                    "label" :every}
                   (->> summary :definitions (take 2) (into {} (map (juxt :def :tests)))))
                "definition with every outcome reused gets plan of these sources")
            (is (= {:closed []
                    :new 0
                    :still-open 1
                    :not-run-before 2}
                   (:progress summary))
                "untouched definition keeps its tag, edited one has none")
            (is (str/includes? plain-output "Reused the last run's outcome for 3 of 7 mutants; running the rest.")
                "run without --iterate runs again what --iterate trusted")))))))

(deftest failing-baseline-keeps-last-run-in-cache
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                _ (run-cached file fixtures/target-tests {})
                before (slurp file)]
            (run/load-text 'cljs-patrol.dev.mutate.fixtures.target-test "(deftest broken (is (= 1 2)))")
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The tests fail before any mutation"
                                  (run-cached file "changed tests" {})))
            (is (= before (slurp file)))))))))

(deftest cache-that-cannot-be-written-leaves-run-alone
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (spit (io/file dir "cache") "a file, not a directory")
          (let [file (io/file dir "cache" "target.edn")
                [summary output] (run-cached file fixtures/target-tests {})]
            (is (str/includes? output (str "Warning: the cache " file " failed, so the run goes on without it:")))
            (is (= 8 (:mutants (:totals summary))))
            (is (= 1 (report/exit-code summary)))))))))

(deftest run-with-iterate-that-reuses-every-outcome-still-plans-tests
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                _ (run-cached file fixtures/target-tests {:defs ["label"]})
                [summary output] (run-cached file "changed tests" {:defs ["label"]
                                                                   :iterate true})]
            (is (str/includes? output "All 2 mutants come from the last run, as --iterate asks; the tests run once"))
            (is (str/includes? output ", timeout ")
                "baseline ran")
            (is (= [["label" :every]] (map (juxt :def :tests) (:definitions summary))))))))))

(deftest run-keeps-progress-tags-of-untouched-definitions-after-edit
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/with-target fixtures/target-tests
        (fn []
          (let [file (io/file dir "target.edn")
                edited (str/replace fixtures/target-source
                                    "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
                                    "(ns cljs-patrol.dev.mutate.fixtures.target)\n\n(def added 1)\n")
                _ (run-cached file fixtures/target-tests {})
                [summary output] (run-cached edited file fixtures/target-tests {})]
            (is (str/includes? output "Running every mutant, since the sources changed"))
            (is (= [["positive?/4:32/>->>=" :still-open]]
                   (->> summary :mutants (filter #(= :survived (:outcome %))) (map (juxt :id :since-last-run)))))
            (is (= {:closed []
                    :new 0
                    :still-open 4
                    :not-run-before 0}
                   (:progress summary)))))))))

(def ^:private waiting-source
  (str "(ns cljs-patrol.dev.mutate.fixtures.target)\n"
       "(defn wait? [] false)\n"))

(def ^:private waiting-tests
  (str "(ns cljs-patrol.dev.mutate.fixtures.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate.fixtures.target :as target]))\n"
       "(deftest waits (when (target/wait?) (Thread/sleep 1500)) (is true))\n"
       "(deftest other (is true))\n"))

(deftest given-timeout-holds-for-selected-tests
  (fixtures/with-target waiting-source waiting-tests
    (fn []
      (is (= [["false" :timeout]]
             (->> (run-target waiting-source {:timeout 400})
                  :mutants
                  (filter #(= "false" (:from %)))
                  (map (juxt :from :outcome))))
          "mutant slower than given timeout times out, though its own plan allows more"))))
