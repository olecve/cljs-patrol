(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [clojure.java.io]
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
  (doseq [[option value] [[:test ['a-test]] [:def ["f"]] [:since "main"] [:timeout 10] [:report "r.edn"]
                          [:html "r.html"] [:all-tests true]]]
    (is (= ["--apply and --restore run no tests, so they take no -t, -d, -s, --timeout, -r, --html or -a."]
           (mutate/usage-errors {:apply "3:8:>"
                                 option value}
                                'a))
        (str option))
    (is (= ["--apply and --restore run no tests, so they take no -t, -d, -s, --timeout, -r, --html or -a."]
           (mutate/usage-errors {:restore true
                                 option value}
                                nil))
        (str option))))

(deftest run-cli-applies-restores-and-refuses-run-while-applied
  (fixtures/with-temp-dir
    (fn [dir]
      (let [kept (str (clojure.java.io/file dir "applied.edn"))
            run (fn [& args]
                  (let [code (atom nil)
                        output (with-out-str (reset! code (mutate/run-cli args kept)))]
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
        (spit kept (pr-str {:file (str (clojure.java.io/file dir "core.clj"))
                            :original "a"
                            :applied "b"}))
        (let [[code output] (run "cljs-patrol.dev.mutate.fixtures.target" "-t" "cljs-patrol.dev.mutate.fixtures.target-test")]
          (is (= 2 code))
          (is (str/includes? output "A mutant is still applied to") output)
          (is (str/includes? output "Put the code back with --restore before a run. If --restore cannot finish, delete")))))))
