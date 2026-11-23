(ns cljs-patrol.dev.mutate
  "Run mutation testing from the command line: plant one bug in a namespace at a time, and check its tests notice.

  This namespace holds the command line and ties the jobs together; each job has its own
  namespace under `cljs-patrol.dev.mutate.`, named in CLAUDE.md.

  The code under test changes, never the tests. A first run records which tests call each
  function. Each mutant changes one definition: it swaps one symbol or literal, such as `when`
  for `when-not`, `<` for `<=` or `0` for `1`, or it replaces the body of one function arity
  with `nil`. That form is evaluated over the namespace, the tests that call it run until the
  first failure, and the original form is evaluated back. A mutant the tests still pass
  on is a survivor: a branch no test pins. A function no test calls has no coverage. A mutant
  that cannot be read or compiled is unviable, and one whose tests run too long timed out;
  neither is scored."
  (:require
   [cljs-patrol.dev.mutate.apply :as apply]
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.error :as error]
   [cljs-patrol.dev.mutate.forms :as forms]
   [cljs-patrol.dev.mutate.git :as git]
   [cljs-patrol.dev.mutate.guard :as guard]
   [cljs-patrol.dev.mutate.report :as report]
   [cljs-patrol.dev.mutate.run :as run]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clojure.tools.cli :as cli])
  (:gen-class))

(defn- source-file [ns-sym]
  (let [path (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (or (some #(some-> (io/resource (str path %)) io/file) [".clj" ".cljc"])
        (throw (error/setup-error (str "No source for " ns-sym " on the classpath."))))))

(defn mutate-source
  "Mutate `target`, whose `source` is loaded from `file`, against `test-nss`, print the report, and return it.

  The tests must pass before any mutation, whether or not `timeout` is given. The report is
  also written to `report` when given. With `cache`, {:file … :make-key …}, the outcomes go
  to that file as the run goes, and the outcomes it already holds are reused as
  [[cache/open!]] decides, with `fresh` and `iterate`. A definition with no test plan from this
  run or the cache shows the tests the baseline saw call it. When a run with the same key
  reuses every mutant, no test runs, the baseline included. After the report, the cache
  records what the run reported as the last run. With `guard-root`, the run stops when a
  mutant that loaded, while it loads or in its tests, adds, changes or deletes a file or folder
  under that folder. Its `.git`, the cache folder, the folders of editors, IDEs and tools, and
  the swap, lock and backup files of editors do not count."
  [target file source test-nss {:keys [defs since timeout report html all-tests cache fresh iterate guard-root]}]
  (let [lines (when since (git/lines-since file since))
        {:keys [defs-in-scope loaded-once const-defs other-forms selected ignored unused-markers]
         top-forms :forms}
        (forms/scope source {:defs defs
                             :lines lines})
        selected (cache/with-ids selected)
        ignored (cache/with-ids ignored)
        _ (doseq [marker unused-markers]
            (println (str "Warning: the skip marker on " marker " matches no mutant; remove it or fix its operator.")))
        store (cache/open! (assoc (when (seq selected) cache)
                                  :current-places #(->> top-forms
                                                        forms/mutants
                                                        cache/with-ids
                                                        (into {} (map (juxt :place :id))))
                                  :fresh fresh
                                  :iterate iterate))
        {:keys [reused to-run message]} (cache/plan-run store selected)
        _ (some-> message println)
        def-names (distinct (map :def selected))
        baseline? (and (seq selected) (or (seq to-run) (= :iterate (:mode store))))
        skipped (when guard-root (guard/skipped-dirs guard-root (some-> cache :file io/file .getParentFile)))
        before-baseline (when (and baseline? guard-root) (guard/snapshot guard-root skipped))
        {:keys [ms covered note]} (when baseline?
                                    (run/baseline target def-names test-nss timeout))
        file-guard (when before-baseline
                     (guard/start guard-root skipped before-baseline (guard/snapshot guard-root skipped)))
        timeout-ms (when ms (or timeout (run/default-timeout-ms ms)))
        _ (println (format "%d mutants of %s against %s%s%s%s"
                           (count selected) target (str/join ", " test-nss)
                           (if (seq ignored) (str ", " (count ignored) " ignored") "")
                           (if since (str ", lines changed since " since) "")
                           (cond
                             timeout (str ", timeout " timeout " ms")
                             timeout-ms (str ", timeout " timeout-ms " ms for every test, less for selected tests")
                             :else "")))
        plans (atom {})
        context {:target target
                 :file (str file)
                 :test-nss test-nss
                 :timeout-ms timeout-ms
                 :fixed-timeout? (some? timeout)
                 :select? (not all-tests)
                 :covered covered
                 :plans plans
                 :guard file-guard
                 :on-result #(cache/record! store % @plans note)
                 :on-forget #(cache/forget! store %)}
        {:keys [results unreached stopped stop-reason]} (run/run-all context to-run)
        fresh-results (into {} (map (juxt :id identity)) results)
        reported (keep #(or (get fresh-results (:id %))
                            (some->> (get reused (:id %)) (merge %)))
                       selected)
        places (into {} (map (juxt :id :place)) selected)
        report-plans (select-keys (merge (run/recorded-plans covered def-names (not all-tests))
                                         (cache/cached-plans store)
                                         @plans)
                                  def-names)
        summary (cache/with-progress
                  (report/summarize {:defs-in-scope defs-in-scope
                                     :loaded-once loaded-once
                                     :const-defs const-defs
                                     :other-forms other-forms
                                     :unreached unreached
                                     :results reported
                                     :ignored ignored
                                     :stopped (when stopped
                                                (str stopped (when @(:file store)
                                                               (cache/stop-note stop-reason @(:recorded? store)))))
                                     :note (cache/run-note store note def-names)
                                     :plans report-plans})
                  store
                  places)]
    (report/print-report summary {:html html
                                  :report report
                                  :since since
                                  :source-lines (str/split-lines source)
                                  :target target
                                  :cached? (some? @(:file store))})
    (cache/finish! store (into {} (map (juxt (comp places :id) :outcome)) (:mutants summary)) report-plans)
    (when report
      (spit report (with-out-str (pprint/pprint (assoc summary
                                                       :target target
                                                       :tests test-nss
                                                       :since since))))
      (println (str "Report written to " report)))
    (when html
      (spit html ((requiring-resolve 'cljs-patrol.dev.mutate-html/page) summary {:target target
                                                                                 :file (apply/relative-path file)
                                                                                 :source source
                                                                                 :test-nss test-nss
                                                                                 :test-names (->> (run/test-vars-of test-nss)
                                                                                                  (map report/test-name)
                                                                                                  sort)
                                                                                 :since since}))
      (println (str "HTML report written to " html)))
    summary))

(defn mutate
  "Mutate `target`, found on the classpath, against `test-nss`, as [[mutate-source]] does, cached in `cache-dir`.

  The cache key hashes every source the target and its tests may read, from the directory the
  command runs from and the classpath directories under it, and the runner's own. The run stops
  when a test changes a file in the directory the command runs from."
  [target test-nss {:keys [cache-dir]
                    :as options}]
  (let [file (source-file target)
        source (slurp file)]
    (try
      (require target)
      (run! require test-nss)
      (catch Exception e
        (throw (error/setup-error (str "Could not load the namespaces: " (ex-message e))))))
    (mutate-source target file source test-nss
                   (assoc options
                          :cache (cache/project-cache cache-dir "." target test-nss options)
                          :guard-root "."))))

(def ^:private cli-options
  [["-t" "--test NS" "A test namespace to run, given once per namespace"
    :multi true
    :default []
    :update-fn conj
    :parse-fn symbol]
   ["-d" "--def NAME" "Mutate only inside this top-level definition, given once per name"
    :multi true
    :default []
    :update-fn conj]
   ["-s" "--since REF" "Mutate only lines the branch changed since it left this git ref, such as main"]
   [nil "--timeout MS" "Milliseconds one test run may take; by default 1.5 times the first run plus 2 s"
    :parse-fn parse-long
    :validate [pos-int? "Must be a positive whole number of milliseconds"]]
   ["-r" "--report FILE" "Write every mutant and its outcome to this EDN file"]
   [nil "--html FILE" "Write the report as an HTML page that leads with the test gaps and how to close them"]
   ["-a" "--all-tests" "Run every test for each mutant, as for a test that only reads state another test wrote"]
   [nil "--fresh" "Run every mutant again, and replace the outcomes the last run cached"]
   [nil "--iterate" "Reuse the last run's caught mutants though the code changed, and run only the rest"]
   [nil "--apply 'ROW:COL:OPERATOR'" "Write the mutant at this place into the file, and run nothing; needs no -t"]
   [nil "--restore" "Put back the code that --apply replaced; needs no target"]])

(def ^:private usage
  (str "Usage: clojure -M:test:mutate TARGET-NS -t TEST-NS [-t TEST-NS …] [-d DEF …] [-s REF] [--timeout MS]"
       " [-r FILE] [--html FILE] [-a] [--fresh | --iterate]\n"
       "       clojure -M:test:mutate TARGET-NS --apply 'ROW:COL:OPERATOR'\n"
       "       clojure -M:test:mutate --restore"))

(defn usage-errors
  "Return what is wrong with the parsed command-line `options` and `target`, each as a sentence; none means it can run."
  [options target]
  (let [editing? (or (:apply options) (:restore options))
        run-option? (or (seq (:test options))
                        (seq (:def options))
                        (some #(some? (get options %)) [:since :timeout :report :html :all-tests :fresh :iterate]))]
    (cond-> []
      (and (:apply options) (:restore options))
      (conj "Give --apply or --restore, not both.")

      (and editing? run-option?)
      (conj "--apply and --restore run no tests, so they take no -t, -d, -s, --timeout, -r, --html, -a, --fresh or --iterate.")

      (and (:fresh options) (:iterate options))
      (conj "Give --fresh or --iterate, not both.")

      (and (not (:restore options)) (nil? target))
      (conj "Name the target namespace.")

      (and (not editing?) (empty? (:test options)))
      (conj "Name a test namespace with -t."))))

(defn run-cli
  "Run the command line `args`, print what happened, and return the exit code.

  `kept` is where --apply keeps text, and `cache-dir` where a run keeps its outcomes.

  Exit 0 when no mutant survives or lacks coverage, 1 when one does, and 2 when the run cannot
  start or stops early. --apply writes one mutant into the file and --restore puts the code back;
  each returns 0, or 2 when it cannot. A run does not start while a mutant is applied, so no
  outcome of applied code is cached."
  [args kept cache-dir]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        target (some-> (first arguments) symbol) ; mutate: skip first: more than one target stops the run
        errors (cond-> (into (vec errors) (usage-errors options target))
                 (next arguments) (conj (str "Give one target namespace, not " (str/join " " arguments) ".")))]
    (if (seq errors)
      (do (println (str/join "\n" (concat errors [usage summary])))
          2)
      (try
        (cond
          (:restore options) (do (println (apply/restore-mutant kept))
                                 0)
          (:apply options) (do (println (apply/apply-mutant (source-file target) (:apply options) kept))
                               0)
          (apply/applied-mutant kept) (throw (error/setup-error
                                              (str "A mutant is still applied to "
                                                   (-> kept apply/applied-mutant :file apply/relative-path)
                                                   ". Put the code back with --restore before a run. If --restore"
                                                   " cannot finish, delete " kept " once you have what you need"
                                                   " from it.")))
          :else (report/exit-code (mutate target (:test options) {:defs (:def options)
                                                                  :since (:since options)
                                                                  :timeout (:timeout options)
                                                                  :report (:report options)
                                                                  :html (:html options)
                                                                  :all-tests (:all-tests options)
                                                                  :fresh (:fresh options)
                                                                  :iterate (:iterate options)
                                                                  :cache-dir cache-dir})))
        (catch Throwable e
          (println (if (::error/setup (ex-data e)) (ex-message e) (str "The run failed: " e)))
          2)))))

(defn -main
  "Run the command line `args` as [[run-cli]] does, and exit with its code."
  [& args]
  (System/exit (run-cli args apply/applied-file cache/default-dir)))
