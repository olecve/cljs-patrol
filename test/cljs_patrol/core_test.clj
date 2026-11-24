(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.memory-fs :as memory-fs]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.tools.cli :as cli])
  (:import
   (java.nio.file
    Files
    FileSystems
    LinkOption)))

(def ^:private filter-groups #'core/filter-groups)
(def ^:private filter-run-results #'core/filter-run-results)
(def ^:private assemble-groups #'core/assemble-groups)
(def ^:private default-groups (assemble-groups {} false))

(deftest filter-run-results-test
  (let [item-a {:kw :a/sub
                :file "src/a.cljs"
                :row 1}
        item-b {:kw :b/sub
                :file "src/b.cljs"
                :row 1}
        run-results [{:source-dir (disk/path "src")
                      :group-results [{:unused-subs [item-a item-b]
                                       :unused-events []}]}]]
    (testing "filters items to requested files only"
      (let [result (filter-run-results run-results (FileSystems/getDefault) ["" (str (disk/path "src/a.cljs"))])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (= 1 (count subs)))
        (is (= "src/a.cljs" (:file (first subs))))))

    (testing "empty result when no files match"
      (let [result (filter-run-results run-results
                                       (FileSystems/getDefault)
                                       [(str (.toAbsolutePath (disk/path "src/other.cljs")))])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (empty? subs))))))

(deftest filter-groups-test
  (testing "no filters returns all groups"
    (is (= 14 (count (filter-groups default-groups {})))))

  (testing "--only selects specific group"
    (let [groups (filter-groups default-groups {:only #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups default-groups {:disable #{:spade}})]
      (is (= 13 (count groups)))
      (is (= #{:re-frame :reagent :typography :focus-ring :forbid-props :a11y :docstrings :css-order :tests :react-testing-library-cljs :layout :idioms :comments}
             (set (map group/group-id groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups default-groups {:only #{:re-frame}
                                                :disable #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups)))))))

(def ^:private fixture-dir "test-projects/re-frame-spade-app/src/webapp")

(deftest baseline-write-integration-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run (disk/typed-path fixture-dir) enabled-groups)]
        identities (baseline/collect-identities run-results)]
    (with-open [file-system (memory-fs/unix)]
      (let [path (memory-fs/path file-system "/work/missing/baseline.edn")]
        (baseline/write-baseline path identities)
        (is (Files/isRegularFile path (make-array LinkOption 0))
            "baseline file created")
        (let [{:keys [ok]} (baseline/read-baseline (memory-fs/typed path))]
          (is (set? ok)
              "reads back as a set")
          (is (pos? (count ok))
              "contains issues from fixture project")
          (is (every? :rule ok)
              "every identity has a :rule"))))))

(defn- capture-exit
  "Call `f`, and give {:exit the code it exits with, :out what it prints, :err what it prints on stderr}.

  The exit throws instead of stopping the JVM. Each exit of `-main` goes through `exit!`."
  [f]
  (let [exit (atom nil)
        err (java.io.StringWriter.)
        out (with-out-str
              (binding [*err* err]
                (with-redefs [core/exit! (fn [_ code]
                                           (reset! exit code)
                                           (throw (ex-info "exit" {})))]
                  (try
                    (f)
                    (catch clojure.lang.ExceptionInfo _
                      nil)))))]
    {:err (str err)
     :exit @exit
     :out out}))

(defn- run-main [& args]
  (capture-exit #(apply core/-main args)))

(deftest main-reports-missing-source-dirs-as-typed
  (is (= {:err ""
          :exit 1
          :out "Error: no such path: , a\u0000b, test-projects/no-such-app\n"}
         (run-main "" "test-projects/a11y-app/src" "a\u0000b" "test-projects/no-such-app" "test-projects/a11y-app/src/"))
      "empty, unaccepted and missing dirs are reported as typed, in order"))

(deftest main-tells-on-stderr-which-groups-read-clj
  (is (str/includes? (:err (run-main "--experimental-clj" "--only" "docstrings" "test-projects/clj-app"))
                     "Experimental: reading .clj files with docstrings.\n")
      "group that reads .clj is named")
  (is (str/includes? (:err (run-main "--experimental-clj" "--only" "a11y" "test-projects/clj-app"))
                     "WARN: --experimental-clj does nothing here.")
      "no group that reads .clj gives warning")
  (is (not (str/includes? (:err (run-main "--only" "docstrings" "test-projects/clj-app")) "Experimental"))
      "no notice without experimental mode"))

(deftest main-exits-through-exit-on-bad-arguments
  (is (= {:err ""
          :exit 1
          :out "Error: --allow-new only applies to --baseline-write.\n"}
         (run-main "--allow-new" "src"))
      "argument error exits 1")
  (doseq [[args code] [[["--help"] 0]
                       [["--jobs" "0" "src"] 1]
                       [["--baseline-write" "--baseline" "src"] 1]
                       [["--baseline-write" "--files" "a.cljs" "src"] 1]
                       [["--fail-on" "no-such-rule" "src"] 1]
                       [["--list-rules"] 0]
                       [["--only" "a11y"] 1]]]
    (is (= code (:exit (apply run-main args)))
        (str "exit code of " (pr-str args)))))

(defn- write-baseline-at
  "Run the baseline write of `-main` on `run-results` with the configured baseline path `configured`."
  [run-results sources configured allow-new]
  (capture-exit #(#'core/run-baseline-write! [a11y/group]
                                             run-results
                                             {:allow-new allow-new}
                                             (baseline/resolve-baseline-path configured (map :path sources))
                                             {})))

(defn- compare-baseline-at [run-results sources configured]
  (capture-exit #(#'core/run-baseline-compare! [a11y/group]
                                               run-results
                                               {:fail-on-rules #{}}
                                               sources
                                               (baseline/resolve-baseline-path configured (map :path sources))
                                               {})))

(deftest configured-baseline-path-prints-as-typed
  (with-open [file-system (memory-fs/unix)]
    (let [src (memory-fs/path file-system "/work/src")
          sources [(memory-fs/typed src)]
          configured "/work/./ci//baseline.edn"
          one-finding (fn []
                        (memory-fs/write! (.resolve src "a.cljs") "(ns a)\n(defn v [] [:img {:src \"x\"}])\n")
                        [(core/run (first sources) [a11y/group])])]
      (is (= {:err ""
              :exit 1
              :out (str "Error: Baseline file not found: " configured "\nRun --baseline-write first to create one.\n")}
             (compare-baseline-at (one-finding) sources configured))
          "missing file named as typed")
      (is (= {:err ""
              :exit 0
              :out (str "Wrote baseline with 1 entries to " configured "\n")}
             (write-baseline-at (one-finding) sources configured false))
          "write names file as typed")
      (memory-fs/write! (.resolve src "b.cljs") "(ns b)\n(defn w [] [:img {:src \"y\"}])\n")
      (is (str/includes? (:out (write-baseline-at [(core/run (first sources) [a11y/group])] sources configured false))
                         (str "\nRefusing to write " configured ": the findings above are not in it."))
          "refusal names file as typed")
      (is (= 1 (:exit (compare-baseline-at [(core/run (first sources) [a11y/group])] sources configured)))
          "compare reads file at typed path"))))

(defn- start-baseline-file
  "Run the start-up baseline check of `-main` with `opts`, and give {:err :exit :out} with :file, the baseline file it gives."
  [opts]
  (let [file (atom nil)]
    (assoc (capture-exit #(reset! file (#'core/start-baseline-file opts [(disk/typed-path "test-projects/a11y-app/src")])))
           :file @file)))

(deftest configured-baseline-path-that-names-no-file-stops-run-at-start
  (doseq [configured ["" "a\u0000b"]]
    (is (= {:err ""
            :exit 1
            :out (str "Error: Baseline file not found: " configured "\nRun --baseline-write first to create one.\n")
            :file nil}
           (start-baseline-file {:baseline true
                                 :baseline-path configured}))
        (str "compare gives not-found error for " (pr-str configured)))
    (is (= {:err ""
            :exit 1
            :out (str "Error: baseline path names no file: " (pr-str configured) "\n")
            :file nil}
           (start-baseline-file {:baseline-write true
                                 :baseline-path configured}))
        (str "write gives error for " (pr-str configured))))
  (is (= {:err ""
          :exit nil
          :out ""
          :file (disk/typed-path "test-projects/a11y-app/src/.cljs-patrol/baseline.edn")}
         (start-baseline-file {:baseline true}))
      "default baseline file of compare")
  (is (= {:err ""
          :exit nil
          :out ""
          :file {:path (disk/path "ci/b.edn")
                 :text "ci//b.edn"}}
         (start-baseline-file {:baseline-write true
                               :baseline-path "ci//b.edn"}))
      "configured baseline file of write")
  (is (= {:err ""
          :exit nil
          :out ""
          :file nil}
         (start-baseline-file {:baseline-path ""}))
      "run without baseline has no baseline file"))

(deftest console-findings-throw-on-nil-source-dir
  (is (thrown? NullPointerException
               (doall (core/console-findings [re-frame/group]
                                             [{:source-dir nil
                                               :group-results [{:deprecated-effects []}]}]
                                             (core/baseline-severity #{} #{}))))))

(deftest main-writes-baseline-under-first-source-dir-and-compares-with-it
  (let [dir (temp-dir/create-dir "cljs-patrol")
        src (str dir "/src")
        baseline-path (str src "/.cljs-patrol/baseline.edn")]
    (try
      (.mkdirs (java.io.File. src))
      (spit (str src "/a.cljs") "(ns a)\n(defn v [] [:img {:src \"x\"}])\n")
      (is (= {:err (str "Analyzing 1 files under " src " ...\n")
              :exit 0
              :out (str "Wrote baseline with 1 entries to " baseline-path "\n")}
             (run-main "--only" "a11y" "--baseline-write" src))
          "write exits 0 and names baseline beside first source dir")
      (is (= [{:rule :img-alt-missing
               :tag :img
               :file "a.cljs"
               :form "[:img {:src \"x\"}]"}]
             (:issues (edn/read-string (slurp baseline-path))))
          "key is relative to source dir")
      (is (= 0 (:exit (run-main "--only" "a11y" "--baseline" src)))
          "compare finds nothing new")
      (is (= 1 (:exit (run-main "--only" "a11y" src)))
          "run without baseline fails on finding")
      (finally
        (temp-dir/delete-dir! dir)))))

(deftest baseline-write-refusal-test
  (let [kept {:rule :unused-subs
              :key :app/kept}
        added {:rule :unused-subs
               :key :app/added}]
    (with-open [file-system (memory-fs/unix)]
      (let [path (memory-fs/path file-system "/work/baseline.edn")
            baseline-file (memory-fs/typed path)]
        (is (nil? (core/baseline-write-refusal baseline-file #{kept} false))
            "first write goes through")
        (baseline/write-baseline path #{kept})
        (is (nil? (core/baseline-write-refusal baseline-file #{kept} false))
            "unchanged findings go through")
        (is (nil? (core/baseline-write-refusal baseline-file #{} false))
            "fixed findings go through")
        (is (= {:added #{added}}
               (core/baseline-write-refusal baseline-file #{kept added} false))
            "added finding is refused")
        (is (nil? (core/baseline-write-refusal baseline-file #{kept added} true))
            "allow-new writes added finding")
        (memory-fs/write! path "[1 2 3]")
        (is (some? (:error (core/baseline-write-refusal baseline-file #{kept} false)))
            "unreadable baseline is refused")
        (is (nil? (core/baseline-write-refusal baseline-file #{kept} true))
            "allow-new overwrites unreadable baseline")))))

(deftest baseline-compare-integration-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run (disk/typed-path fixture-dir) enabled-groups)]
        found (baseline/collect-identities run-results)]

    (testing "all issues in baseline — nothing new"
      (let [{:keys [new present fixed]} (baseline/diff-baseline found found)]
        (is (empty? new))
        (is (= found present))
        (is (empty? fixed))))

    (testing "empty baseline — everything is new"
      (let [{:keys [new present fixed]} (baseline/diff-baseline #{} found)]
        (is (= found new))
        (is (empty? present))
        (is (empty? fixed))))

    (testing "extra baseline issue — shows as fixed"
      (let [extra {:rule :unused-subs
                   :key :app/gone}
            baseline-with-extra (conj found extra)
            {:keys [new present fixed]} (baseline/diff-baseline baseline-with-extra found)]
        (is (empty? new))
        (is (= found present))
        (is (= #{extra} fixed))))))

(def ^:private blocking-issue
  {:kw :app/a
   :file "a.cljs"
   :row 1
   :tier :bugs})

(def ^:private non-blocking-issue
  {:kw :app/b
   :file "b.cljs"
   :row 2
   :tier :cleanup})

(deftest standalone-failed?-test
  (testing "without --fail-on, falls back to group/failed?"
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [blocking-issue]}]}]]
      (is (true? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                           :run-results run-results}))
          ":unused-subs causes group/failed? to return truthy"))
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs []
                                         :phantom-subs [non-blocking-issue]}]}]]
      (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                            :run-results run-results}))
          "phantom-subs alone does not trigger group/failed?")))

  (testing "with --fail-on rule set, only listed rules cause failure"
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [non-blocking-issue]
                                         :phantom-subs []}]}]]
      (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                            :run-results run-results
                                            :fail-on-rules #{:phantom-subs}}))
          "unused-subs issues don't fail when only phantom-subs is selected"))
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs []
                                         :phantom-subs [non-blocking-issue]}]}]]
      (is (true? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                           :run-results run-results
                                           :fail-on-rules #{:phantom-subs}}))
          "phantom-subs issues fail when phantom-subs is in fail-on set")))

  (testing "empty run-results never fails"
    (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                          :run-results []})))
    (is (false? (core/standalone-failed? {:enabled-groups [re-frame/group]
                                          :run-results []
                                          :fail-on-rules #{:phantom-subs}})))))

(def ^:private new-bug
  {:rule :duplicate-subs
   :key :app/dup})

(def ^:private new-cleanup
  {:rule :unused-subs
   :key :app/unused})

(def ^:private fixed-issue
  {:rule :unused-subs
   :key :app/gone})

(deftest baseline-failed?-test
  (testing "without --fail-on (nil rules)"
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{}}))
        "no new, no fixed -> pass")
    (is (true? (core/baseline-failed? {:new-issues #{new-cleanup}
                                       :fixed-issues #{}}))
        "any new issue fails when fail-on is unset")
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{fixed-issue}}))
        "fixed without strict -> pass")
    (is (true? (core/baseline-failed? {:new-issues #{}
                                       :fixed-issues #{fixed-issue}
                                       :strict-baseline true}))
        "fixed with strict -> fail"))

  (testing "with --fail-on bugs"
    (let [fail-on #{:duplicate-subs :duplicate-events}]
      (is (true? (core/baseline-failed? {:new-issues #{new-bug}
                                         :fixed-issues #{}
                                         :fail-on-rules fail-on}))
          "new bug fails")
      (is (false? (core/baseline-failed? {:new-issues #{new-cleanup}
                                          :fixed-issues #{}
                                          :fail-on-rules fail-on}))
          "new cleanup does not fail when only bugs selected")
      (is (true? (core/baseline-failed? {:new-issues #{new-bug new-cleanup}
                                         :fixed-issues #{}
                                         :fail-on-rules fail-on}))
          "mixed new issues: bug still fails")))

  (testing "baseline issues never fail (that's the point)"
    (is (false? (core/baseline-failed? {:new-issues #{}
                                        :fixed-issues #{}
                                        :fail-on-rules #{:duplicate-subs}}))
        "no new issues, even with fail-on set"))

  (testing "fixed issues with strict ignore fail-on tier"
    (is (true? (core/baseline-failed? {:new-issues #{}
                                       :fixed-issues #{fixed-issue}
                                       :fail-on-rules #{:duplicate-subs}
                                       :strict-baseline true}))
        "strict fails on fixed regardless of tier"))

  (testing "combined: new bug + fixed cleanup, with strict and fail-on bugs"
    (is (true? (core/baseline-failed? {:new-issues #{new-bug}
                                       :fixed-issues #{fixed-issue}
                                       :fail-on-rules #{:duplicate-subs}
                                       :strict-baseline true})))))

(deftest baseline-with-files-filter-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run (disk/typed-path fixture-dir) enabled-groups)]
        all-ids (baseline/collect-identities run-results)
        filtered-results (#'core/filter-run-results
                          run-results
                          (FileSystems/getDefault)
                          [(str (.toAbsolutePath (disk/path fixture-dir "subs.cljs")))])
        filtered-ids (baseline/collect-identities filtered-results)]
    (is (< (count filtered-ids) (count all-ids))
        "filtering reduces issue count")
    (is (every? #(contains? all-ids %) filtered-ids)
        "filtered ids are a subset of all ids")))

(deftest standalone-severity-test
  (let [label (fn [fail-on-rules rule]
                ((core/standalone-severity fail-on-rules) {:group re-frame/group
                                                           :items [{:kw :app/sub}]
                                                           :rule rule}))]
    (is (= "error" (label nil :duplicate-subs))
        "rule that fails its group labeled error")
    (is (= "warning" (label nil :phantom-subs))
        "rule that only reports labeled warning")
    (is (= "info" (label nil :dynamic-sites))
        "info-only rule labeled info")
    (is (= "error" (label #{:phantom-subs} :phantom-subs))
        "--fail-on rule labeled error")
    (is (= "warning" (label #{:phantom-subs} :duplicate-subs))
        "--fail-on replaces group's own check")
    (is (= "info" (label #{:phantom-subs} :dynamic-sites))
        "info-only rule outside --fail-on stays info")))

(deftest failed-splits-per-rule-test
  (doseq [g default-groups
          rule (keys (group/rule->tier g))]
    (let [items [{:kw :app/x
                  :file "a.cljs"
                  :row 1}]
          others (into {} (map (fn [other] [other []])) (keys (group/rule->tier g)))]
      (is (= (boolean (group/failed? g {rule items}))
             (boolean (group/failed? g (assoc others rule items))))
          (str (group/group-id g) " " rule " fails alone as it does among the other rules"))))
  (doseq [g default-groups
          :let [rules (keys (group/rule->tier g))
                items [{:kw :app/x
                        :file "a.cljs"
                        :row 1}]]]
    (is (= (boolean (group/failed? g (into {} (map (fn [rule] [rule items])) rules)))
           (boolean (some #(group/failed? g {% items}) rules)))
        (str (group/group-id g) " fails exactly when one of its rules fails alone"))))

(deftest baseline-severity-test
  (let [new-item {:kw :app/new
                  :file "a.cljs"
                  :row 1}
        old-item {:kw :app/old
                  :file "a.cljs"
                  :row 2}
        new-issues #{(baseline/issue->identity :unused-subs new-item)}
        label (fn [fail-on-rules item]
                ((core/baseline-severity new-issues fail-on-rules) {:item item
                                                                    :rule :unused-subs}))]
    (is (= "error" (label nil new-item))
        "new finding labeled error")
    (is (= "warning" (label #{:duplicate-subs} new-item))
        "new finding outside --fail-on labeled warning")
    (is (= "error" (label #{:unused-subs} new-item))
        "new finding in --fail-on labeled error")
    (is (= "baseline" (label nil old-item))
        "baselined finding labeled baseline")))

(defn- parse-jobs [& args]
  (let [{:keys [options errors]} (cli/parse-opts (vec args) core/cli-options)]
    {:errors errors
     :jobs (:jobs options)}))

(deftest jobs-option-test
  (is (= {:errors nil
          :jobs nil}
         (parse-jobs))
      "absent leaves default to processor count")
  (is (= {:errors nil
          :jobs 1}
         (parse-jobs "--jobs" "1")))
  (is (= {:errors nil
          :jobs 4}
         (parse-jobs "--jobs" "4")))
  (is (= {:errors nil
          :jobs 256}
         (parse-jobs "--jobs" "256"))
      "accepts limit")
  (is (= {:errors ["Failed to validate \"--jobs 257\": Must be at most 256"]
          :jobs nil}
         (parse-jobs "--jobs" "257"))
      "rejects value above limit")
  (doseq [value ["0" "-2" "abc" "1.5" "" "99999999999999999999"]]
    (is (= {:errors [(str "Failed to validate \"--jobs " value "\": Must be a positive integer")]
            :jobs nil}
           (parse-jobs "--jobs" value))
        (str "rejects " (pr-str value))))
  (is (= ["Missing required argument for \"--jobs N\""] (:errors (parse-jobs "--jobs")))
      "value required"))

(defn- thread-probe
  "Build a group that declares, for each file, the thread that read it."
  []
  (reify group/RuleGroup
    (group-id [_] :thread-probe)
    (group-name [_] "Thread probe")
    (analyze [_ {:keys [declarations]}] {:threads (vec declarations)})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})
    (parse-handlers [_]
      {:handle-file (fn [_ _ _]
                      {:decls [{:thread (Thread/currentThread)}]
                       :dynamics []
                       :usages []})})))

(deftest run-all-reads-files-on-jobs-threads
  (let [threads-with (fn [jobs]
                       (binding [*err* (java.io.StringWriter.)]
                         (->> (core/run-all [(disk/typed-path fixture-dir)] [(thread-probe)] nil jobs)
                              first :group-results first :threads (map :thread))))
        caller (Thread/currentThread)]
    (is (not-any? #(identical? caller %) (threads-with 1))
        "one job reads on worker")
    (is (not-any? #(identical? caller %) (threads-with 4))
        "four jobs read on workers")))
