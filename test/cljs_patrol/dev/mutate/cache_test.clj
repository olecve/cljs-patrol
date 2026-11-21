(ns cljs-patrol.dev.mutate.cache-test
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.forms :as forms]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(deftest mutant-id-names-definition-place-and-swap
  (is (= "holds-focus?/490:43/remove->filter"
         (cache/mutant-id {:def "holds-focus?"
                           :row 490
                           :col 43
                           :from "remove"
                           :to 'filter})))
  (is (= "f/2:16/body->nil"
         (cache/mutant-id {:def "f"
                           :row 2
                           :col 16
                           :from "body"
                           :to nil}))
      "body mutant")
  (is (= "f/2:16/80->81"
         (cache/mutant-id {:def "f"
                           :row 2
                           :col 16
                           :from "80"
                           :to 81}))
      "number at same place as body mutant"))

(defn- mutants-of [source] (cache/with-ids (:selected (forms/scope source {}))))

(defn- by-swap [source] (into {} (map (juxt #(str (:def %) " " (:from %)) identity)) (mutants-of source)))

(deftest with-id-names-place-by-definition-text-and-offset
  (let [before (by-swap "(ns a)\n(defn f [x] (when x 1))\n(defn g [y] (if y 2 3))\n")
        moved (by-swap "(ns a)\n\n\n(defn f [x] (when x 1))\n(defn g [y] (if y 2 3))\n")
        edited (by-swap "(ns a)\n(defn f [x] (when x 1))\n(defn g [y]\n  ; why\n  (if y 2 3))\n")
        same-offset (by-swap "(ns a)\n(defn f [x] (when x 1))\n(defn g [y] (if y 2 4))\n")
        twice "(ns a)\n(defn f [x] (when x 1))\n(defn f [x] (when x 1))\n"
        under-meta (by-swap "(ns a)\n^:x (defn f [x] (when x 1))\n")]
    (is (= "f/2:14/when->when-not" (:id (before "f when"))))
    (is (= "f/4:14/when->when-not" (:id (moved "f when")))
        "id holds line")
    (is (= (:place (before "f when")) (:place (moved "f when")))
        "moved definition keeps place")
    (is (not= (:place (before "f when")) (:place (before "f 1")))
        "each swap has own place")
    (is (= (:place (before "f when")) (:place (edited "f when")))
        "edit of other definition keeps place")
    (is (not= (:place (before "g if")) (:place (edited "g if")))
        "edited definition gets new place")
    (is (= 2 (->> twice mutants-of (filter #(= "when" (:from %))) (map :id) set count)))
    (is (= 1 (->> twice mutants-of (filter #(= "when" (:from %))) (map :place) set count))
        "each of two same definitions counts from its own start")
    (is (not= (:place (before "g if")) (:place (same-offset "g if")))
        "other text at same offset gives new place")
    (is (= (str (cache/text-hash "^:x (defn f [x] (when x 1))") "/0:17/when->when-not") (:place (under-meta "f when")))
        "place counts from metadata that starts definition, hashes its text")))

(def ^:private key-input
  {:sources "s1"
   :test-nss ['a-test]
   :runner "r1"
   :timeout nil
   :all-tests nil})

(deftest run-key-changes-with-each-input
  (let [base (cache/run-key key-input)]
    (is (= base (cache/run-key key-input)))
    (is (= ["a-test" "b-test"] (:test-nss (cache/run-key (assoc key-input :test-nss ['b-test 'a-test]))))
        "test namespaces in name order")
    (doseq [[input-key value] [[:sources "s2"]
                               [:test-nss ['a-test 'b-test]]
                               [:runner "r2"]
                               [:timeout 100]
                               [:all-tests true]]]
      (is (not= base (cache/run-key (assoc key-input input-key value)))
          (str input-key)))
    (is (= base (cache/run-key (assoc key-input :all-tests false)))
        "no -a and -a off are one option")))

(defn- write-file [dir path text]
  (let [file (io/file dir path)]
    (io/make-parents file)
    (spit file text)))

(def ^:private source-paths
  ["README.md" "deps.edn" "dev/d.clj" "docs/rules.md" "resources/r.css" "src/a.clj" "test/a_test.clj"
   "test-projects/p/x.cljs"])

(deftest sources-hash-covers-every-file-that-tests-may-read
  (fixtures/with-temp-dir
    (fn [dir]
      (doseq [path source-paths]
        (write-file dir path "x"))
      (let [base (cache/sources-hash dir cache/source-roots)]
        (is (re-matches #"[0-9a-f]{64}" base))
        (is (= base (cache/sources-hash dir cache/source-roots)))
        (write-file dir "notes.txt" "x")
        (write-file dir ".mutate-cache/a.edn" "x")
        (is (= base (cache/sources-hash dir cache/source-roots))
            "files outside source roots left out")
        (doseq [path source-paths]
          (write-file dir path "y")
          (is (not= base (cache/sources-hash dir cache/source-roots))
              path)
          (write-file dir path "x"))
        (write-file dir "src/b.clj" "")
        (is (not= base (cache/sources-hash dir cache/source-roots))
            "new empty file counts by its path")))))

(deftest sources-hash-tells-file-name-from-its-text
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/ab" "c")
      (let [one (cache/sources-hash dir cache/source-roots)]
        (.delete (io/file dir "src/ab"))
        (write-file dir "src/a" "bc")
        (is (not= one (cache/sources-hash dir cache/source-roots)))))))

(defn- git [dir & args] (apply shell/sh "git" "-C" (str dir) args))

(deftest sources-hash-in-git-counts-only-files-git-sees
  (fixtures/with-temp-dir
    (fn [dir]
      (git dir "init" "-q")
      (write-file dir ".gitignore" "*.tmp\n")
      (write-file dir "src/a.clj" "x")
      (git dir "add" "src/a.clj")
      (let [base (cache/sources-hash dir ["src"])]
        (write-file dir "src/report.tmp" "x")
        (is (= base (cache/sources-hash dir ["src"]))
            "ignored file left out")
        (write-file dir "src/b.clj" "x")
        (is (not= base (cache/sources-hash dir ["src"]))
            "untracked file that git does not ignore counts")
        (.delete (io/file dir "src/b.clj"))
        (.delete (io/file dir "src/a.clj"))
        (is (not= base (cache/sources-hash dir ["src"]))
            "tracked file gone from disk counts as gone")))))

(deftest sources-hash-outside-git-counts-every-file
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/a.clj" "x")
      (let [base (cache/sources-hash dir ["src"])]
        (write-file dir "src/.DS_Store" "x")
        (is (not= base (cache/sources-hash dir ["src"])))))))

(deftest classpath-roots-keep-directories-under-root
  (fixtures/with-temp-dir
    (fn [dir]
      (.mkdirs (io/file dir "src"))
      (.mkdirs (io/file dir "extra" "classes"))
      (spit (io/file dir "lib.jar") "")
      (is (= ["src" "extra/classes"]
             (cache/classpath-roots (str/join java.io.File/pathSeparator
                                              [(io/file dir "src") (io/file dir "extra" "classes") (io/file dir "lib.jar")
                                               (io/file dir "missing") (System/getProperty "java.io.tmpdir")])
                                    dir))
          "jar, missing and outside directories left out")
      (is (= ["."] (cache/classpath-roots (str dir) dir))
          "root itself on classpath is whole project"))))

(deftest sources-hash-keeps-text-inside-its-file
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/a" "x\nsrc/b\ny")
      (let [one (cache/sources-hash dir ["src"])]
        (write-file dir "src/a" "x\n")
        (write-file dir "src/b" "y")
        (is (not= one (cache/sources-hash dir ["src"]))
            "text moved into next file changes hash")))))

(deftest runner-version-keeps-text-inside-its-file
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "a.clj" "x\nb.clj\ny")
      (let [one (cache/runner-version (constantly [(io/file dir "a.clj")]))]
        (write-file dir "a.clj" "x\n")
        (write-file dir "b.clj" "y")
        (is (not= one (cache/runner-version (constantly [(io/file dir "a.clj") (io/file dir "b.clj")])))
            "text moved into next file changes hash")
        (is (= (cache/runner-version (constantly [(io/file dir "a.clj")]))
               (cache/runner-version (constantly [(io/file dir "a.clj")]))))))))

(deftest sources-hash-outside-git-skips-git-and-cache-directories
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/a.clj" "x")
      (let [base (cache/sources-hash dir ["."])]
        (write-file dir ".git/HEAD" "x")
        (write-file dir ".mutate-cache/a.edn" "x")
        (is (= base (cache/sources-hash dir ["."])))
        (write-file dir "src/b.clj" "x")
        (is (not= base (cache/sources-hash dir ["."])))))))

(deftest project-key-hashes-project-and-names-tests-and-options
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/a.clj" "x")
      (let [options {:timeout 5
                     :all-tests true}
            project-key (cache/project-key dir ['b-test 'a-test] options)]
        (is (= {:test-nss ["a-test" "b-test"]
                :runner (cache/runner-version)
                :options {:all-tests true
                          :timeout 5}}
               (dissoc project-key :sources)))
        (is (= (cache/sources-hash dir cache/source-roots) (:sources project-key))
            "no classpath directory lies in temporary directory")
        (write-file dir "src/a.clj" "y")
        (is (not= project-key (cache/project-key dir ['b-test 'a-test] options)))))))

(deftest sources-hash-names-root-it-could-not-read
  (fixtures/with-temp-dir
    (fn [dir]
      (let [unreadable (io/file dir "src" "a.clj")]
        (write-file dir "src/a.clj" "x")
        (.setReadable unreadable false)
        (try
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                (re-pattern (str "^reading the sources under " dir " failed: "))
                                (cache/sources-hash dir cache/source-roots)))
          (finally (.setReadable unreadable true)))))))

(deftest runner-files-are-runner-namespaces-without-page
  (let [names (map #(.getName ^java.io.File %) (cache/runner-files))]
    (is (= "mutate.clj" (first names)))
    (is (= (sort (rest names)) (rest names))
        "namespaces in name order")
    (is (every? (set (rest names)) ["cache.clj" "forms.clj" "run.clj"]))
    (is (not-any? #{"mutate_html.clj" "mutate_report.css"} names)
        "page changes no outcome")
    (is (re-matches #"[0-9a-f]{64}" (cache/runner-version)))))

(deftest runner-version-names-runner-files-it-could-not-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"^reading the runner files failed: "
                        (cache/runner-version (constantly [(io/file "no-such-runner-file.clj")])))))

(defn- run-key [sources] (cache/run-key (assoc key-input :sources sources)))

(def ^:private old-results
  {"f/1:1/when->when-not" {:outcome :failed
                           :killed-by "a-test/t"
                           :place "p1"}
   "f/1:2/if->if-not" {:outcome :passed
                       :place "p2"}
   "f/1:3/and->or" {:outcome :no-coverage
                    :place "p3"}
   "f/1:4/=->not=" {:outcome :unviable
                    :place "p4"}
   "f/1:5/<-><=" {:outcome :timeout
                  :place "p5"}
   "f/1:6/>->>=" {:outcome :timeout
                  :stuck? true
                  :place "p6"}})

(defn- mutant
  ([id] {:id id})
  ([id place] {:id id
               :place place}))

(defn- plan-message [store ids] (:message (cache/plan-run store (map mutant ids))))

(def ^:private trust-warning
  (str " --iterate trusts the caught outcomes, though the code changed; a run without it checks them, so run one"
       " before a merge."))

(defn- write-cache [file entry]
  (io/make-parents file)
  (spit file (pr-str entry)))

(defn- read-cache [file] (edn/read-string (slurp file)))

(def ^:private old-last-run {"p1" :killed
                             "p2" :survived})

(defn- with-old-cache
  "Call `f` with a cache file that holds `old-results` under sources `s1`."
  [f]
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (cache/cache-file (io/file dir "cache") 'a)]
        (write-cache file {:key (run-key "s1")
                           :results old-results
                           :plans {"f" {:kind :every}}
                           :notes ["old note"]
                           :last-run old-last-run})
        (f file)))))

(defn- open [file options] (cache/open! (assoc options :file file)))

(defn- key-of [sources] (constantly (run-key sources)))

(deftest open-keeps-every-outcome-for-same-key
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s1")})]
        (is (= :resume (:mode store)))
        (is (= old-results (:results @(:entry store))))
        (is (= {"f" {:kind :every}} (cache/cached-plans store)))
        (is (= "old note" (cache/run-note store nil)))
        (is (= old-last-run (:previous store))
            "progress compares with outcomes last run reported")))))

(deftest open-reuses-nothing-when-sources-changed
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s2")})]
        (is (= :start-over (:mode store)))
        (is (= {} (:results @(:entry store))))
        (is (= {} (cache/cached-plans store)))
        (is (nil? (cache/run-note store nil)))
        (is (= old-last-run (:previous store))
            "progress kept across edits")))))

(def ^:private current-places
  {"p1" "f/9:1/when->when-not"
   "p2" "f/9:2/if->if-not"
   "p3" "f/9:3/and->or"
   "p5" "f/9:5/<-><="
   "p6" "f/9:6/>->>="})

(deftest open-with-iterate-keeps-caught-and-unviable-outcomes-by-place
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s2")
                              :current-places (constantly current-places)
                              :iterate true})]
        (is (= :iterate (:mode store)))
        (is (= {"f/9:1/when->when-not" {:outcome :failed
                                        :killed-by "a-test/t"
                                        :place "p1"
                                        :trusted? true}
                "f/9:6/>->>=" {:outcome :timeout
                               :stuck? true
                               :place "p6"}}
               (:results @(:entry store)))
            "caught and loop kept under new id, unviable gone from target dropped, gaps and timeout run again")
        (is (= {} (cache/cached-plans store))
            "plans of other sources dropped")
        (is (nil? (cache/run-note store nil)))))))

(deftest open-with-iterate-keeps-unviable-outcome-whose-place-stays
  (with-old-cache
    (fn [file]
      (is (= {:outcome :unviable
              :place "p4"
              :trusted? true}
             (get (:results @(:entry (open file {:make-key (key-of "s2")
                                                 :current-places (constantly {"p4" "f/9:4/=->not="})
                                                 :iterate true})))
                  "f/9:4/=->not="))))))

(deftest plan-run-reuses-trusted-outcomes-only-with-iterate
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (io/file dir "a.edn")
            ids ["f/1:1/when->when-not" "f/1:2/if->if-not"]]
        (write-cache file {:key (run-key "s1")
                           :results {"f/1:1/when->when-not" {:outcome :failed
                                                             :trusted? true}
                                     "f/1:2/if->if-not" {:outcome :failed}}})
        (is (= ["f/1:2/if->if-not"]
               (-> (open file {:make-key (key-of "s1")}) (cache/plan-run (map mutant ids)) :reused keys))
            "plain run checks trusted outcome")
        (is (= ids
               (-> (open file {:make-key (key-of "s1")
                               :iterate true})
                   (cache/plan-run (map mutant ids))
                   :reused
                   keys
                   sort))
            "iterate reuses it")
        (is (str/ends-with? (plan-message (open file {:make-key (key-of "s1")
                                                      :iterate true})
                                          ids)
                            trust-warning)
            "one trusted outcome among others is enough to warn")))))

(deftest open-with-iterate-starts-over-when-more-than-sources-changed
  (with-old-cache
    (fn [file]
      (doseq [key-change [{:runner "r2"} {:timeout 100} {:test-nss ['b-test]}]]
        (is (= :start-over (:mode (open file {:make-key (constantly (cache/run-key (merge key-input
                                                                                          {:sources "s2"}
                                                                                          key-change)))
                                              :current-places (constantly current-places)
                                              :iterate true})))
            (str key-change))))))

(deftest open-with-fresh-keeps-nothing-but-progress
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s1")
                              :fresh true})]
        (is (= :fresh (:mode store)))
        (is (= {} (:results @(:entry store))))
        (is (= old-last-run (:previous store)))))))

(deftest open-warns-and-starts-over-on-unreadable-cache
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (io/file dir "a.edn")]
        (doseq [[text error] [["{:key" "EOF while reading"] ["[]" "it holds no cache"]
                              ["{:key {} :results []}" "it holds no cache"]]]
          (spit file text)
          (let [store (atom nil)
                output (with-out-str (reset! store (open file {:make-key (key-of "s1")})))]
            (is (str/starts-with? output (str "Warning: the cache " file " could not be read, so the run does not use"
                                              " it; its first outcome replaces it: "))
                text)
            (is (str/includes? output error)
                text)
            (is (= :start-over (:mode @store))
                text)
            (is (nil? (:old-key @store))
                text)))))))

(deftest open-without-cache-file-says-nothing
  (fixtures/with-temp-dir
    (fn [dir]
      (is (= "" (with-out-str (open (io/file dir "a.edn") {:make-key (key-of "s1")})))))))

(deftest open-without-file-reads-and-writes-nothing
  (let [store (cache/open! {:make-key #(throw (ex-info "never called" {}))})]
    (is (= :start-over (:mode store)))
    (is (nil? (:previous store)))
    (cache/record! store {:id "f/1:1/when->when-not"
                          :outcome :failed}
                   {}
                   nil)
    (cache/finish! store {"p" :killed} {})
    (is (= {"f/1:1/when->when-not" {:outcome :failed
                                    :place nil}}
           (:results @(:entry store)))
        "outcome kept in memory only")))

(deftest open-goes-on-without-cache-when-key-fails
  (with-old-cache
    (fn [file]
      (let [before (slurp file)
            output (with-out-str
                     (let [store (open file {:make-key #(throw (ex-info "reading the runner files failed: gone" {}))})]
                       (is (nil? @(:file store)))
                       (is (nil? (:previous store)))
                       (cache/record! store (mutant "f/1") {} nil)
                       (cache/finish! store {} {})))]
        (is (= (str "Warning: the cache key could not be made, so the run does not use the cache: reading the"
                    " runner files failed: gone\n")
               output))
        (is (= before (slurp file)))))))

(deftest plan-run-splits-reused-from-mutants-to-run
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s1")})
            {:keys [reused to-run]} (cache/plan-run store (map mutant ["f/1:1/when->when-not" "f/9:9/new->old"]))]
        (is (= ["f/1:1/when->when-not"] (keys reused)))
        (is (= [(mutant "f/9:9/new->old")] to-run))
        (is (= "Reused the last run's outcome for 1 of 2 mutants; running the rest."
               (plan-message store ["f/1:1/when->when-not" "f/9:9/new->old"])))
        (is (= "All 3 mutants come from the last run, so no test runs."
               (plan-message store ["f/1:1/when->when-not" "f/1:2/if->if-not" "f/1:5/<-><="]))
            "plain timeout reused")
        (is (nil? (plan-message store ["f/9:9/new->old"]))
            "nothing reused, nothing to say")))))

(deftest plan-run-runs-past-loop-again-after-every-other-mutant
  (with-old-cache
    (fn [file]
      (let [{:keys [reused to-run]} (cache/plan-run (open file {:make-key (key-of "s1")})
                                                    (map mutant ["f/1:1/when->when-not" "f/1:6/>->>="
                                                                 "f/9:9/new->old" "f/9:10/when->when-not"]))]
        (is (= ["f/1:1/when->when-not"] (keys reused))
            "loop never reused")
        (is (= ["f/9:9/new->old" "f/9:10/when->when-not" "f/1:6/>->>="] (map :id to-run)))))))

(deftest plan-run-says-why-it-reuses-nothing
  (with-old-cache
    (fn [file]
      (is (= "Running every mutant, since the sources changed since the last run; --iterate keeps its caught mutants."
             (plan-message (open file {:make-key (key-of "s2")}) ["f/1:1/when->when-not"])))))
  (with-old-cache
    (fn [file]
      (is (= (str "Running every mutant, since the test namespaces and the runner and the options (-a or --timeout)"
                  " changed since the last run.")
             (plan-message (open file {:make-key (constantly (cache/run-key (assoc key-input
                                                                                   :test-nss ['b-test]
                                                                                   :runner "r2"
                                                                                   :all-tests true)))})
                           ["f/1:1/when->when-not"]))
          "hint for --iterate only when only sources changed")))
  (with-old-cache
    (fn [file]
      (is (= "Running every mutant again, as --fresh asks."
             (plan-message (open file {:make-key (key-of "s1")
                                       :fresh true})
                           ["f/1:1/when->when-not"])))))
  (is (nil? (plan-message (cache/open! {}) ["f/1:1/when->when-not"]))
      "first run says nothing"))

(deftest plan-run-with-iterate-counts-and-warns-of-trusted-outcomes
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s2")
                              :current-places (constantly (assoc current-places "p4" "f/9:4/=->not="))
                              :iterate true})]
        (is (= (str "Reused the last run's outcome for 2 of 4 mutants (1 caught, 1 did not compile), as --iterate"
                    " asks; running the rest." trust-warning)
               (plan-message store ["f/9:1/when->when-not" "f/9:4/=->not=" "f/9:6/>->>=" "f/9:2/if->if-not"])))
        (is (= (str "The 1 mutant comes from the last run, as --iterate asks; the tests run once to find which tests"
                    " reach each definition." trust-warning)
               (plan-message store ["f/9:1/when->when-not"]))
            "all from cache still warns")))))

(deftest plan-run-without-trusted-outcome-gives-no-warning
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (io/file dir "a.edn")]
        (write-cache file {:key (run-key "s1")
                           :results {"f/1" {:outcome :unviable}}})
        (is (= "The 1 mutant comes from the last run, so no test runs."
               (plan-message (open file {:make-key (key-of "s1")
                                         :iterate true})
                             ["f/1"]))
            "same key resumes, also with --iterate")))))

(deftest stop-note-matches-why-run-stopped
  (is (str/includes? (cache/stop-note :stuck true) "the next run runs the rest, and this one last"))
  (is (= " The cache keeps the mutants that finished, but the next run stops here again until these tests stop looping."
         (cache/stop-note :plan-stuck true)))
  (is (= " The next run stops here again until these tests stop looping." (cache/stop-note :plan-stuck false))
      "nothing recorded, nothing kept")
  (is (str/includes? (cache/stop-note :restore-error true) "this one too, so the next run runs the rest"))
  (is (nil? (cache/stop-note nil true))))

(defn- outcome [id place & {:as more}]
  (merge {:id id
          :place place
          :outcome :failed}
         more))

(deftest record-writes-cache-after-each-outcome
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s2")})]
        (cache/record! store
                       {:id "f/1:1/when->when-not"
                        :place "p1"
                        :outcome :failed
                        :killed-by #'clojure.core/inc
                        :at-load? false
                        :stuck? false}
                       {"f" {:kind :subset
                             :tests #{#'clojure.core/inc}
                             :timeout-ms 10}}
                       "a note")
        (is (= {:key (run-key "s2")
                :results {"f/1:1/when->when-not" {:outcome :failed
                                                  :place "p1"
                                                  :killed-by "clojure.core/inc"}}
                :plans {"f" {:kind :subset
                             :tests ["clojure.core/inc"]}}
                :notes ["a note"]
                :last-run old-last-run}
               (read-cache file))
            "test vars as names, false flags left out, last run kept until run ends")
        (cache/record! store (outcome "g/1" "pg" :outcome :timeout :at-load? true :stuck? true) {} "a note")
        (cache/record! store (outcome "h/1" "ph") {} "other note")
        (cache/record! store (outcome "h/2" "ph2") {} nil)
        (is (= {"g/1" {:outcome :timeout
                       :place "pg"
                       :at-load? true
                       :stuck? true}
                "h/1" {:outcome :failed
                       :place "ph"}}
               (-> file read-cache :results (select-keys ["g/1" "h/1"]))))
        (is (= ["a note" "other note"] (:notes (read-cache file)))
            "note kept once")
        (is (= 4 (-> file read-cache :results count)))
        (cache/record! store (outcome "h/3" "ph3") {"f" {:kind :every}} nil)
        (is (= {:kind :every} (get-in (read-cache file) [:plans "f"]))
            "changed plan written")))))

(deftest finish-records-what-run-reported-as-last-run
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s2")})]
        (cache/record! store (outcome "f/1" "p1") {} nil)
        (cache/finish! store
                       {"p1" :killed
                        "p9" :survived}
                       {"g" {:kind :subset
                             :tests #{#'clojure.core/inc}}})
        (is (= {"p1" :killed
                "p9" :survived}
               (:last-run (read-cache file))))
        (is (= {:kind :subset
                :tests ["clojure.core/inc"]}
               (get-in (read-cache file) [:plans "g"]))
            "plans report used are saved for run with no baseline")))))

(deftest finish-of-resumed-run-records-last-run-without-new-outcome
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s1")})]
        (cache/finish! store {"p1" :killed} {})
        (is (= {"p1" :killed} (:last-run (read-cache file)))
            "-d run that reuses all sets last run to what it reported")
        (is (= old-results (:results (read-cache file))))))))

(deftest finish-of-run-that-reported-nothing-leaves-cache-alone
  (with-old-cache
    (fn [file]
      (let [before (slurp file)]
        (doseq [options [{:make-key (key-of "s1")}
                         {:make-key (key-of "s2")}
                         {:make-key (key-of "s1")
                          :fresh true}]]
          (cache/finish! (open file options) {} {"g" {:kind :every}})
          (is (= before (slurp file))
              (str options)))))))

(deftest finish-of-iterate-run-that-reused-all-saves-key-and-trusted-outcomes
  (with-old-cache
    (fn [file]
      (cache/finish! (open file {:make-key (key-of "s2")
                                 :current-places (constantly {"p1" "f/9:1/when->when-not"})
                                 :iterate true})
                     {"p1" :killed}
                     {})
      (let [{:keys [key results last-run]} (read-cache file)]
        (is (= (run-key "s2") key))
        (is (= {"f/9:1/when->when-not" {:outcome :failed
                                        :killed-by "a-test/t"
                                        :place "p1"
                                        :trusted? true}}
               results))
        (is (= {"p1" :killed} last-run))))))

(deftest open-looks-for-current-places-only-under-iterate
  (with-old-cache
    (fn [file]
      (doseq [options [{:make-key (key-of "s1")
                        :iterate true}
                       {:make-key (key-of "s2")}]]
        (is (some? (open file (assoc options :current-places #(throw (ex-info "called" {})))))
            (str options))))))

(deftest cache-that-cannot-be-written-warns-once-and-is-dropped
  (fixtures/with-temp-dir
    (fn [dir]
      (spit (io/file dir "cache") "a file, not a directory")
      (let [file (cache/cache-file (io/file dir "cache") 'a)
            store (open file {:make-key (key-of "s1")})
            output (with-out-str
                     (cache/record! store (outcome "f/1" "p1") {} nil)
                     (cache/record! store (outcome "f/2" "p2") {} nil)
                     (cache/finish! store {} {}))]
        (is (str/starts-with? output (str "Warning: the cache " file " could not be written, so the run goes on"
                                          " without it:")))
        (is (= 1 (count (re-seq #"Warning" output))))
        (is (nil? @(:file store)))
        (is (= 2 (count (:results @(:entry store))))
            "run keeps going")))))

(deftest run-note-joins-distinct-notes
  (with-old-cache
    (fn [file]
      (let [store (open file {:make-key (key-of "s1")})]
        (is (= "old note new" (cache/run-note store "new")))
        (is (= "old note" (cache/run-note store "old note")))))))

(defn- summary-of [& mutants] {:mutants (vec mutants)})

(deftest with-progress-matches-last-run-by-place
  (let [{:keys [mutants progress]} (cache/with-progress
                                     (summary-of {:id "a"
                                                  :outcome :killed}
                                                 {:id "b"
                                                  :outcome :survived}
                                                 {:id "c"
                                                  :outcome :no-coverage}
                                                 {:id "d"
                                                  :outcome :timeout}
                                                 {:id "e"
                                                  :outcome :killed}
                                                 {:id "f"
                                                  :outcome :survived})
                                     {:previous {"pa" :survived
                                                 "pb" :no-coverage
                                                 "pc" :killed
                                                 "pd" :survived
                                                 "pe" :timeout
                                                 "gone" :passed}}
                                     {"a" "pa"
                                      "b" "pb"
                                      "c" "pc"
                                      "d" "pd"
                                      "e" "pe"
                                      "f" "pf"})]
    (is (= [nil :still-open :new nil nil nil] (map :since-last-run mutants))
        "gap last run did not have gets no tag")
    (is (= {:closed ["a"]
            :new 1
            :still-open 1
            :not-run-before 1}
           progress)
        "gap now timed out, caught that was no gap, or out of this run is not closed")))

(deftest with-progress-leaves-first-run-alone
  (is (= (summary-of {:id "b"
                      :outcome :survived})
         (cache/with-progress (summary-of {:id "b"
                                           :outcome :survived})
           {:previous nil}
           {"b" "pb"}))))

(deftest cache-file-is-named-after-target
  (is (str/ends-with? (str (cache/cache-file ".mutate-cache" 'app.core)) ".mutate-cache/app.core.edn")))

(deftest record-marks-run-as-having-recorded-outcome
  (let [store (cache/open! {})]
    (is (false? @(:recorded? store)))
    (cache/record! store (outcome "f/1" "p1") {} nil)
    (is (true? @(:recorded? store)))))
