(ns cljs-patrol.dev.mutate.cache-test
  (:require
   [cljs-patrol.dev.mutate.cache :as cache]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
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
      "number at same place as body mutant")
  (is (= "f/3:1/true->false"
         (:id (cache/with-id {:def "f"
                              :row 3
                              :col 1
                              :from "true"
                              :to false})))))

(def ^:private key-input
  {:sources "s1"
   :runner "r1"
   :timeout nil
   :all-tests nil})

(deftest run-key-changes-with-each-input
  (let [base (cache/run-key key-input)]
    (is (= base (cache/run-key key-input)))
    (doseq [[input-key value] [[:sources "s2"]
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

(deftest sources-hash-covers-every-file-that-tests-may-read
  (fixtures/with-temp-dir
    (fn [dir]
      (doseq [path ["deps.edn" "src/a.clj" "test/a_test.clj" "dev/d.clj" "test-projects/p/x.cljs" "docs/rules.md"]]
        (write-file dir path "x"))
      (let [base (cache/sources-hash dir)]
        (is (re-matches #"[0-9a-f]{64}" base))
        (is (= base (cache/sources-hash dir)))
        (write-file dir "README.md" "x")
        (write-file dir ".mutate-cache/a.edn" "x")
        (is (= base (cache/sources-hash dir))
            "files outside source roots left out")
        (doseq [path ["deps.edn" "src/a.clj" "test/a_test.clj" "dev/d.clj" "test-projects/p/x.cljs" "docs/rules.md"]]
          (write-file dir path "y")
          (is (not= base (cache/sources-hash dir))
              path)
          (write-file dir path "x"))
        (write-file dir "src/b.clj" "")
        (is (not= base (cache/sources-hash dir))
            "new empty file counts by its path")))))

(deftest sources-hash-tells-file-name-from-its-text
  (fixtures/with-temp-dir
    (fn [dir]
      (write-file dir "src/ab" "c")
      (let [one (cache/sources-hash dir)]
        (.delete (io/file dir "src/ab"))
        (write-file dir "src/a" "bc")
        (is (not= one (cache/sources-hash dir)))))))

(deftest runner-files-are-runner-namespaces-without-page
  (let [names (map #(.getName ^java.io.File %) (cache/runner-files))]
    (is (= "mutate.clj" (first names)))
    (is (= (sort (rest names)) (rest names))
        "namespaces in name order")
    (is (every? (set (rest names)) ["cache.clj" "forms.clj" "run.clj"]))
    (is (not-any? #{"mutate_html.clj" "mutate_report.css"} names)
        "page changes no outcome")
    (is (re-matches #"[0-9a-f]{64}" (cache/runner-version)))))

(defn- run-key [sources] (cache/run-key (assoc key-input :sources sources)))

(def ^:private old-results
  {"f/1:1/when->when-not" {:outcome :failed
                           :killed-by "a-test/t"}
   "f/1:2/if->if-not" {:outcome :passed}
   "f/1:3/and->or" {:outcome :no-coverage}
   "f/1:4/=->not=" {:outcome :unviable}
   "f/1:5/<-><=" {:outcome :timeout}
   "f/1:6/>->>=" {:outcome :timeout
                  :stuck? true}})

(def ^:private old-ids (set (keys old-results)))

(defn- mutant [id] {:id id})

(defn- write-cache [file entry]
  (io/make-parents file)
  (spit file (pr-str entry)))

(defn- read-cache [file] (edn/read-string (slurp file)))

(defn- with-old-cache
  "Call `f` with a cache file that holds `old-results` under sources `s1` and target `t1`."
  [f]
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (cache/cache-file (io/file dir "cache") 'a)]
        (write-cache file {:key (run-key "s1")
                           :target "t1"
                           :results old-results
                           :plans {"f" {:kind :every}}
                           :note "old note"})
        (f file)))))

(defn- open [file options]
  (cache/open! (merge {:file file
                       :target "t1"
                       :current-ids old-ids}
                      options)))

(deftest open-keeps-every-outcome-for-same-key
  (with-old-cache
    (fn [file]
      (let [before (slurp file)
            store (open file {:key (run-key "s1")})]
        (is (= :resume (:mode store)))
        (is (= old-results (:results @(:entry store))))
        (is (= {"f" {:kind :every}} (cache/cached-plans store)))
        (is (= "old note" (cache/cached-note store)))
        (is (= #{"f/1:2/if->if-not" "f/1:3/and->or"} (:previous-gaps store))
            "survived and no coverage are last run's gaps")
        (is (= before (slurp file))
            "resumed cache is not written at start")))))

(deftest open-starts-over-when-sources-changed
  (with-old-cache
    (fn [file]
      (let [store (open file {:key (run-key "s2")})]
        (is (= :start-over (:mode store)))
        (is (= {} (:results @(:entry store))))
        (is (= {} (cache/cached-plans store)))
        (is (nil? (cache/cached-note store)))
        (is (= {:key (run-key "s2")
                :target "t1"
                :results {}
                :plans {}
                :note nil}
               (read-cache file))
            "stale cache replaced at once")
        (is (= 2 (count (:previous-gaps store)))
            "same target keeps progress")))))

(deftest open-compares-progress-only-with-same-target
  (with-old-cache
    (fn [file]
      (is (nil? (:previous-gaps (open file {:key (run-key "s1")
                                            :target "t2"})))
          "mutant ids name places in old target text"))))

(deftest open-with-iterate-keeps-caught-and-unviable-outcomes-of-current-mutants
  (with-old-cache
    (fn [file]
      (let [store (open file {:key (run-key "s2")
                              :target "t2"
                              :current-ids (disj old-ids "f/1:4/=->not=")
                              :iterate true})]
        (is (= :iterate (:mode store)))
        (is (= ["f/1:1/when->when-not" "f/1:6/>->>="]
               (-> @(:entry store) :results keys sort))
            "caught and loop kept, unviable mutant gone from target dropped, gaps and timeout run again")
        (is (= {"f/1:1/when->when-not" {:outcome :failed
                                        :killed-by "a-test/t"
                                        :trusted? true}
                "f/1:6/>->>=" {:outcome :timeout
                               :stuck? true}}
               (:results (read-cache file)))
            "kept outcome marked trusted, loop not")
        (is (= {"f" {:kind :every}} (cache/cached-plans store)))))))

(deftest plan-run-reuses-trusted-outcomes-only-with-iterate
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (io/file dir "a.edn")
            ids ["f/1:1/when->when-not" "f/1:2/if->if-not"]]
        (write-cache file {:key (run-key "s1")
                           :target "t1"
                           :results {"f/1:1/when->when-not" {:outcome :failed
                                                             :trusted? true}
                                     "f/1:2/if->if-not" {:outcome :failed}}})
        (is (= ["f/1:2/if->if-not"]
               (-> (open file {:key (run-key "s1")}) (cache/plan-run (map mutant ids)) :reused keys))
            "plain run checks trusted outcome")
        (is (= ids
               (-> (open file {:key (run-key "s1")
                               :iterate true})
                   (cache/plan-run (map mutant ids))
                   :reused
                   keys
                   sort))
            "iterate reuses it")))))

(deftest open-with-iterate-starts-over-when-runner-or-options-changed
  (with-old-cache
    (fn [file]
      (doseq [key-change [{:runner "r2"} {:timeout 100}]]
        (is (= :start-over (:mode (open file {:key (cache/run-key (merge key-input {:sources "s2"} key-change))
                                              :iterate true})))
            (str key-change))))))

(deftest open-with-fresh-keeps-nothing-but-progress
  (with-old-cache
    (fn [file]
      (let [store (open file {:key (run-key "s1")
                              :fresh true})]
        (is (= :fresh (:mode store)))
        (is (= {} (:results (read-cache file))))
        (is (= 2 (count (:previous-gaps store))))))))

(deftest open-starts-over-on-unreadable-cache
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (io/file dir "a.edn")]
        (doseq [text ["{:key" "[]" "{:key {} :results []}"]]
          (spit file text)
          (let [store (open file {:key (run-key "s1")})]
            (is (= :start-over (:mode store))
                text)
            (is (nil? (:old-key store))
                text)
            (is (= {} (:results (read-cache file)))
                text)))))))

(deftest open-without-file-reads-and-writes-nothing
  (let [store (cache/open! {:key nil})]
    (is (= :start-over (:mode store)))
    (is (nil? (:previous-gaps store)))
    (cache/record! store {:id "f/1:1/when->when-not"
                          :outcome :failed}
                   {}
                   nil)
    (is (= {"f/1:1/when->when-not" {:outcome :failed}} (:results @(:entry store)))
        "outcome kept in memory only")))

(defn- plan-message [store ids] (:message (cache/plan-run store (map mutant ids))))

(deftest plan-run-splits-reused-from-mutants-to-run
  (with-old-cache
    (fn [file]
      (let [store (open file {:key (run-key "s1")})
            {:keys [reused to-run]} (cache/plan-run store (map mutant ["f/1:1/when->when-not" "f/9:9/new->old"]))]
        (is (= {"f/1:1/when->when-not" {:outcome :failed
                                        :killed-by "a-test/t"}}
               reused))
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
      (let [{:keys [reused to-run]} (cache/plan-run (open file {:key (run-key "s1")})
                                                    (map mutant ["f/1:1/when->when-not" "f/1:6/>->>="
                                                                 "f/9:9/new->old" "f/9:10/when->when-not"]))]
        (is (= ["f/1:1/when->when-not"] (keys reused))
            "loop never reused")
        (is (= ["f/9:9/new->old" "f/9:10/when->when-not" "f/1:6/>->>="] (map :id to-run)))))))

(deftest plan-run-says-why-it-reuses-nothing
  (with-old-cache
    (fn [file]
      (is (= "Running every mutant, since the sources changed since the last run; --iterate keeps its caught mutants."
             (plan-message (open file {:key (run-key "s2")}) ["f/1:1/when->when-not"])))))
  (with-old-cache
    (fn [file]
      (is (= (str "Running every mutant, since the runner and the options (-a or --timeout) changed since the last"
                  " run.")
             (plan-message (open file {:key (cache/run-key (assoc key-input
                                                                  :runner "r2"
                                                                  :all-tests true))})
                           ["f/1:1/when->when-not"]))
          "hint for --iterate only when only sources changed")))
  (with-old-cache
    (fn [file]
      (is (= "Running every mutant again, as --fresh asks."
             (plan-message (open file {:key (run-key "s1")
                                       :fresh true})
                           ["f/1:1/when->when-not"])))))
  (is (nil? (plan-message (cache/open! {:key nil}) ["f/1:1/when->when-not"]))
      "first run says nothing"))

(deftest plan-run-with-iterate-counts-caught-and-unviable-outcomes
  (with-old-cache
    (fn [file]
      (is (= (str "Reused the last run's outcome for 2 of 4 mutants (1 caught, 1 did not compile), as --iterate"
                  " asks; running the rest. --iterate trusts these outcomes, though the code changed, so run"
                  " without it before a merge.")
             (plan-message (open file {:key (run-key "s2")
                                       :iterate true})
                           ["f/1:1/when->when-not" "f/1:4/=->not=" "f/1:6/>->>=" "f/1:2/if->if-not"]))))))

(deftest record-writes-each-outcome-as-it-comes
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (cache/cache-file (io/file dir "nested" "cache") 'a)
            store (cache/open! {:file file
                                :key (run-key "s1")})]
        (cache/record! store
                       {:id "f/1:1/when->when-not"
                        :outcome :failed
                        :killed-by #'clojure.core/inc
                        :at-load? false
                        :stuck? false}
                       {"f" {:kind :subset
                             :tests #{#'clojure.core/inc}
                             :timeout-ms 10}}
                       nil)
        (is (= {:key (run-key "s1")
                :target nil
                :results {"f/1:1/when->when-not" {:outcome :failed
                                                  :killed-by "clojure.core/inc"}}
                :plans {"f" {:kind :subset
                             :tests ["clojure.core/inc"]}}
                :note nil}
               (read-cache file))
            "test vars kept as names, false flags left out")
        (cache/record! store
                       {:id "g/2:1/body->nil"
                        :outcome :timeout
                        :at-load? true
                        :stuck? true}
                       {"g" {:kind :every}}
                       "a note")
        (is (= {:results {"f/1:1/when->when-not" {:outcome :failed
                                                  :killed-by "clojure.core/inc"}
                          "g/2:1/body->nil" {:outcome :timeout
                                             :at-load? true
                                             :stuck? true}}
                :plans {"f" {:kind :subset
                             :tests ["clojure.core/inc"]}
                        "g" {:kind :every}}
                :note "a note"}
               (dissoc (read-cache file) :key :target))
            "second outcome added to first")))))

(defn- summary-of [& mutants] {:mutants (vec mutants)})

(deftest with-progress-tags-gaps-and-lists-closed-ones
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
                                                  :outcome :killed})
                                     {:previous-gaps #{"a" "b" "d" "gone"}})]
    (is (= [nil :still-open :new nil nil] (map :since-last-run mutants)))
    (is (= {:closed ["a"]
            :new 1
            :still-open 1}
           progress)
        "gap now timed out or out of this run is not closed")))

(deftest with-progress-leaves-first-run-alone
  (is (= (summary-of {:id "b"
                      :outcome :survived})
         (cache/with-progress (summary-of {:id "b"
                                           :outcome :survived})
           {:previous-gaps nil})))
  (is (= [:new]
         (map :since-last-run (:mutants (cache/with-progress (summary-of {:id "b"
                                                                          :outcome :survived})
                                          {:previous-gaps #{}}))))
      "last run with no gaps makes every gap new"))

(deftest cache-file-is-named-after-target
  (is (str/ends-with? (str (cache/cache-file ".mutate-cache" 'app.core)) ".mutate-cache/app.core.edn")))
