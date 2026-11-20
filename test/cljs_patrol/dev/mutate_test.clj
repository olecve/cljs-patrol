(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [clojure.java.io]
   [clojure.java.shell]
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
                                   :unreached [{:def "e"}]})]
    (is (= [{:def "a"
             :mutants 2
             :killed 1
             :survived 1
             :unviable 0
             :timeout 0}
            {:def "b"
             :mutants 2
             :killed 0
             :survived 0
             :unviable 1
             :timeout 1}]
           (:definitions summary)))
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
       "(defn label [x] (if (positive? x) :up :down))\n"))

(def ^:private target-tests
  (str "(ns cljs-patrol.dev.mutate-test.target-test (:require [clojure.test :refer [deftest is]]"
       " [cljs-patrol.dev.mutate-test.target :as target]))\n"
       "(deftest labels (is (= :up (target/label 1))))\n"))

(defn- with-target [tests f]
  (sample-ns 'cljs-patrol.dev.mutate-test.target)
  (sample-ns 'cljs-patrol.dev.mutate-test.target-test)
  (try
    (mutate/load-text 'cljs-patrol.dev.mutate-test.target target-source)
    (mutate/load-text 'cljs-patrol.dev.mutate-test.target-test tests)
    (f)
    (finally
      (remove-ns 'cljs-patrol.dev.mutate-test.target)
      (remove-ns 'cljs-patrol.dev.mutate-test.target-test))))

(defn- run-target [options]
  (binding [*out* (java.io.StringWriter.)]
    (mutate/mutate-source 'cljs-patrol.dev.mutate-test.target "target.clj" target-source
                          ['cljs-patrol.dev.mutate-test.target-test] options)))

(deftest run-kills-and-keeps-mutants-and-restores-the-namespace
  (with-target target-tests
    (fn []
      (let [{:keys [totals]} (run-target {})]
        (is (= {:mutants 2
                :killed 1
                :survived 1
                :unviable 0
                :timeout 0}
               totals)
            "boundary at 0 is never tested, so > -> >= survives and if -> if-not is killed"))
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
