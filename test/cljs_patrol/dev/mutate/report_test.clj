(ns cljs-patrol.dev.mutate.report-test
  (:require
   [cljs-patrol.dev.mutate.report :as report]
   [clojure.test :refer [deftest is]]))

(deftest summary-counts-each-definition-and-names-the-rest
  (let [summary (report/summarize {:defs-in-scope ["a" "b" "c" "e"]
                                   :results [{:def "a"
                                              :outcome :failed}
                                             {:def "a"
                                              :outcome :passed}
                                             {:def "b"
                                              :outcome :unviable}
                                             {:def "b"
                                              :outcome :timeout}]
                                   :loaded-once ["d (defmulti)"]
                                   :unreached [{:def "e"}]
                                   :plans {"a" {:kind :subset
                                                :tests [#'clojure.core/inc]}
                                           "b" {:kind :every}}})]
    (is (= [{:def "a"
             :mutants 2
             :killed 1
             :survived 1
             :no-coverage 0
             :unviable 0
             :timeout 0
             :tests ["clojure.core/inc"]}
            {:def "b"
             :mutants 2
             :killed 0
             :survived 0
             :no-coverage 0
             :unviable 1
             :timeout 1
             :tests :every}]
           (:definitions summary)))
    (is (= {"a" :subset
            "b" :every}
           (:tests-run summary)))
    (is (not-any? #(contains? % :killed-by) (:mutants summary))
        "no killing test, no key")
    (is (= ["c"] (:not-mutated summary))
        "definition with no mutant is named, one never reached is not")
    (is (= [{:def "e"
             :mutants 1}]
           (:unreached summary)))
    (is (== 1/2 (rationalize (:score summary)))
        "unviable and timed-out mutants are not scored")))

(deftest exit-code-tells-survivors-from-a-stopped-run
  (is (= 0 (report/exit-code {:totals {:survived 0}})))
  (is (= 1 (report/exit-code {:totals {:survived 2}})))
  (is (= 2 (report/exit-code {:totals {:survived 2}
                              :stopped "looping"}))))

(deftest killed-mutant-shows-its-definition-is-reached
  (is (= [:killed :survived]
         (map :outcome (:mutants (report/summarize {:defs-in-scope ["a"]
                                                    :results [{:def "a"
                                                               :outcome :failed}
                                                              {:def "a"
                                                               :outcome :no-coverage}]}))))
      "passing mutant of a definition a test reached is a survivor, not uncovered"))

(deftest exit-code-counts-mutants-without-coverage
  (is (= 1 (report/exit-code {:totals {:survived 0
                                       :no-coverage 1}}))))

(deftest not-covered-definitions-and-covered-score
  (let [{:keys [not-covered score covered-score]}
        (report/summarize {:defs-in-scope ["a" "b"]
                           :results [{:def "a"
                                      :outcome :failed}
                                     {:def "a"
                                      :outcome :passed}
                                     {:def "b"
                                      :outcome :no-coverage}
                                     {:def "b"
                                      :outcome :unviable}]})]
    (is (= ["b"] not-covered)
        "definition with an uncovered mutant is named, whatever its other outcomes")
    (is (< (abs (- score 1/3)) 1e-9))
    (is (== 1/2 (rationalize covered-score))
        "covered score leaves uncovered mutants out")))

(deftest reached-rule-counts-test-failures-and-timeouts-only
  (let [outcomes #(map :outcome (:mutants (report/summarize {:defs-in-scope ["a"]
                                                             :results %})))]
    (is (= [:timeout :survived]
           (outcomes [{:def "a"
                       :outcome :timeout}
                      {:def "a"
                       :outcome :no-coverage}])))
    (is (= [:killed :no-coverage]
           (outcomes [{:def "a"
                       :outcome :failed
                       :at-load? true}
                      {:def "a"
                       :outcome :no-coverage}]))
        "mutant that threw while it loaded shows nothing about tests")))

(deftest summary-lists-ignored-mutants-apart-from-not-mutated
  (let [summary (report/summarize {:defs-in-scope ["a" "b"]
                                   :results [{:def "a"
                                              :outcome :failed}]
                                   :ignored [{:def "b"
                                              :row 3
                                              :col 4
                                              :from "if"
                                              :to 'if-not
                                              :skip-reason "wraps a JDK call"}]})]
    (is (= [{:def "b"
             :row 3
             :col 4
             :from "if"
             :to "if-not"
             :reason "wraps a JDK call"}]
           (:ignored summary)))
    (is (= [] (:not-mutated summary))
        "definition with only ignored mutants is no unmutated one")
    (is (= 1 (:mutants (:totals summary)))
        "ignored mutants are not counted")))
