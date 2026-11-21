(ns cljs-patrol.dev.mutate.report-test
  (:require
   [cljs-patrol.dev.mutate.report :as report]
   [clojure.string :as str]
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

(defn- survivor [row col from to]
  {:def "f"
   :row row
   :col col
   :from from
   :to to
   :outcome :survived})

(defn- printed-report [mutants source-lines]
  (with-out-str (report/print-report {:mutants mutants
                                      :totals {:mutants (count mutants)
                                               :killed 0
                                               :survived (count mutants)
                                               :no-coverage 0
                                               :unviable 0
                                               :timeout 0}}
                                     source-lines
                                     nil
                                     "x.target")))

(defn- swap-lines-printed [line from to]
  (let [col (inc (str/index-of line from))]
    (->> (printed-report [(survivor 1 col from to)] [line])
         str/split-lines
         (drop-while #(not= "Survived:" %))
         (drop 2)
         (take 2)
         vec)))

(deftest survivor-shows-its-line-before-and-after-swap
  (is (str/includes? (printed-report [(survivor 1 4 "when" "when-not")] ["  (when (seq xs) :ok)"])
                     (str "Survived:\n"
                          "  1:4  when -> when-not  in f\n"
                          "    - (when (seq xs) :ok)\n"
                          "    + (when-not (seq xs) :ok)\n"))
      "line loses its indent and shows once before and once after swap"))

(deftest survivor-at-line-end-shows-its-swap
  (is (= ["    - true"
          "    + false"]
         (swap-lines-printed "  true" "true" "false"))))

(deftest survivor-not-found-at-its-place-shows-line-once
  (is (str/includes? (printed-report [(survivor 1 2 "when" "when-not")] ["  (if x 1 2)"])
                     "  1:2  when -> when-not  in f\n      (if x 1 2)\n")
      "other text at column")
  (is (str/includes? (printed-report [(survivor 1 9 "when" "when-not")] ["(if x)"])
                     "  1:9  when -> when-not  in f\n      (if x)\n")
      "column past line end"))

(deftest line-of-line-width-stays-whole
  (let [as (str/join (repeat 50 "a"))
        bs (str/join (repeat 36 "b"))]
    (is (= [(str "    - " as " (when x) " bs)
            (str "    + " as " (when-not x) " bs)]
           (swap-lines-printed (str as " (when x) " bs) "when" "when-not"))
        "100 characters after swap fit, even with swap past 40 characters")))

(deftest line-past-line-width-is-cut-at-its-end
  (let [line (str "(when x " (str/join (repeat 92 "b")))]
    (is (= [(str "    - " line)
            (str "    + (when-not x " (str/join (repeat 88 "b")) " …")]
           (swap-lines-printed line "when" "when-not"))
        "swap near start keeps start, and only longer line gets end marker")))

(deftest long-line-is-cut-around-its-swap
  (let [line (str "    " (str/join (repeat 60 "a")) " (when x) " (str/join (repeat 60 "b")))]
    (is (= [(str "    - … " (str/join (repeat 38 "a")) " (when x) " (str/join (repeat 52 "b")) " …")
            (str "    + … " (str/join (repeat 38 "a")) " (when-not x) " (str/join (repeat 48 "b")) " …")]
           (swap-lines-printed line "when" "when-not"))
        "40 characters before swap, 100 in all")))

(deftest first-ten-survivors-show-their-line
  (let [lines (map #(str "(when x" % ")") (range 11))
        output (printed-report (map #(survivor % 2 "when" "when-not") (range 1 12)) lines)]
    (is (str/includes? output "  10:2  when -> when-not  in f\n    - (when x9)\n    + (when-not x9)\n"))
    (is (str/includes? output (str "  11:2  when -> when-not  in f\n"
                                   "  The first 10 survivors show their line; --html FILE shows the line of every one,"
                                   " and -r FILE writes them all as EDN.\n"))
        "eleventh survivor shows no line, and pointer follows")))

(deftest ten-survivors-need-no-pointer
  (is (not (str/includes? (printed-report (map #(survivor % 2 "when" "when-not") (range 1 11))
                                          (repeat 10 "(when x)"))
                          "The first 10"))))
