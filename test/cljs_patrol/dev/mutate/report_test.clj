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

(defn- printed-report
  ([mutants source-lines] (printed-report mutants source-lines {}))
  ([mutants source-lines options]
   (with-out-str (report/print-report {:mutants mutants
                                       :totals {:mutants (count mutants)
                                                :killed 0
                                                :survived (count mutants)
                                                :no-coverage 0
                                                :unviable 0
                                                :timeout 0}}
                                      (merge {:source-lines source-lines
                                              :target "x.target"}
                                             options)))))

(defn- survived-section [output]
  (->> (str/split-lines output)
       (drop-while #(not= "Survived:" %))
       rest
       (take-while seq)
       vec))

(defn- swap-printed [line from to]
  (let [col (inc (str/index-of line from))]
    (->> (survived-section (printed-report [(survivor 1 col from to)] [line]))
         (drop 1)
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
         (swap-printed "  true" "true" "false"))))

(deftest survivor-not-found-at-its-place-shows-line-once
  (is (str/includes? (printed-report [(survivor 1 2 "when" "when-not")] ["  (if x 1 2)"])
                     "  1:2  when -> when-not  in f\n      (if x 1 2)\n")
      "other text at column")
  (is (str/includes? (printed-report [(survivor 1 9 "when" "when-not")] ["(if x)"])
                     "  1:9  when -> when-not  in f\n      (if x)\n")
      "column past line end"))

(deftest survivors-on-one-line-share-its-line-before
  (is (= ["  1:2  and -> or  in f"
          "    - (and a b)"
          "    + (or a b)"
          "  2:2  when -> when-not  in f"
          "    - (when (if x 1 2))"
          "    + (when-not (if x 1 2))"
          "  2:8  if -> if-not  in f"
          "    + (when (if-not x 1 2))"]
         (survived-section (printed-report [(survivor 2 2 "when" "when-not")
                                            (survivor 1 2 "and" "or")
                                            (survivor 2 8 "if" "if-not")]
                                           ["(and a b)" "(when (if x 1 2))"])))
      "survivors are grouped by line, in line order"))

(deftest survivor-not-found-keeps-line-before-of-its-group
  (is (= ["  1:2  when -> when-not  in f"
          "    - (when (if x 1 2))"
          "    + (when-not (if x 1 2))"
          "  1:3  if -> if-not  in f"
          "      (when (if x 1 2))"
          "  1:8  if -> if-not  in f"
          "    + (when (if-not x 1 2))"]
         (survived-section (printed-report [(survivor 1 2 "when" "when-not")
                                            (survivor 1 3 "if" "if-not")
                                            (survivor 1 8 "if" "if-not")]
                                           ["(when (if x 1 2))"])))))

(deftest swaps-far-apart-on-long-line-show-line-before-again
  (let [line (str "(when a) " (str/join (repeat 150 "b")) " (when c)")
        output (survived-section (printed-report [(survivor 1 2 "when" "when-not")
                                                  (survivor 1 162 "when" "when-not")]
                                                 [line]))]
    (is (= 2 (count (filter #(str/starts-with? % "    - ") output)))
        "second swap needs other part of line")
    (is (= (str "    - … " (str/join (repeat 87 "b")) " (when c)") (nth output 4)))))

(deftest line-of-line-width-stays-whole
  (let [as (str/join (repeat 50 "a"))
        bs (str/join (repeat 36 "b"))]
    (is (= [(str "    - " as " (when x) " bs)
            (str "    + " as " (when-not x) " bs)]
           (swap-printed (str as " (when x) " bs) "when" "when-not"))
        "100 characters after swap fit, even with swap past 40 characters")))

(deftest line-past-line-width-is-cut-at-its-end
  (let [line (str "(when x " (str/join (repeat 92 "b")))]
    (is (= [(str "    - " line)
            (str "    + (when-not x " (str/join (repeat 88 "b")) " …")]
           (swap-printed line "when" "when-not"))
        "swap near start keeps start, and only longer line gets end marker")))

(deftest long-line-is-cut-around-its-swap
  (let [line (str "    " (str/join (repeat 60 "a")) " (when x) " (str/join (repeat 60 "b")))]
    (is (= [(str "    - … " (str/join (repeat 38 "a")) " (when x) " (str/join (repeat 52 "b")) " …")
            (str "    + … " (str/join (repeat 38 "a")) " (when-not x) " (str/join (repeat 48 "b")) " …")]
           (swap-printed line "when" "when-not"))
        "40 characters before swap, 100 in all")))

(deftest swap-near-long-line-end-shows-100-characters-up-to-end
  (let [line (str "  " (str/join (repeat 150 "a")) " (when x)")]
    (is (= [(str "    - … " (str/join (repeat 87 "a")) " (when x)")
            (str "    + … " (str/join (repeat 87 "a")) " (when-not x)")]
           (swap-printed line "when" "when-not"))
        "cut moves back so longer line ends window")))

(defn- two-survivors-per-line [line-count]
  (mapcat #(vector (survivor % 2 "when" "when-not") (survivor % 8 "if" "if-not")) (range 1 (inc line-count))))

(deftest first-ten-lines-show-their-text
  (let [output (printed-report (two-survivors-per-line 11) (repeat 11 "(when (if x))"))]
    (is (str/includes? output "  10:8  if -> if-not  in f\n    + (when (if-not x))\n")
        "tenth line shows text for each of its swaps")
    (is (str/includes? output (str "  11:2  when -> when-not  in f\n"
                                   "  11:8  if -> if-not  in f\n"
                                   "  The first 10 lines show their text; --html FILE shows every survivor's line,"
                                   " and -r FILE writes them all as EDN.\n"))
        "eleventh line shows no text, and pointer follows")))

(deftest ten-lines-need-no-pointer
  (is (not (str/includes? (printed-report (two-survivors-per-line 10) (repeat 10 "(when (if x))"))
                          "The first 10"))
      "twenty survivors on ten lines all show text"))

(deftest pointer-names-files-run-writes
  (let [pointer #(->> (printed-report (two-survivors-per-line 11) (repeat 11 "(when (if x))") %)
                      str/split-lines
                      (filter (fn [line] (str/starts-with? line "  The first 10")))
                      first)]
    (is (= "  The first 10 lines show their text; every survivor's line is in r.html."
           (pointer {:html "r.html"
                     :report "r.edn"})))
    (is (= "  The first 10 lines show their text; --html FILE shows every survivor's line, and r.edn has them all as EDN."
           (pointer {:report "r.edn"})))))
