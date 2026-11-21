(ns cljs-patrol.dev.mutate-html-test
  (:require
   [cljs-patrol.dev.mutate-html :as mutate-html]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private source
  (str "(ns app.core)\n"
       "(defn label [x]\n"
       "  \"Name the sign of x.\"\n"
       "  (let [y (inc x)]\n"
       "    (if (pos? y)\n"
       "      :up\n"
       "      (first [:down]))))\n"
       "(defn unused [x] (when x 1))\n"))

(defn- mutant [function row col from to outcome & {:as more}]
  (merge {:def function
          :row row
          :col col
          :from from
          :to to
          :outcome outcome
          :covered-by (if (= :no-coverage outcome) [] ["app.core-test/labels" "app.core-test/signs"])}
         more))

(def ^:private mutants
  [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
   (mutant "label" 7 8 "first" "last" :survived)
   (mutant "unused" 8 19 "when" "when-not" :no-coverage)])

(defn- summary [mutants & {:as more}]
  (let [counts (frequencies (map :outcome mutants))
        totals (merge {:killed 0
                       :survived 0
                       :no-coverage 0
                       :unviable 0
                       :timeout 0}
                      counts
                      {:mutants (count mutants)})]
    (merge {:mutants mutants
            :definitions (vec (for [[function of-def] (group-by :def mutants)]
                                (merge {:killed 0
                                        :survived 0
                                        :no-coverage 0
                                        :unviable 0
                                        :timeout 0}
                                       (frequencies (map :outcome of-def))
                                       {:def function
                                        :mutants (count of-def)})))
            :tests-run {}
            :totals totals
            :score (let [scored (+ (:killed totals) (:survived totals) (:no-coverage totals))]
                     (when (pos? scored) (double (/ (:killed totals) scored))))
            :covered-score nil}
           more)))

(defn- page
  ([summary] (page summary {}))
  ([summary run]
   (mutate-html/page summary (merge {:target 'app.core
                                     :file "src/app/core.clj"
                                     :source source
                                     :test-nss ['app.core-test]
                                     :test-names ["app.core-test/labels" "app.core-test/signs"]}
                                    run))))

(defn- text-of [html]
  (-> (subs html (or (str/index-of html "<body>") 0))
      (str/replace #"<[^>]+>" " ")
      (str/replace #"\s+" " ")))

(deftest context-rows-climb-enclosing-forms-to-definition
  (is (= [2 4 5]
         (mutate-html/context-rows (str/split-lines source) 7))
      "docstring and sibling branch left out")
  (is (= []
         (mutate-html/context-rows (str/split-lines source) 8))
      "unindented line has no context"))

(deftest page-leads-with-score-and-gaps
  (let [text (-> mutants summary page text-of)]
    (is (str/includes? text "The tests caught 1 of 3 planted bugs (33%)"))
    (is (str/includes? text "2 test gaps in 2 functions. Work down this list: label : 1 gap, line 7 unused : no test calls it, line 8")
        "to-do lists each function with its gap lines")
    (is (str/includes? text "No test calls this function, so none of its 1 planted bug on line 8 can be caught."))
    (is (str/includes? text "Likely: every test that reaches this line has only one item here."))
    (is (str/includes? text "May be harmless"))))

(deftest page-shows-gap-line-before-and-after-swap
  (let [html (page (summary mutants))]
    (is (str/includes? html "<span class=\"was\">first</span> [:down]))))")
        "original operator marked in place")
    (is (str/includes? html "<span class=\"now\">last</span> [:down]))))")
        "swapped operator marked in place")))

(deftest page-escapes-source-text
  (let [html (page (summary [(mutant "label" 1 2 "if" "if-not" :survived)])
                   {:source "(if \"</script><marquee>\" 1)\n"})]
    (is (not (str/includes? html "<marquee>"))
        "source cannot add markup")
    (is (str/includes? html "&lt;/script&gt;&lt;marquee&gt;"))))

(deftest page-leaves-timeouts-and-unviable-mutants-out-of-score
  (let [text (-> mutants
                 (conj (mutant "label" 5 6 "if" "if-not" :timeout) (mutant "label" 5 6 "if" "if-not" :unviable))
                 summary
                 page
                 text-of)]
    (is (str/includes? text "The tests caught 1 of 3 planted bugs (33%)"))
    (is (str/includes? text "Timed out (left out of the score): 1"))
    (is (str/includes? text "Did not compile (left out of the score): 1"))))

(deftest page-warns-when-run-is-partial
  (let [text (text-of (page (summary mutants
                                     :stopped "the tests keep running"
                                     :unreached [{:def "unused"
                                                  :mutants 2}])
                            {:since "main"}))]
    (is (str/includes? text "The run stopped early: the tests keep running 2 planted bugs in unused did not run"))
    (is (str/includes? text "only in the lines changed since main"))))

(deftest page-names-closest-test-among-tests-reaching-gap
  (let [text (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/other")
                                      (mutant "label" 5 13 "pos?" "neg?" :killed :killed-by "app.core-test/signs")
                                      (mutant "label" 7 8 "first" "last" :survived)])))]
    (is (str/includes? text "Closest test to extend: signs (it runs for this function and caught 1 other planted bug"))))

(deftest page-counts-every-test-for-function-that-runs-them-all
  (let [text (text-of (page (summary [(mutant "label" 7 8 "first" "last" :survived :covered-by [])]
                                     :tests-run {"label" :every})))]
    (is (str/includes? text "2 tests run for label , and none failed with this planted bug"))
    (is (str/includes? text "labels 1 1"))))

(deftest page-without-gaps-says-nothing-to-fix
  (let [text (text-of (page (summary [(first mutants)])))]
    (is (str/includes? text "No test gaps: every planted bug the tests ran made a test fail."))
    (is (str/includes? text "Nothing to fix."))))

(defn- slice
  "Return the part of `html` from the first `start` on, up to the first `end` after it."
  [html start end]
  (let [from (str/index-of html start)]
    (subs html from (str/index-of html end (inc from)))))

(defn- gaps-of [html] (slice html "<h2>Test gaps</h2>" "<h2>By function</h2>"))

(defn- row-numbers [html] (map second (re-seq #"<span class=\"no\">([^<]*)</span>" html)))

(deftest context-rows-stop-at-first-line
  (is (= [1]
         (mutate-html/context-rows ["  (a" "    (b"] 2))
      "first line kept, nothing read before it"))

(deftest gap-code-marks-skipped-lines
  (is (= ["2" "…" "4" "5" "…" "7" ""]
         (row-numbers (slice (page (summary mutants)) "<div class=\"code\">" "</pre>")))
      "skip mark only where rows are missing")
  (is (= ["2" "…" "4" "5" ""]
         (row-numbers (slice (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)]))
                             "<div class=\"code\">" "</pre>")))
      "no skip mark before row right after its context"))

(deftest gap-shows-harmless-tag-only-for-harmless-swaps
  (is (not (str/includes? (gaps-of (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)]))) "May be harmless")))
  (is (str/includes? (gaps-of (page (summary [(mutant "label" 7 8 "first" "last" :survived)
                                              (mutant "label" 7 8 "[" "(" :survived)])))
                     "May be harmless")
      "one harmless swap on line is enough"))

(deftest gap-names-shared-line-only-for-several-gaps
  (is (not (str/includes? (page (summary mutants)) "on this line went unnoticed")))
  (is (str/includes? (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)
                                              (mutant "label" 5 10 "pos?" "neg?" :survived)])))
                     "2 planted bugs on this line went unnoticed")))

(deftest gap-lists-tests-per-line-when-they-differ
  (let [text (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)
                                      (mutant "label" 7 8 "first" "last" :survived
                                              :covered-by ["app.core-test/labels"])])))]
    (is (str/includes? text "2 tests run for label , and none failed with this planted bug"))
    (is (str/includes? text "1 test runs for label , and none failed with this planted bug"))))

(deftest function-card-counts-caught-planted-bugs
  (let [text (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 5 10 "pos?" "neg?" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 6 7 "true" "false" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 7 8 "first" "last" :survived)
                                      (mutant "label" 4 12 "inc" "dec" :no-coverage)])))]
    (is (str/includes? text "label 2 gaps, 3 of 5 planted bugs caught")
        "mixed gaps make normal card")
    (is (str/includes? text "label : 2 gaps, lines 4 to 7")
        "to-do counts gaps of mixed function")
    (is (str/includes? text "Closest test to extend: labels (it runs for this function and caught 3 other"))))

(deftest function-card-prefers-test-that-caught-most
  (is (str/includes? (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/signs")
                                              (mutant "label" 5 10 "pos?" "neg?" :killed :killed-by "app.core-test/labels")
                                              (mutant "label" 4 12 "inc" "dec" :killed :killed-by "app.core-test/labels")
                                              (mutant "label" 7 8 "first" "last" :survived)])))
                     "Closest test to extend: labels")))

(deftest function-table-folds-functions-without-gaps
  (let [html (-> mutants
                 (conj (mutant "other" 2 2 "if" "if-not" :killed :killed-by "app.core-test/labels"))
                 summary
                 page)]
    (is (str/includes? (slice html "<tbody>" "</tbody>") "label"))
    (is (not (str/includes? (slice html "<tbody>" "</tbody>") "other")))
    (is (str/includes? (text-of (slice html "<details class=\"more\">" "</details>"))
                       "1 more function with no gaps, 1 planted bug in all other")))
  (is (not (str/includes? (-> mutants rest summary page) "more function"))
      "nothing to fold"))

(deftest test-table-orders-and-folds-tests
  (let [html (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
                             (mutant "label" 7 8 "first" "last" :survived :covered-by ["app.core-test/signs"])
                             (mutant "label" 4 12 "inc" "dec" :survived)]
                            :definitions [])
                   {:test-names ["app.core-test/labels" "app.core-test/signs" "app.core-test/quiet"]})
        tests (slice html "<h2>By test</h2>" "</section>")]
    (is (str/includes? (text-of tests) "signs 3 2 labels 2 1")
        "more gaps first, then more reaches")
    (is (not (str/includes? tests "more test")))
    (is (not (str/includes? tests "num zero"))))
  (let [tests (slice (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
                                     (mutant "label" 7 8 "first" "last" :survived :covered-by ["app.core-test/signs"])]))
                     "<h2>By test</h2>" "</section>")]
    (is (str/includes? (text-of tests) "signs 2 1 1 more test that reaches no gap labels 1 0"))
    (is (str/includes? tests "<td class=\"num zero\">0</td>"))))

(deftest source-view-marks-each-line
  (let [html (-> mutants (conj (mutant "label" 4 12 "inc" "dec" :timeout)) summary page)
        source-view (slice html "<details class=\"fold\" id=\"source\">" "</details>")]
    (is (str/includes? source-view "<input checked=\"checked\" id=\"show-all\" name=\"show\" type=\"radio\" />")
        "all lines shown first")
    (is (not (re-find #"checked=\"checked\" id=\"show-(marked|gaps|caught)\"" source-view)))
    (is (str/includes? source-view "<span class=\"ln\" id=\"L1\">"))
    (is (str/includes? source-view "<span class=\"ln other\" id=\"L4\"><span class=\"no\">4</span><span class=\"mk\" title=\"0 of 1 planted bugs caught\">left out</span>"))
    (is (str/includes? source-view "<span class=\"ln caught\" id=\"L5\"><span class=\"no\">5</span><span class=\"mk\" title=\"1 of 1 planted bugs caught\">1 caught</span>"))
    (is (str/includes? source-view "<span class=\"ln missed\" id=\"L7\"><span class=\"no\">7</span><span class=\"mk\" title=\"0 of 1 planted bugs caught\">1 gap</span>"))
    (is (str/includes? source-view "first to last: not caught"))
    (is (str/includes? source-view "when to when-not: never run"))
    (is (re-find #"<span class=\"mk\"></span>\(ns app.core\)" source-view)
        "unmarked line has empty marker")))

(deftest numbers-table-shows-counts-that-apply
  (let [numbers (text-of (slice (page (summary [(first mutants)] :score 1.0)) "<summary>All numbers" "</details>"))]
    (is (str/includes? numbers "Caught: a test failed 1 Planted bugs 1 Score: caught, of all that the tests ran or should have run 100%"))
    (is (not (str/includes? numbers "Not caught")))
    (is (not (str/includes? numbers "Covered score"))))
  (let [numbers (text-of (slice (page (summary [(last mutants)] :score nil :covered-score nil)) "<summary>All numbers" "</details>"))]
    (is (str/includes? numbers "Score: caught, of all that the tests ran or should have run n/a Covered score: leaves out never run n/a"))))

(deftest header-shows-run-note
  (is (str/includes? (text-of (page (summary mutants :note "Every mutant ran every test."))) "Every mutant ran every test."))
  (is (not (str/includes? (page (summary mutants)) "class=\"banner\""))))

(deftest test-table-breaks-gap-ties-by-reach
  (is (str/includes? (text-of (slice (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/signs"
                                                             :covered-by ["app.core-test/signs"])
                                                     (mutant "label" 7 8 "first" "last" :survived)]))
                                     "<h2>By test</h2>" "</section>"))
                     "signs 2 1 labels 1 1")
      "same gaps, more reaches first"))
