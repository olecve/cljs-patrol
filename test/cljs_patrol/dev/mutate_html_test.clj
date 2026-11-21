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
          :outcome outcome}
         more))

(def ^:private mutants
  [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
   (mutant "label" 7 8 "first" "last" :survived)
   (mutant "unused" 8 19 "when" "when-not" :no-coverage)])

(def ^:private both-tests ["app.core-test/labels" "app.core-test/signs"])

(defn- summary
  "Build the summary of a run of `mutants`, as the runner does, with each definition's tests from `tests`."
  [mutants & {:keys [tests]
              :or {tests {}}
              :as more}]
  (let [zero {:killed 0
              :survived 0
              :no-coverage 0
              :unviable 0
              :timeout 0}
        totals (merge zero (frequencies (map :outcome mutants)) {:mutants (count mutants)})
        scored (+ (:killed totals) (:survived totals) (:no-coverage totals))]
    (merge {:mutants mutants
            :definitions (vec (for [[function of-def] (group-by :def mutants)]
                                (merge zero
                                       (frequencies (map :outcome of-def))
                                       {:def function
                                        :mutants (count of-def)
                                        :tests (get tests function both-tests)})))
            :totals totals
            :score (when (pos? scored) (double (/ (:killed totals) scored)))
            :covered-score nil}
           (dissoc more :tests))))

(defn- page
  ([summary] (page summary {}))
  ([summary run]
   (mutate-html/page summary (merge {:target 'app.core
                                     :file "src/app/core.clj"
                                     :source source
                                     :test-nss ['app.core-test]}
                                    run))))

(defn- text-of [html]
  (-> (subs html (or (str/index-of html "<body>") 0))
      (str/replace #"<[^>]+>" " ")
      (str/replace #"\s+" " ")))

(defn- slice
  "Return the part of `html` from the first `start` on, up to the first `end` after it."
  [html start end]
  (let [from (str/index-of html start)]
    (subs html from (str/index-of html end (inc from)))))

(defn- gaps-of [html] (slice html "<h2>Test gaps</h2>" "<h2>By function</h2>"))

(defn- tests-of [html] (text-of (slice html "<h2>By test</h2>" "</section>")))

(defn- row-numbers [html] (map second (re-seq #"<span class=\"no\">([^<]*)</span>" html)))

(deftest context-rows-climb-enclosing-forms-to-definition
  (is (= [2 4 5]
         (mutate-html/context-rows (str/split-lines source) 7))
      "docstring and sibling branch left out")
  (is (= []
         (mutate-html/context-rows (str/split-lines source) 8))
      "unindented line has no context"))

(deftest context-rows-stop-at-first-line
  (is (= [1]
         (mutate-html/context-rows ["  (a" "    (b"] 2))
      "first line kept, nothing read before it"))

(deftest context-rows-skip-string-and-comment-lines
  (is (= [1 2]
         (mutate-html/context-rows ["(defn f [x]" "  (let [q \"select *" "from t\"]" "  ; why" "    (when x q)))"] 5))
      "rest of multi-line string and comment are no forms")
  (is (= [1]
         (mutate-html/context-rows ["(defn f [x]" "  (log x)" "  (when x 1))"] 3))
      "sibling form at same indent left out"))

(deftest page-leads-with-score-and-gaps
  (let [text (-> mutants summary page text-of)]
    (is (str/includes? text "The tests caught 1 of 3 planted bugs (33%)"))
    (is (str/includes? text (str "2 test gaps in 2 functions. Work down this list: label : 1 gap, line 7"
                                 " unused : no test calls it, line 8"))
        "to-do lists each function with its gap lines")
    (is (str/includes? text "No test calls this function, so none of its 1 planted bug on line 8 can be caught."))
    (is (str/includes? text "Likely: every test that reaches this line has only one item here."))
    (is (str/includes? text "2 tests run for label , and none failed with this planted bug"))))

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

(deftest page-without-gaps-says-nothing-to-fix
  (let [text (text-of (page (summary [(first mutants)])))]
    (is (str/includes? text "No test gaps: every planted bug the tests ran made a test fail."))
    (is (str/includes? text "Nothing to fix."))))

(deftest header-shows-run-note
  (is (str/includes? (text-of (page (summary mutants :note "Every mutant ran every test.")))
                     "Every mutant ran every test."))
  (is (not (str/includes? (page (summary mutants)) "class=\"banner\""))))

(deftest gap-code-marks-skipped-lines
  (is (= ["2" "…" "4" "5" "…" "7" ""]
         (row-numbers (slice (page (summary mutants)) "<div class=\"code\">" "</pre>")))
      "skip mark only where rows are missing")
  (is (= ["2" "…" "4" "5" ""]
         (row-numbers (slice (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)]))
                             "<div class=\"code\">" "</pre>")))
      "no skip mark before row right after its context"))

(deftest gap-shows-harmless-tag-only-for-harmless-swaps
  (is (not (str/includes? (gaps-of (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)])))
                          "May be harmless")))
  (is (str/includes? (gaps-of (page (summary [(mutant "label" 7 8 "first" "last" :survived)
                                              (mutant "label" 7 8 "[" "(" :survived)])))
                     "May be harmless")
      "one harmless swap on line is enough"))

(deftest gap-names-shared-line-only-for-several-gaps
  (is (not (str/includes? (page (summary mutants)) "on this line went unnoticed")))
  (is (str/includes? (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :survived)
                                              (mutant "label" 5 10 "pos?" "neg?" :survived)])))
                     "2 planted bugs on this line went unnoticed")))

(deftest function-card-counts-caught-planted-bugs
  (let [text (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 5 10 "pos?" "neg?" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 6 7 "true" "false" :killed :killed-by "app.core-test/labels")
                                      (mutant "label" 7 8 "first" "last" :survived)
                                      (mutant "label" 4 12 "inc" "dec" :survived)])))]
    (is (str/includes? text "label 2 gaps, 3 of 5 planted bugs caught"))
    (is (str/includes? text "label : 2 gaps, lines 4 to 7"))
    (is (str/includes? text "Closest test to extend: labels (it runs for this function and caught 3 other"))))

(deftest function-card-names-closest-test-among-tests-that-ran
  (is (str/includes? (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/other")
                                              (mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/other")
                                              (mutant "label" 5 13 "pos?" "neg?" :killed :killed-by "app.core-test/signs")
                                              (mutant "label" 7 8 "first" "last" :survived)])))
                     "Closest test to extend: signs (it runs for this function and caught 1 other planted bug"))
  (is (str/includes? (text-of (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/signs")
                                              (mutant "label" 5 10 "pos?" "neg?" :killed :killed-by "app.core-test/labels")
                                              (mutant "label" 4 12 "inc" "dec" :killed :killed-by "app.core-test/labels")
                                              (mutant "label" 7 8 "first" "last" :survived)])))
                     "Closest test to extend: labels")
      "test that caught most wins"))

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

(deftest test-table-folds-tests-that-reach-no-gap
  (let [html (page (summary [(mutant "label" 5 6 "if" "if-not" :killed :killed-by "app.core-test/labels")
                             (mutant "other" 2 2 "if" "if-not" :killed :killed-by "app.core-test/signs")
                             (mutant "other" 2 6 "when" "when-not" :survived)
                             (mutant "other" 2 9 "and" "or" :survived)]
                            :tests {"label" ["app.core-test/labels"]
                                    "other" ["app.core-test/signs"]}))]
    (is (str/includes? (tests-of html) "signs 3 2 1 more test that reaches no gap labels 1 0"))
    (is (str/includes? html "<td class=\"num zero\">0</td>")))
  (let [html (page (summary mutants))]
    (is (not (str/includes? (tests-of html) "more test")))
    (is (not (str/includes? html "num zero")))))

(deftest test-table-breaks-gap-ties-by-reach
  (is (str/includes? (tests-of (page (summary [(mutant "label" 7 8 "first" "last" :survived)
                                               (mutant "other" 2 2 "if" "if-not" :killed :killed-by "app.core-test/signs")]
                                              :tests {"other" ["app.core-test/signs"]})))
                     "signs 2 1 labels 1 1")
      "same gaps, more reaches first"))

(deftest test-table-counts-only-planted-bugs-whose-tests-ran
  (is (str/includes? (tests-of (page (summary [(mutant "label" 7 8 "first" "last" :survived)
                                               (mutant "label" 5 6 "if" "if-not" :unviable)
                                               (mutant "label" 4 12 "inc" "dec" :killed :at-load? true)
                                               (mutant "label" 6 7 "true" "false" :timeout)]
                                              :tests {"label" ["app.core-test/labels"]})))
                     "labels 2 1")
      "unviable and killed at load reach no test"))

(deftest source-view-marks-each-line
  (let [html (-> mutants (conj (mutant "label" 4 12 "inc" "dec" :timeout)) summary page)
        source-view (slice html "<details class=\"fold\" id=\"source\">" "</details>")
        line (fn [kind row marker]
               (str "<span class=\"ln " kind "\" id=\"L" row "\"><span class=\"no\">" row "</span><span class=\"mk\" title=\""
                    marker))]
    (is (str/includes? source-view "<input checked=\"checked\" id=\"show-all\" name=\"show\" type=\"radio\" />")
        "all lines shown first")
    (is (not (re-find #"checked=\"checked\" id=\"show-(marked|gaps|caught)\"" source-view)))
    (is (str/includes? source-view "<span class=\"ln\" id=\"L1\">"))
    (is (str/includes? source-view (line "other" 4 "0 of 1 planted bugs caught\">left out")))
    (is (str/includes? source-view (line "caught" 5 "1 of 1 planted bugs caught\">1 caught")))
    (is (str/includes? source-view (line "missed" 7 "0 of 1 planted bugs caught\">1 gap")))
    (is (str/includes? source-view "first to last: not caught"))
    (is (str/includes? source-view "when to when-not: never run"))
    (is (re-find #"<span class=\"mk\"></span>\(ns app.core\)" source-view)
        "unmarked line has empty marker")))

(deftest numbers-table-shows-counts-that-apply
  (let [numbers (text-of (slice (page (summary [(first mutants)])) "<summary>All numbers" "</details>"))]
    (is (str/includes? numbers (str "Caught: a test failed 1 Planted bugs 1"
                                    " Score: caught, of all that the tests ran or should have run 100%")))
    (is (not (str/includes? numbers "Not caught")))
    (is (not (str/includes? numbers "Covered score"))))
  (let [numbers (text-of (slice (page (summary [(last mutants)] :score nil)) "<summary>All numbers" "</details>"))]
    (is (str/includes? numbers (str "Score: caught, of all that the tests ran or should have run n/a"
                                    " Covered score: leaves out never run n/a")))))
