(ns cljs-patrol.dev.mutate-html
  "An HTML page for a mutation run that leads with the test gaps and what to do about each.

  The page needs no network: its style and its one script are inline. Its words follow
  docs/mutation-testing.md: a planted bug is a mutant, caught means killed, and a test gap is a
  mutant that survived or that no test reaches."
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup2.core :as h]))

(def ^:private css (slurp (io/resource "cljs_patrol/dev/mutate_report.css")))

(def ^:private script
  "Open the source at the line that a `#L<row>` link names, with every line shown, also when the link is followed again."
  (str "const show = () => {"
       " const row = location.hash.match(/^#L(\\d+)$/);"
       " if (!row) return;"
       " document.getElementById('source').open = true;"
       " document.getElementById('show-all').checked = true;"
       " document.getElementById('L' + row[1])?.scrollIntoView({block: 'center'});"
       "};"
       " addEventListener('hashchange', show);"
       " addEventListener('click', event => { if (event.target.closest('a[href^=\"#L\"]')) setTimeout(show); });"
       " show();"))

(def hints
  "The likely reason a test misses a swap, by the two operators it swaps: one for each pair of `mutate/swaps`.

  A swap of integer literals has one hint of its own, `number-hint`."
  {#{"<" "<="} "no test uses the boundary value, where both sides are equal."
   #{">" ">="} "no test uses the boundary value, where both sides are equal."
   #{"=" "not="} "no test checks both outcomes of this comparison."
   #{"and" "or"} "no test has one side of this true and the other false."
   #{"dec" "inc"} "no test checks the exact number this gives."
   #{"empty?" "seq"} "no test checks both an empty and a non-empty collection here."
   #{"every?" "some"} "no test has a mix of items that match and items that do not."
   #{"false" "true"} "no test reaches this branch, or no test checks the value it gives."
   #{"false?" "true?"} "no test checks both a true and a false value here."
   #{"filter" "remove"} "no test has a mix of items that this filter keeps and drops."
   #{"first" "last"} "every test that reaches this line has only one item here. Add a case with two or more."
   #{"if" "if-not"} "no test checks both sides of this condition."
   #{"nil?" "some?"} "no test checks both a nil and a non-nil value here."
   #{"when" "when-not"} "no test checks both sides of this condition."})

(def ^:private number-hint
  "The likely reason a test misses a swap of one integer literal for another."
  "no test checks the exact number here, or the boundary it sets.")

(def ^:private maybe-harmless
  "Why a swap may behave the same as the original in every case, by the two operators it swaps."
  {#{"first" "last"} (str "This is harmless if the collection here can only ever hold one item."
                          " Then both versions behave the same.")})

(def ^:private sources
  "The papers and docs behind the page's advice and words: each title, its link, and what it backs."
  [["Petrović, Ivanković, Fraser, Just. " "Practical Mutation Testing at Scale: A view from Google"
    "https://arxiv.org/abs/2102.11378"
    (str ". IEEE TSE, 2021. Google shows at most one surviving mutant per line, and skips \"arid\" code such as"
         " logging, which raised the share of mutants developers found useful from about 15% to 89%.")]
   ["Petrović, Ivanković, Fraser, Just. " "Does mutation testing improve testing practices?"
    "https://arxiv.org/abs/2103.07189"
    " ICSE, 2021. Developers who saw mutants wrote more and better tests over time."]
   ["Petrović et al. " "Please fix this mutant: How do developers resolve mutants surfaced during code review?"
    "https://research.google/pubs/please-fix-this-mutant-how-do-developers-resolve-mutants-surfaced-during-code-review/"
    " ICSE SEIP, 2023. How developers act on the mutants a review shows them."]
   ["Schuler, Zeller. " "(Un-)Covering Equivalent Mutants"
    "https://www.st.cs.uni-saarland.de/publications/files/schuler-icst-2010.pdf"
    ". ICST, 2010. How often a survivor is an equivalent mutant, and what it costs to check one by hand."]
   ["Stryker. " "Mutant states and metrics"
    "https://stryker-mutator.io/docs/mutation-testing-elements/mutant-states-and-metrics/"
    ". The usual names for each outcome and score."]
   ["cargo-mutants. " "Using the results" "https://mutants.rs/using-results.html"
    ". Test the behavior, not the mutant."]])

(defn- plural [n one many] (str n " " (if (= 1 n) one many)))

(defn- line-range [rows]
  (let [first-row (apply min rows)
        last-row (apply max rows)]
    (if (= first-row last-row) (str "line " first-row) (str "lines " first-row " to " last-row))))

(defn- gap? [{:keys [outcome]}] (contains? #{:survived :no-coverage} outcome))

; mutate: skip every?: a function's gaps are all not caught or all never run, never a mix
(defn- never-run? [gaps] (every? #(= :no-coverage (:outcome %)) gaps))

(defn- swap-pair [{:keys [from to]}] #{from to})

(defn- hint
  "Return the likely reason a test misses `mutant`, by the operators it swaps, or nil when there is none."
  [{:keys [from]
    :as mutant}]
  (or (hints (swap-pair mutant))
      (when (re-matches #"-?\d+" from) number-hint)))

(defn- indent [line] (count (re-find #"^ *" line)))

(defn- string-open-after
  "True when a string or regex is still open at the end of `line`, given whether one was open at its start."
  [line open?]
  (loop [i 0
         open? open?]
    (if (>= i (count line))
      open?
      (case (.charAt ^String line i)
        \\ (recur (+ i 2) open?)
        \" (recur (inc i) (not open?))
        \; (if open? (recur (inc i) open?) open?)
        (recur (inc i) open?)))))

(defn context-rows
  "Return the rows of the forms around `row` in `lines`, each less indented than the next, up to an unindented one.

  Only a line that opens a form outside a string counts, so a docstring, a comment or the rest
  of a multi-line string is left out."
  [lines row]
  (let [in-string (vec (reductions #(string-open-after %2 %1) false (take row lines)))]
    (loop [r (dec row)
           limit (indent (nth lines (dec row) ""))
           shown ()]
      (if (or (< r 1) (zero? limit))
        shown
        (let [text (nth lines (dec r))
              opens-form? (and (not (in-string (dec r))) (re-find #"^\s*[(\[{#]" text))]
          (if (and opens-form? (< (indent text) limit))
            ; mutate: skip dec: stepping back down past the form just taken only meets lines already left out
            (recur (dec r) (indent text) (conj shown r))
            (recur (dec r) limit shown)))))))

(defn- marked
  "Return the text of a line with each swapped operator wrapped in a span, written by `pick`."
  [text cut mutants span-class pick]
  (loop [[mutant & more] (sort-by :col mutants)
         at 0
         parts []]
    (if mutant
      (let [start (max at (- (:col mutant) 1 cut))]
        (recur more
               (+ start (count (:from mutant)))
               (conj parts (subs text at start) [:span {:class span-class} (pick mutant)])))
      (seq (conj parts (subs text (min at (count text))))))))

(defn- code-block
  "Return the gap's line before and after its swaps, below the forms around it."
  [lines row mutants]
  (let [line-text #(nth lines (dec %) "")
        context (context-rows lines row)
        cut (apply min (map (comp indent line-text) (conj (vec context) row)))
        skipped [:span {:class "row ctx"} [:span {:class "no"} "…"] [:span] "\n"]
        text (subs (line-text row) cut)]
    [:div {:class "code"}
     [:pre
      (for [[previous r] (map vector (cons nil context) context)]
        (list (when (and previous (> r (inc previous))) skipped)
              [:span {:class "row ctx"} [:span {:class "no"} r] [:span] (str (subs (line-text r) cut) "\n")]))
      (when (and (seq context) (> row (inc (last context)))) skipped)
      [:span {:class "row mine"} [:span {:class "no"} row] [:span {:class "lab"} "now"]
       [:span (marked text cut mutants "from" :from) "\n"]]
      (for [mutant (sort-by :col mutants)]
        [:span {:class "row bug"} [:span {:class "no"}] [:span {:class "lab"} "with bug"]
         [:span (marked text cut [mutant] "to" :to) "\n"]])]]))

(defn- tests-list [function tests these?]
  (when (seq tests)
    [:details {:class "tests"}
     [:summary (str (plural (count tests) "test runs" "tests run") " for ") [:code function]
      (str ", and none failed with " (if these? "these planted bugs" "this planted bug"))]
     [:ul (for [test-name tests] [:li [:code test-name]])]]))

(defn- gap-section [lines target row mutants]
  [:div {:class "gap"}
   [:div {:class "gap-head"}
    [:strong (interpose ", " (for [{:keys [from to]} mutants] (list [:code from] " to " [:code to])))]
    [:a {:href (str "#L" row)} (str "line " row)]
    (when (some (comp maybe-harmless swap-pair) mutants) [:span {:class "tag"} "May be harmless"])]
   (code-block lines row mutants)
   (when (next mutants)
     [:p {:class "shared"}
      (str (plural (count mutants) "planted bug" "planted bugs") " on this line went unnoticed, so no test makes this"
           " part of the line matter. One new test will likely catch all of them.")])
   (for [reason (distinct (keep hint mutants))]
     [:p {:class "why"} (str "Likely: " reason)])
   (for [reason (distinct (keep (comp maybe-harmless swap-pair) mutants))]
     [:p {:class "harmless"} reason])
   [:details {:class "tests"}
    [:summary "Watch a new test fail on it"]
    [:p "Write the planted bug into the file, run the tests, then put the code back with "
     [:code "clojure -M:test:mutate --restore"] ":"]
    [:ul (for [mutant mutants]
           [:li [:code (mutate/apply-command target mutant)]])]]])

(defn- function-card
  "Return the card of a definition with test gaps: the closest test to extend, then each gap line by line."
  [lines target {:keys [function anchor mutants gaps tests]}]
  (if (never-run? gaps)
    [:article {:class "fn unrun"
               :id anchor}
     [:div {:class "fn-head"} [:h3 [:code function]] [:span {:class "count"} "never run"]]
     [:p {:class "why"}
      (str "No test calls this function, so none of its " (plural (count gaps) "planted bug" "planted bugs") " on ")
      [:a {:href (str "#L" (apply min (map :row gaps)))} (line-range (map :row gaps))]
      " can be caught. Start with one test that calls it."]]
    (let [[nearest caught] (->> mutants
                                (keep :killed-by)
                                (filter (set tests))
                                frequencies
                                (sort-by (juxt (comp - val) key))
                                first)]
      [:article {:class "fn"
                 :id anchor}
       [:div {:class "fn-head"}
        [:h3 [:code function]]
        [:span {:class "count"} (str (plural (count gaps) "gap" "gaps") ", "
                                     (count (filter #(= :killed (:outcome %)) mutants)) " of " (count mutants)
                                     " planted bugs caught")]]
       (when nearest
         [:p {:class "nearest"} "Closest test to extend: " [:code nearest]
          (str " (it runs for this function and caught " (plural caught "other planted bug" "other planted bugs")
               " in it).")])
       (tests-list function tests (next gaps))
       (for [[row on-row] (sort-by key (group-by :row gaps))]
         (gap-section lines target row on-row))])))

(defn- bar [{:keys [killed survived no-coverage]}]
  (let [scored (+ killed survived no-coverage)
        width #(str "width:" (if (pos? scored) (/ (* 100.0 %) scored) 0) "%")]
    [:div {:class "share"}
     [:b (mutate/percent (when (pos? scored) (/ killed scored)))]
     [:div {:class "bar"
            :aria-hidden "true"}
      [:i {:class "c"
           :style (width killed)}]
      [:i {:class "m"
           :style (width survived)}]
      [:i {:class "u"
           :style (width no-coverage)}]]]))

(def ^:private function-columns
  [:colgroup [:col {:style "width:40%"}] [:col {:style "width:11%"}] [:col {:style "width:11%"}]
   [:col {:style "width:10%"}] [:col {:style "width:28%"}]])

(defn- function-row [anchors {function :def
                              :keys [mutants killed survived no-coverage]
                              :as counts}]
  [:tr
   [:td (if-let [anchor (get anchors function)] [:a {:href (str "#" anchor)} [:code function]] [:code function])]
   [:td {:class "num"} mutants]
   [:td {:class "num"} killed]
   [:td {:class "num"} (+ survived no-coverage)]
   [:td (bar counts)]])

(defn- function-table [definitions anchors]
  (let [with-gaps? #(pos? (+ (:survived %) (:no-coverage %)))
        [gapped clean] ((juxt filter remove) with-gaps? definitions)]
    [:div {:class "table-wrap"}
     [:table
      function-columns
      [:thead [:tr [:th "Function"] [:th {:class "num"} "Planted"] [:th {:class "num"} "Caught"]
               [:th {:class "num"} "Gaps"] [:th "Caught share"]]]
      [:tbody (map #(function-row anchors %) (sort-by #(- (+ (:survived %) (:no-coverage %))) gapped))]]
     (when (seq clean)
       [:details {:class "more"}
        [:summary (str (plural (count clean) "more function" "more functions") " with no gaps, "
                       (plural (reduce + (map :mutants clean)) "planted bug" "planted bugs") " in all")]
        [:table function-columns [:tbody (map #(function-row anchors %) clean)]]])]))

(def ^:private test-columns
  [:colgroup [:col {:style "width:60%"}] [:col {:style "width:20%"}] [:col {:style "width:20%"}]])

(defn- test-table [mutants tests-of]
  (let [stats (reduce (fn [stats mutant]
                        (reduce #(update %1 %2 (fnil (fn [[reaches gaps]]
                                                       [(inc reaches) (cond-> gaps (= :survived (:outcome mutant)) inc)])
                                                     [0 0]))
                                stats
                                (tests-of mutant)))
                      {}
                      mutants)
        row (fn [[test-name [reaches gaps]]]
              [:tr [:td [:code test-name]] [:td {:class "num"} reaches]
               [:td {:class (if (pos? gaps) "num" "num zero")} gaps]])
        [at-gaps rest-tests] ((juxt filter remove) #(pos? (second (val %)))
                                                   (sort-by (juxt (comp - second val) (comp - first val) key) stats))]
    [:div {:class "table-wrap"}
     [:table
      test-columns
      [:thead [:tr [:th "Test"] [:th {:class "num"} "Reaches"] [:th {:class "num"} "Reaches a gap, still passes"]]]
      [:tbody (map row at-gaps)]]
     (when (seq rest-tests)
       [:details {:class "more"}
        [:summary (plural (count rest-tests) "more test that reaches no gap" "more tests that reach no gap")]
        [:table test-columns [:tbody (map row rest-tests)]]])]))

(defn- source-view [lines mutants ignored]
  (let [by-row (group-by :row mutants)
        ignored-by-row (group-by :row ignored)]
    [:details {:class "fold"
               :id "source"}
     [:summary "Source, with every planted bug marked"]
     [:div {:class "filters"
            :role "radiogroup"
            :aria-label "Lines to show"}
      (for [[value label] [["all" "All lines"] ["marked" "Lines with planted bugs"] ["gaps" "Only gaps"]
                           ["caught" "Only caught"]]]
        [:label [:input (cond-> {:type "radio"
                                 :name "show"
                                 :id (str "show-" value)}
                          (= "all" value) (assoc :checked true))]
         label])]
     [:div {:class "src"}
      [:pre
       (for [[i text] (map-indexed vector lines)
             :let [row (inc i)
                   here (get by-row row)
                   ignored (get ignored-by-row row)
                   gaps (filter gap? here)
                   caught (count (filter #(= :killed (:outcome %)) here))]]
         (list [:span {:class (str "ln"
                                   (when (seq gaps) " missed")
                                   (when (pos? caught) " caught")
                                   (when (and (seq here) (empty? gaps) (zero? caught)) " other")
                                   (when (and (empty? here) (seq ignored)) " ignored"))
                       :id (str "L" row)}
                [:span {:class "no"} row]
                [:span {:class "mk"
                        :title (not-empty (str/join "; " (concat (when (seq here)
                                                                   [(str caught " of " (count here) " planted bugs caught")])
                                                                 (when (seq ignored)
                                                                   [(str (count ignored) " ignored")]))))}
                 (cond
                   (seq gaps) (plural (count gaps) "gap" "gaps")
                   (pos? caught) (str caught " caught")
                   (seq here) "left out"
                   (seq ignored) "ignored")]
                (str text "\n")]
               (for [{:keys [from to outcome]} gaps]
                 [:span {:class "sub"} [:span] [:span]
                  (str from " to " to ": " (if (= :no-coverage outcome) "never run" "not caught") "\n")])
               (for [{:keys [from to reason]} ignored]
                 [:span {:class "sub ignored"} [:span] [:span] (str from " to " to ": ignored, " reason "\n")])))]]]))

(defn- numbers-table [{:keys [totals score covered-score unreached]}]
  [:details {:class "fold"}
   [:summary "All numbers"]
   [:table
    [:tbody
     (for [[label value] (concat (remove (comp zero? second)
                                         [["Caught: a test failed" (:killed totals)]
                                          ["Not caught: the tests ran and passed" (:survived totals)]
                                          ["Never run: no test calls the code" (:no-coverage totals)]
                                          ["Timed out: left out of the score" (:timeout totals)]
                                          ["Did not compile: left out of the score" (:unviable totals)]
                                          ["Not run: the run stopped first" (reduce + (map :mutants unreached))]])
                                 [["Planted bugs" (:mutants totals)]
                                  ["Score: caught, of all that the tests ran or should have run"
                                   (mutate/percent score)]]
                                 (when (pos? (:no-coverage totals))
                                   [["Covered score: leaves out never run"
                                     (mutate/percent covered-score)]]))]
       [:tr [:td label] [:td {:class "num"} value]])]]])

(def ^:private terms
  [:details {:class "fold"}
   [:summary "Terms and sources"]
   [:div {:class "terms"}
    [:dl
     [:dt "Planted bug"]
     [:dd "One small change to the code under test, such as " [:code "when"] " to " [:code "when-not"]
      ". The tests never change. Other tools call it a mutant."]
     [:dt "Caught"]
     [:dd "A test failed, or the changed code threw while it loaded. Other tools say the mutant was killed."]
     [:dt "Test gap"]
     [:dd "A planted bug that no test caught: the tests ran and passed (the mutant survived), or no test calls that"
      " code (no coverage)."]
     [:dt "May be harmless"]
     [:dd "A planted bug that may behave the same as the original code in every case, so no test can catch it."
      " Other tools call it an equivalent mutant. Only a reader can tell for sure."]
     [:dt "Timed out"]
     [:dd "The tests ran past the time limit. Left out of the score, since a loop and a slow test look the same."]
     [:dt "Did not compile"]
     [:dd "The changed code could not be read or compiled. It shows nothing about the tests and is left out of the"
      " score. Other tools say unviable or compile error."]
     [:dt "Score"]
     [:dd "Caught planted bugs divided by those caught, not caught and never run. Other tools call it the mutation"
      " score. The covered score leaves out code no test calls."]]
    [:h3 "Sources"]
    [:ol (for [[authors title link note] sources]
           [:li authors [:a {:href link} title] note])]]])

(defn- ignored-table
  "Return the planted bugs a skip marker left out, each with its reason, or nil when there are none."
  [ignored]
  (when (seq ignored)
    [:details {:class "fold"}
     [:summary (str "Ignored with a skip marker: " (count ignored))]
     [:table
      [:tbody
       (for [{function :def
              :keys [row from to reason]} ignored]
         [:tr [:td [:a {:href (str "#L" row)} (str "line " row)]] [:td [:code from] " to " [:code to]]
          [:td [:code function]] [:td reason]])]]]))

(defn- header [{:keys [target file test-nss since]} {:keys [totals score stopped unreached note ignored]} cards]
  (let [{:keys [killed survived no-coverage timeout unviable]} totals
        scored (+ killed survived no-coverage)
        gaps (+ survived no-coverage)]
    [:header
     (when stopped
       [:p {:class "banner"}
        [:strong (str "The run stopped early: " stopped)]
        (when (seq unreached)
          (str " " (plural (reduce + (map :mutants unreached)) "planted bug" "planted bugs") " in "
               (str/join ", " (map :def unreached)) " did not run, so the numbers below are partial."))])
     (when note [:p {:class "banner"} note])
     (when since
       [:p {:class "banner"} "This run planted bugs only in the lines changed since " [:code since]
        ". Code that the change affects only through other functions was not checked."])
     [:p {:class "eyebrow"} "Mutation report · " [:code (str target)] " · " [:code file]]
     [:h1 (if (pos? scored)
            (str "The tests caught " killed " of " (plural scored "planted bug" "planted bugs") " (" (mutate/percent score) ")")
            "No planted bug counts toward the score")]
     (if (seq cards)
       (list [:p {:class "lead"} (str (plural gaps "test gap" "test gaps") " in "
                                      (plural (count cards) "function" "functions") ". Work down this list:")]
             [:ol {:class "todo"}
              (for [{:keys [function anchor gaps]} cards]
                [:li [:a {:href (str "#" anchor)} [:code function]]
                 (if (never-run? gaps)
                   ": no test calls it, "
                   (str ": " (plural (count gaps) "gap" "gaps") ", "))
                 [:span {:class "where"} (line-range (map :row gaps))]])])
       [:p {:class "lead"} (if (pos? (:mutants totals))
                             "No test gaps: every planted bug the tests ran made a test fail."
                             (if (seq ignored)
                               "No planted bugs ran: a skip marker left out every one in scope."
                               "No planted bugs: nothing in scope could be changed."))])
     (let [counts (remove (comp zero? first) [[no-coverage "Never run (no test calls this code)"]
                                              [timeout "Timed out (left out of the score)"]
                                              [unviable "Did not compile (left out of the score)"]
                                              [(count ignored) "Ignored with a skip marker (left out of the score)"]])]
       (when (seq counts)
         [:div {:class "strip"} (for [[n label] counts] [:span (str label ": " n)])]))
     [:p {:class "meta"} "The tool changed " [:code file] " one small step at a time and ran "
      [:code (str/join ", " test-nss)]
      (str " each time. Each change is a planted bug (other tools say mutant). A test gap is a planted bug that no"
           " test caught, either because the tests ran and passed or because no test calls that code.")]]))

(defn page
  "Return the HTML page of a run's `summary`, for `target` loaded from `file` with `source`, tested by `test-nss`.

  `test-names` lists every test, for a definition that ran them all. Test names are shown without
  their namespace when they all share one. A test reaches a mutant when it ran for it: every
  test its definition ran when the mutant survived, and only the killing test when one failed,
  since the run stops there. A mutant with no coverage, that timed out, or that did not load
  reaches none."
  [{:keys [mutants definitions]
    :as summary}
   {:keys [target source test-names]
    :as run}]
  (let [lines (str/split-lines source)
        test-namespace #(first (str/split % #"/"))
        named (concat test-names
                      (mapcat #(when (sequential? (:tests %)) (:tests %)) definitions)
                      (keep :killed-by mutants))
        short-name (if (>= 1 (->> named (map test-namespace) distinct count)) #(str/replace % #"^[^/]*/" "") identity)
        mutants (map #(update % :killed-by (fn [test-name] (some-> test-name short-name))) mutants)
        tests-by-def (into {}
                           (map (fn [{function :def
                                      :keys [tests]}]
                                  [function (map short-name (if (= :every tests) test-names tests))]))
                           definitions)
        tests-of (fn [{:keys [outcome killed-by]
                       :as mutant}]
                   (case outcome
                     :survived (get tests-by-def (:def mutant))
                     :killed (if killed-by [killed-by] [])
                     []))
        by-def (group-by :def mutants)
        cards (->> (map :def definitions)
                   (keep (fn [function]
                           (let [function-mutants (get by-def function)
                                 gaps (filter gap? function-mutants)]
                             (when (seq gaps)
                               {:function function
                                :mutants function-mutants
                                :gaps gaps
                                :tests (get tests-by-def function)}))))
                   (sort-by (juxt (comp - count :gaps) (comp #(apply min %) #(map :row %) :gaps)))
                   ; mutate: skip inc: an anchor only needs to be unique
                   (map-indexed #(assoc %2 :anchor (str "fn-" (inc %1)))))]
    (str "<!doctype html>\n"
         (h/html
          [:html {:lang "en"}
           [:head
            [:meta {:charset "utf-8"}]
            [:meta {:name "viewport"
                    :content "width=device-width, initial-scale=1"}]
            [:title (str target " mutation report")]
            [:style (h/raw css)]]
           [:body
            [:main
             (header run summary cards)
             [:section
              [:h2 "Test gaps"]
              [:p {:class "intro"} "Each gap is a planted bug that no test caught: a real bug of that shape could hide"
               " there today. To close one, add or sharpen a test so that it fails with the planted bug."]
              [:p {:class "intro"} "Test the behavior the code is for, not the planted bug itself: a test written only"
               " to catch one swap tends to break on every refactor. Several gaps in one function usually point to"
               " one missing case."]
              (if (seq cards)
                (map #(function-card lines target %) cards)
                [:p {:class "ok"} "Nothing to fix."])]
             [:section
              [:h2 "By function"]
              [:div {:class "key"}
               (for [[class label n] [["c" "Caught" (:killed (:totals summary))]
                                      ["m" "Not caught" (:survived (:totals summary))]
                                      ["u" "Never run" (:no-coverage (:totals summary))]]
                     :when (pos? n)]
                 [:span [:i {:class (str "sw " class)}] label])]
              (function-table definitions (into {} (map (juxt :function :anchor)) cards))]
             [:section
              [:h2 "By test"]
              [:p {:class "note"} "A test reaches a planted bug when it runs for the changed function. Tests that"
               " reach a gap and still pass are the ones to sharpen."]
              (test-table mutants tests-of)]
             (ignored-table (:ignored summary))
             (source-view lines mutants (:ignored summary))
             (numbers-table summary)
             terms]
            [:script (h/raw script)]]]))))
