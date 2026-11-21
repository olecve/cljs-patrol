(ns cljs-patrol.dev.mutate.report
  "Summarize a run, print its report to the console, and turn it into an exit code."
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(defn apply-command
  "Return the command that writes `mutant` of `target` into its file, as the reports show it, quoted for a shell."
  [target {:keys [row col from]}]
  (str "clojure -M:test:mutate " target " --apply '" row ":" col ":" from "'"))

(defn test-name
  "Name a test var as the reports do: namespace/name."
  [test-var]
  (str (symbol test-var)))

(defn- outcome-key [outcome]
  (case outcome
    :passed :survived
    :failed :killed
    outcome))

(defn- outcome-counts [results]
  (merge {:killed 0
          :survived 0
          :no-coverage 0
          :unviable 0
          :timeout 0}
         (frequencies (map (comp outcome-key :outcome) results))))

(defn summarize
  "Return the report of a run: each definition's counts, the definitions not mutated or with no coverage, and every outcome.

  A definition one mutant of which a test killed or timed out is reached by the tests, so its
  other passing mutants survived, even when the coverage pass saw no call. A mutant that threw
  while it loaded shows nothing about the tests. The score is killed over killed,
  survived and no-coverage mutants; the covered score leaves the mutants no test reaches out, so it
  shows how well the tests check the code they run. Each definition names the tests its mutants
  ran, from its plan in `plans`, or :every when they ran every test."
  [{:keys [defs-in-scope loaded-once const-defs other-forms unreached results ignored stopped note plans]}]
  (let [reached (set (keep #(when (and (contains? #{:failed :timeout} (:outcome %)) (not (:at-load? %))) (:def %))
                           results))
        results (map #(cond-> %
                        (and (= :no-coverage (:outcome %)) (contains? reached (:def %))) (assoc :outcome :passed))
                     results)
        by-def (group-by :def results)
        counts (outcome-counts results)
        covered (+ (:killed counts) (:survived counts))
        scored (+ covered (:no-coverage counts))
        unreached-counts (frequencies (map :def unreached))]
    {:definitions (vec (for [def-name (distinct defs-in-scope)
                             :let [def-results (get by-def def-name)]
                             :when (seq def-results)]
                         (assoc (outcome-counts def-results)
                                :def def-name
                                :mutants (count def-results)
                                :tests (let [{:keys [kind tests]} (get plans def-name)]
                                         (cond
                                           (= :subset kind) (->> tests (map test-name) sort vec)
                                           kind :every
                                           :else [])))))
     :not-mutated (->> defs-in-scope
                       (remove (set (map :def ignored)))
                       (remove #(or (contains? by-def %) (contains? unreached-counts %)))
                       distinct
                       vec)
     :loaded-once (vec (distinct loaded-once))
     :const-defs (vec (distinct const-defs))
     :other-forms (vec (distinct other-forms))
     :unreached (vec (for [def-name (distinct (map :def unreached))]
                       {:def def-name
                        :mutants (get unreached-counts def-name)}))
     :not-covered (vec (for [def-name (distinct defs-in-scope)
                             :when (some #(= :no-coverage (:outcome %)) (get by-def def-name))]
                         def-name))
     :note note
     :tests-run (update-vals (or plans {}) :kind)
     :totals (assoc counts :mutants (count results))
     :score (when (pos? scored) (double (/ (:killed counts) scored)))
     :covered-score (when (pos? covered) (double (/ (:killed counts) covered)))
     :mutants (mapv #(-> %
                         (select-keys [:def :row :col :from :swap :at-load?])
                         (assoc :to (pr-str (:to %))
                                :outcome (outcome-key (:outcome %)))
                         (cond-> (:killed-by %) (assoc :killed-by (test-name (:killed-by %)))))
                    results)
     :ignored (mapv #(-> %
                         (select-keys [:def :row :col :from :swap])
                         (assoc :to (pr-str (:to %))
                                :reason (:skip-reason %)))
                    ignored)
     :stopped stopped}))

(defn- print-list [title names]
  (when (seq names)
    (println (str "\n" title ": " (str/join ", " names)))))

(defn percent
  "Write a ratio as a whole percentage, or n/a when there is none."
  [ratio]
  (if ratio (format "%.0f%%" (* 100.0 ratio)) "n/a"))

(defn print-report
  "Print the console report of `summary`: the counts, what was not mutated, each survivor with its line, and the score.

  `source-lines` are the target's lines, `since` is the ref of `-s` or nil, and `target` names the
  namespace for the example --apply command."
  [{:keys [definitions not-mutated not-covered loaded-once const-defs other-forms unreached totals score
           covered-score mutants ignored stopped note]}
   source-lines
   since
   target]
  (println)
  (pprint/print-table [:def :mutants :killed :survived :no-coverage :unviable :timeout] definitions)
  (print-list "No coverage, no test calls them through their var" not-covered)
  (print-list (str "Not mutated, no operator " (if since "on the changed lines" "applies")) not-mutated)
  (print-list "Not mutated, loaded once" loaded-once)
  (print-list "Not mutated, ^:const, callers copy its value" const-defs)
  (print-list "Not mutated, not a definition" other-forms)
  (print-list "Not reached, the run stopped early" (map #(str (:def %) " (" (:mutants %) " mutants)") unreached))
  (doseq [[outcome title] [[:survived "Survived"]
                           [:unviable "Unviable, could not be read or compiled"]
                           [:timeout "Timed out"]]
          :let [shown (filter #(= outcome (:outcome %)) mutants)]
          :when (seq shown)]
    (println (str "\n" title ":"))
    (doseq [{:keys [def row col from to]} shown]
      (println (format "  %d:%d  %s -> %s  in %s" row col from to def))
      (println (str "      " (str/trim (nth source-lines (dec row) ""))))))
  ; mutate: skip first: any survivor makes a fine example
  (when-let [mutant (first (filter #(= :survived (:outcome %)) mutants))]
    (println (str "\nTo watch a new test fail on a survivor, apply it to the file, such as:\n"
                  "  " (apply-command target mutant))))
  (when (seq ignored)
    (println "\nIgnored, with a skip marker:")
    (doseq [{:keys [def row col from to reason]} ignored]
      (println (format "  %d:%d  %s -> %s  in %s: %s" row col from to def reason))))
  (println (format "\n%d mutants: %d killed, %d survived, %d no coverage, %d unviable, %d timed out."
                   (:mutants totals) (:killed totals) (:survived totals) (:no-coverage totals) (:unviable totals)
                   (:timeout totals)))
  (println (str "Score " (percent score) ", covered score " (percent covered-score) "."))
  (when note
    (println (str "\nNote: " note)))
  (when stopped
    (println (str "\nStopped early: " stopped))))

(defn exit-code
  "Return 2 when the run stopped early, 1 when a mutant survived or had no coverage, and 0 otherwise."
  [{:keys [totals stopped]}]
  (cond
    stopped 2
    (pos? (+ (:survived totals 0) (:no-coverage totals 0))) 1
    :else 0))
