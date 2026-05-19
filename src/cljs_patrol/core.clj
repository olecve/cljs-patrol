(ns cljs-patrol.core
  "Entry point for the cljs-patrol CLI tool.

  Detects unused re-frame subscriptions, events, and Spade style declarations via
  static analysis of ClojureScript source files. Exits with code 1 when unused
  code is found, making it suitable for use in CI pipelines."
  (:gen-class)
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.groups.typography :as typography]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporters.console :as console]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [cljs-patrol.reporters.html :as html-reporter]
   [cljs-patrol.reporters.markdown :as md-reporter]
   [cljs-patrol.severity :as severity]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]))

(def ^:private all-groups [re-frame/group spade/group reagent/group typography/group])

(defn- filter-groups [{:keys [disable only]}]
  (cond
    only (filter #(contains? only (group/group-id %)) all-groups)
    (seq disable) (remove #(contains? disable (group/group-id %)) all-groups)
    :else all-groups))

(def ^:private cli-options
  [[nil "--only GROUPS" "Enable only these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--disable GROUPS" "Disable these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--output FORMAT" "Output format: html, edn, or markdown"
    :parse-fn keyword]
   [nil "--files FILES" "Limit results to these files (comma-separated)"
    :parse-fn #(str/split % #",")]
   [nil "--baseline-write" "Write current issues to baseline file and exit 0"]
   [nil "--baseline" "Compare against baseline; exit 1 only on new issues"]
   [nil "--strict-baseline" "Also fail if baseline issues are no longer present"]
   [nil "--quiet-baseline" "Only print new issues, suppress baseline issues"]
   [nil "--fail-on TIERS_OR_RULES"
    "Comma-separated list of tiers (bugs/deprecations/cleanup), rule keys, or 'all'"]
   ["-h" "--help"]])

(defn- abspath [path]
  (.getAbsolutePath (java.io.File. path)))

(defn- filter-result [result files-set]
  (into {} (map (fn [[k v]]
                  [k (if (sequential? v)
                       (filterv #(contains? files-set (abspath (:file %))) v)
                       v)])
                result)))

(defn- filter-run-results [run-results files]
  (let [files-set (set (map abspath files))]
    (mapv (fn [rr]
            (update rr :group-results #(mapv (fn [r] (filter-result r files-set)) %)))
          run-results)))

(defn- print-summary [enabled-groups group-results]
  (println "\n=== SUMMARY ===")
  (doseq [[g r] (map vector enabled-groups group-results)]
    (doseq [[label cnt] (group/summary-lines g r)]
      (println (format "  %-30s %d" label cnt)))))

(defn- any-rule-issue?
  "True if any group-result in run-results has a non-empty vector under a key
  contained in rule-set."
  [run-results rule-set]
  (some (fn [{:keys [group-results]}]
          (some (fn [result]
                  (some (fn [[rule-key items]]
                          (and (contains? rule-set rule-key)
                               (sequential? items)
                               (seq items)))
                        result))
                group-results))
        run-results))

(defn standalone-failed?
  "Decide whether to exit non-zero when not using --baseline.
  When fail-on-rules is empty/nil, falls back to each group's failed? method.
  When fail-on-rules is non-empty, fails iff any issue's rule is in that set."
  [{:keys [enabled-groups run-results fail-on-rules]}]
  (if (seq fail-on-rules)
    (boolean (any-rule-issue? run-results fail-on-rules))
    (boolean
     (some (fn [{:keys [group-results]}]
             (some (fn [[g r]] (group/failed? g r))
                   (map vector enabled-groups group-results)))
           run-results))))

(defn baseline-failed?
  "Return truthy if baseline comparison should cause a non-zero exit.
  - When fail-on-rules is empty/nil, fails on any new issue.
  - When fail-on-rules is set, fails only on new issues whose rule is in that set.
  - When strict-baseline is truthy, additionally fails on fixed issues
    regardless of tier (forces baseline regeneration)."
  [{:keys [new-issues fixed-issues fail-on-rules strict-baseline]}]
  (let [blocking-new (if (seq fail-on-rules)
                       (filter #(contains? fail-on-rules (:rule %)) new-issues)
                       new-issues)]
    (boolean
     (or (seq blocking-new)
         (and strict-baseline (seq fixed-issues))))))

(defn run
  "Analyze source-dir with enabled-groups, return {:source-dir source-dir :group-results [...]}."
  [source-dir enabled-groups]
  (let [{:keys [declarations dynamic-sites usages]} (parser/analyze-project source-dir enabled-groups)
        parsed-data {:declarations declarations
                     :dynamic-sites dynamic-sites
                     :usages usages}
        group-results (mapv (fn [group]
                              (->> parsed-data
                                   (group/analyze group)
                                   (severity/annotate-tiers group)))
                            enabled-groups)]
    {:source-dir source-dir
     :group-results group-results}))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (when (or (:help options) (empty? args))
      (println "Usage: cljs-patrol [options] <source-dir> [<source-dir> ...]")
      (println)
      (println "  Detects unused re-frame subscriptions, events, and Spade style declarations.")
      (println "  Exits with code 1 when blocking issues are found. Use --fail-on to narrow which")
      (println "  rules block CI, and --baseline to ignore pre-existing issues.")
      (println)
      (println "Options:")
      (println summary)
      (System/exit 0))
    (when errors
      (doseq [e errors] (println e))
      (System/exit 1))
    (when (and (:baseline-write options) (:baseline options))
      (println "Error: --baseline-write and --baseline are mutually exclusive.")
      (System/exit 1))
    (when (and (:baseline-write options) (:files options))
      (println "Error: --baseline-write cannot be used with --files (would write a partial baseline).")
      (System/exit 1))
    (let [config (baseline/read-config)
          base-opts (baseline/merge-config
                     config
                     (select-keys options [:only :disable :output :files
                                           :baseline-write :baseline :strict-baseline
                                           :quiet-baseline]))
          dirs arguments
          enabled-groups (filter-groups base-opts)
          fail-on-input (or (:fail-on options) (:fail-on config))
          rule->tier (severity/collect-rule->tier enabled-groups)
          {:keys [ok error]} (severity/parse-fail-on fail-on-input rule->tier)
          _ (when error
              (println (str "Error: " error))
              (System/exit 1))
          opts (assoc base-opts :fail-on-rules (or ok #{}))]
      (when (empty? dirs)
        (println "Error: no source directories specified")
        (System/exit 1))
      (let [run-results (cond-> (mapv #(run % enabled-groups) dirs)
                          (:files opts) (filter-run-results (:files opts)))]
        (cond
          (:baseline-write opts)
          (let [identities (baseline/collect-identities run-results)
                path (baseline/resolve-baseline-path (:baseline-path opts) dirs)]
            (baseline/write-baseline path identities)
            (println (str "Wrote baseline with " (count identities)
                          " issues to " path))
            (System/exit 0))

          (:baseline opts)
          (let [path (baseline/resolve-baseline-path (:baseline-path opts) dirs)
                {:keys [ok error]} (baseline/read-baseline path)]
            (when error
              (println (str "Error: " error))
              (System/exit 1))
            (let [found (baseline/collect-identities run-results)
                  {new-issues :new
                   present :present
                   fixed :fixed} (baseline/diff-baseline ok found)
                  exit-code (if (baseline-failed? {:new-issues new-issues
                                                   :fixed-issues fixed
                                                   :fail-on-rules (:fail-on-rules opts)
                                                   :strict-baseline (:strict-baseline opts)})
                              1 0)]
              (when (= :markdown (:output opts))
                (println "Error: --output markdown is not supported with --baseline.")
                (System/exit 1))
              (case (:output opts)
                :edn (edn-reporter/print-baseline-report
                      dirs new-issues present fixed exit-code
                      (:fail-on-rules opts) rule->tier)
                :html (let [fail-on-rules (:fail-on-rules opts)
                            blocking-count (count (filter #(contains? fail-on-rules (:rule %))
                                                          new-issues))
                            warning-count (- (count new-issues) blocking-count)]
                        (html-reporter/write-baseline-report
                         enabled-groups run-results "report.html"
                         new-issues (count fixed)
                         fail-on-rules blocking-count warning-count)
                        (println "Report written to report.html"))
                (do
                  (doseq [{:keys [source-dir group-results]} run-results]
                    (doseq [result group-results]
                      (console/report-with-baseline
                       result new-issues
                       {:quiet? (:quiet-baseline opts)
                        :source-dir source-dir
                        :fail-on-rules (:fail-on-rules opts)})))
                  (let [fail-on-rules (:fail-on-rules opts)
                        blocking-new (if (seq fail-on-rules)
                                       (filter #(contains? fail-on-rules (:rule %)) new-issues)
                                       new-issues)
                        warning-new (when (seq fail-on-rules)
                                      (remove #(contains? fail-on-rules (:rule %)) new-issues))]
                    (println (format "\nFound %d issues: %d new, %d in baseline, %d fixed."
                                     (+ (count new-issues) (count present))
                                     (count new-issues) (count present) (count fixed)))
                    (when (seq fail-on-rules)
                      (println (format "New: %d blocking, %d warnings."
                                       (count blocking-new) (count warning-new))))
                    (when (seq fixed)
                      (println (format "%d baseline issues no longer present - consider running --baseline-write to refresh."
                                       (count fixed)))))))
              (System/exit exit-code))))

        :else
        (let [any-failed? (standalone-failed? {:enabled-groups enabled-groups
                                               :run-results run-results
                                               :fail-on-rules (:fail-on-rules opts)})]
          (case (:output opts)
            :html (do
                    (html-reporter/write-report enabled-groups run-results "report.html"
                                                (:fail-on-rules opts))
                    (println "Report written to report.html")
                    (doseq [{:keys [group-results]} run-results]
                      (print-summary enabled-groups group-results)))
            :edn (edn-reporter/print-report enabled-groups dirs run-results
                                            (:fail-on-rules opts))
            :markdown (md-reporter/print-report enabled-groups dirs run-results)
            (do
              (doseq [{:keys [group-results]} run-results]
                (doseq [r group-results]
                  (console/report r (:fail-on-rules opts)))
                (print-summary enabled-groups group-results))
              (when (seq (:fail-on-rules opts))
                (let [fail-on-rules (:fail-on-rules opts)
                      counts (reduce (fn [acc {:keys [group-results]}]
                                       (reduce (fn [acc result]
                                                 (reduce-kv
                                                  (fn [acc rule-key items]
                                                    (if (and (sequential? items) (seq items))
                                                      (if (contains? fail-on-rules rule-key)
                                                        (update acc :blocking + (count items))
                                                        (update acc :warning + (count items)))
                                                      acc))
                                                  acc
                                                  result))
                                               acc
                                               group-results))
                                     {:blocking 0
                                      :warning 0}
                                     run-results)]
                  (println (format "\n%d blocking, %d warnings."
                                   (:blocking counts) (:warning counts)))))))
          (System/exit (if any-failed? 1 0)))))))
