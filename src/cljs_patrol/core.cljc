(ns cljs-patrol.core
  "Entry point for the cljs-patrol CLI tool.

  Detects unused re-frame subscriptions, events, and Spade style declarations via
  static analysis of ClojureScript source files. Exits with code 1 when unused
  code is found, making it suitable for use in CI pipelines."
  #?@(:clj [(:gen-class)])
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.format :refer [formatf]]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.docstrings :as docstrings]
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
   [clojure.string :as str]))

(defn- exit! [code]
  #?(:clj (System/exit code)
     :cljs (js/process.exit code)))

(defn- assemble-groups [config]
  [re-frame/group
   spade/group
   reagent/group
   typography/group
   (a11y/make-group (get config :a11y))
   docstrings/group])

(defn- filter-groups [all-groups {:keys [disable only]}]
  (cond
    only (filter #(contains? only (group/group-id %)) all-groups)
    (seq disable) (remove #(contains? disable (group/group-id %)) all-groups)
    :else all-groups))

(def ^:private options-spec
  "Every supported CLI flag. `:kind` is `:flag` (boolean, no value) or
  `:value` (consumes the next argv slot, optionally through `:parse-fn`)."
  [{:long "--only"
    :arg "GROUPS"
    :kind :value
    :key :only
    :parse-fn #(set (map keyword (str/split % #",")))
    :help "Enable only these groups (comma-separated)"}
   {:long "--disable"
    :arg "GROUPS"
    :kind :value
    :key :disable
    :parse-fn #(set (map keyword (str/split % #",")))
    :help "Disable these groups (comma-separated)"}
   {:long "--output"
    :arg "FORMAT"
    :kind :value
    :key :output
    :parse-fn keyword
    :help "Output format: html, edn, or markdown"}
   {:long "--files"
    :arg "FILES"
    :kind :value
    :key :files
    :parse-fn #(str/split % #",")
    :help "Limit results to these files (comma-separated)"}
   {:long "--baseline-write"
    :kind :flag
    :key :baseline-write
    :help "Write current issues to baseline file and exit 0"}
   {:long "--baseline"
    :kind :flag
    :key :baseline
    :help "Compare against baseline; exit 1 only on new issues"}
   {:long "--strict-baseline"
    :kind :flag
    :key :strict-baseline
    :help "Also fail if baseline issues are no longer present"}
   {:long "--quiet-baseline"
    :kind :flag
    :key :quiet-baseline
    :help "Only print new issues, suppress baseline issues"}
   {:long "--fail-on"
    :arg "TIERS_OR_RULES"
    :kind :value
    :key :fail-on
    :help "Comma-separated list of tiers (bugs/deprecations/cleanup), rule keys, or 'all'"}
   {:long "--list-rules"
    :kind :flag
    :key :list-rules
    :help "Print all rules grouped by tier and exit"}
   {:long "--help"
    :short "-h"
    :kind :flag
    :key :help
    :help "Print this help and exit"}])

(defn- spec-by-flag [flag]
  (some (fn [s] (when (or (= flag (:long s)) (= flag (:short s))) s))
        options-spec))

(defn- summary-str []
  (str/join
   "\n"
   (for [{:keys [long arg help]} options-spec]
     (str "  " long
          (when arg (str " " arg))
          "  " help))))

(defn parse-opts
  "Small tools.cli-lookalike returning {:options :arguments :errors :summary}."
  [args]
  (loop [remaining args
         options {}
         arguments []
         errors []]
    (if (empty? remaining)
      {:options options
       :arguments arguments
       :errors errors
       :summary (summary-str)}
      (let [arg (first remaining)
            tail (rest remaining)]
        (cond
          (str/starts-with? arg "--")
          (if-let [{:keys [kind key parse-fn]} (spec-by-flag arg)]
            (case kind
              :flag (recur tail (assoc options key true) arguments errors)
              :value (if (empty? tail)
                       (recur []
                              options
                              arguments
                              (conj errors (str "Missing value for " arg)))
                       (recur (rest tail)
                              (assoc options key ((or parse-fn identity) (first tail)))
                              arguments
                              errors)))
            (recur tail options arguments (conj errors (str "Unknown option: " arg))))

          (= arg "-h")
          (recur tail (assoc options :help true) arguments errors)

          :else
          (recur tail options (conj arguments arg) errors))))))

(defn- filter-result [result files-set]
  (->> result
       (map (fn [[k v]]
              [k (if (sequential? v)
                   (filterv #(contains? files-set (fs/absolute-path (:file %))) v)
                   v)]))
       (into {})))

(defn- filter-run-results [run-results files]
  (let [files-set (set (map fs/absolute-path files))]
    (mapv (fn [rr]
            (update rr :group-results #(mapv (fn [r] (filter-result r files-set)) %)))
          run-results)))

(defn- print-summary [enabled-groups group-results]
  (println "\n=== SUMMARY ===")
  (doseq [[g r] (map vector enabled-groups group-results)]
    (doseq [[label cnt] (group/summary-lines g r)]
      (println (formatf "  %-30s %d" label cnt)))))

(defn- any-rule-issue?
  "True if `run-results` contains any non-empty items vector keyed by a rule in `rule-set`."
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

(defn- run-results->results
  "Flatten run-results into a seq of group result maps."
  [run-results]
  (mapcat :group-results run-results))

(defn- run-baseline-write! [run-results opts dirs]
  (let [identities (baseline/collect-identities run-results)
        path (baseline/resolve-baseline-path (:baseline-path opts) dirs)]
    (baseline/write-baseline path identities)
    (println (str "Wrote baseline with " (count identities) " issues to " path))
    (exit! 0)))

(defn- print-baseline-console-summary [{:keys [new-issues present fixed fail-on-rules]}]
  (println (formatf "\nFound %d issues: %d new, %d in baseline, %d fixed."
                    (+ (count new-issues) (count present))
                    (count new-issues) (count present) (count fixed)))
  (when (seq fail-on-rules)
    (let [blocking (count (filter #(contains? fail-on-rules (:rule %)) new-issues))
          warning (- (count new-issues) blocking)]
      (println (formatf "New: %d blocking, %d warnings." blocking warning))))
  (when (seq fixed)
    (println (formatf "%d baseline issues no longer present - consider running --baseline-write to refresh."
                      (count fixed)))))

(defn- write-baseline-html-report! [enabled-groups run-results new-issues fixed-count fail-on-rules]
  (let [blocking-count (count (filter #(contains? fail-on-rules (:rule %)) new-issues))
        warning-count (- (count new-issues) blocking-count)]
    (html-reporter/write-baseline-report
     enabled-groups run-results "report.html"
     new-issues fixed-count
     fail-on-rules blocking-count warning-count)
    (println "Report written to report.html")))

(defn- run-baseline-compare! [enabled-groups run-results opts dirs rule->tier]
  (let [path (baseline/resolve-baseline-path (:baseline-path opts) dirs)
        {:keys [ok error]} (baseline/read-baseline path)]
    (when error
      (println (str "Error: " error))
      (exit! 1))
    (let [found (baseline/collect-identities run-results)
          {new-issues :new
           present :present
           fixed :fixed} (baseline/diff-baseline ok found)
          fail-on-rules (:fail-on-rules opts)
          exit-code (if (baseline-failed? {:new-issues new-issues
                                           :fixed-issues fixed
                                           :fail-on-rules fail-on-rules
                                           :strict-baseline (:strict-baseline opts)})
                      1 0)]
      (when (= :markdown (:output opts))
        (println "Error: --output markdown is not supported with --baseline.")
        (exit! 1))
      (case (:output opts)
        :edn (edn-reporter/print-baseline-report
              dirs new-issues present fixed exit-code
              fail-on-rules rule->tier)
        :html (write-baseline-html-report! enabled-groups run-results new-issues (count fixed) fail-on-rules)
        (do
          (doseq [{:keys [source-dir group-results]} run-results]
            (doseq [result group-results]
              (console/report-with-baseline
               result new-issues
               {:quiet? (:quiet-baseline opts)
                :source-dir source-dir
                :fail-on-rules fail-on-rules})))
          (print-baseline-console-summary
           {:new-issues new-issues
            :present present
            :fixed fixed
            :fail-on-rules fail-on-rules})))
      (exit! exit-code))))

(defn- write-standalone-html-report! [enabled-groups run-results fail-on-rules]
  (html-reporter/write-report enabled-groups run-results "report.html" fail-on-rules)
  (println "Report written to report.html")
  (doseq [{:keys [group-results]} run-results]
    (print-summary enabled-groups group-results)))

(defn- run-standalone! [enabled-groups run-results opts dirs]
  (let [fail-on-rules (:fail-on-rules opts)
        any-failed? (standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules fail-on-rules})]
    (case (:output opts)
      :html (write-standalone-html-report! enabled-groups run-results fail-on-rules)
      :edn (edn-reporter/print-report enabled-groups dirs run-results fail-on-rules)
      :markdown (md-reporter/print-report enabled-groups dirs run-results)
      (do
        (doseq [{:keys [group-results]} run-results]
          (doseq [r group-results]
            (console/report r fail-on-rules))
          (print-summary enabled-groups group-results))
        (when (seq fail-on-rules)
          (let [{:keys [blocking warning]} (severity/count-by-fail-on
                                            (run-results->results run-results)
                                            fail-on-rules)]
            (println (formatf "\n%d blocking, %d warnings." blocking warning))))))
    (exit! (if any-failed? 1 0))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args)]
    (when (or (:help options) (empty? args))
      (println "Usage: cljs-patrol [options] <source-dir> [<source-dir> ...]")
      (println)
      (println "  Detects unused re-frame subscriptions, events, and Spade style declarations.")
      (println "  Exits with code 1 when blocking issues are found. Use --fail-on to narrow which")
      (println "  rules block CI, and --baseline to ignore pre-existing issues.")
      (println)
      (println "Options:")
      (println summary)
      (exit! 0))
    (when (seq errors)
      (doseq [e errors] (println e))
      (exit! 1))
    (when (and (:baseline-write options) (:baseline options))
      (println "Error: --baseline-write and --baseline are mutually exclusive.")
      (exit! 1))
    (when (and (:baseline-write options) (:files options))
      (println "Error: --baseline-write cannot be used with --files (would write a partial baseline).")
      (exit! 1))
    (let [config (baseline/read-config)
          base-opts (baseline/merge-config
                     config
                     (select-keys options [:only :disable :output :files
                                           :baseline-write :baseline :strict-baseline
                                           :quiet-baseline]))
          dirs arguments
          all-groups (assemble-groups config)
          enabled-groups (filter-groups all-groups base-opts)
          fail-on-input (or (:fail-on options) (:fail-on config))
          rule->tier (severity/collect-rule->tier enabled-groups)
          {:keys [ok error]} (severity/parse-fail-on fail-on-input rule->tier)
          _ (when error
              (println (str "Error: " error))
              (exit! 1))
          opts (assoc base-opts :fail-on-rules (or ok #{}))]
      (when (:list-rules options)
        (println (severity/format-rules (severity/list-rules enabled-groups)))
        (exit! 0))
      (when (empty? dirs)
        (println "Error: no source directories specified")
        (exit! 1))
      (let [run-results (cond-> (mapv #(run % enabled-groups) dirs)
                          (:files opts) (filter-run-results (:files opts)))]
        (cond
          (:baseline-write opts)
          (run-baseline-write! run-results opts dirs)

          (:baseline opts)
          (run-baseline-compare! enabled-groups run-results opts dirs rule->tier)

          :else
          (run-standalone! enabled-groups run-results opts dirs))))))
