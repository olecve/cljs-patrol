(ns cljs-patrol.core
  "Entry point for the cljs-patrol CLI tool.

  Detects unused re-frame subscriptions, events, and Spade style declarations via
  static analysis of ClojureScript source files. Exits with code 1 when unused
  code is found, making it suitable for use in CI pipelines."
  (:gen-class)
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.css-order :as css-order]
   [cljs-patrol.groups.css-order.orders :as orders]
   [cljs-patrol.groups.docstrings :as docstrings]
   [cljs-patrol.groups.idioms :as idioms]
   [cljs-patrol.groups.layout :as layout]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.react-testing-library-cljs :as react-testing-library-cljs]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.groups.tests :as tests]
   [cljs-patrol.groups.typography :as typography]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporters.console :as console]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [cljs-patrol.reporters.html :as html-reporter]
   [cljs-patrol.reporters.markdown :as md-reporter]
   [cljs-patrol.severity :as severity]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]))

(defn- assemble-groups
  "Build every rule group from `config`.

  `clj?` is the experimental `.clj` mode: it reaches only `docstrings`, `tests`,
  `layout` and `idioms`, whose rules are about Clojure the language rather than about ClojureScript, and
  leaves every group that reads Hiccup, re-frame or Spade on `.cljs`/`.cljc` alone."
  [config clj?]
  [re-frame/group
   spade/group
   reagent/group
   typography/group
   (a11y/make-group (get config :a11y))
   (docstrings/make-group (get config :docstrings) clj?)
   (css-order/make-group (get config :css-order))
   (tests/make-group (get config :tests) clj?)
   react-testing-library-cljs/group
   (layout/make-group (get config :layout) clj?)
   (idioms/make-group (get config :idioms) clj?)])

(defn- experimental-clj?
  "Resolve the experimental `.clj` mode from CLI options and config.

  Tested on `some?` rather than truthiness: the flag is absent as nil and written out as
  false, and only the first of those may fall through to the config value, or
  `--no-experimental-clj` could never turn a configured true back off."
  [options config]
  (boolean (if (some? (:experimental-clj options))
             (:experimental-clj options)
             (:experimental-clj config))))

(defn- clj-reading-groups [enabled-groups]
  (filter #(contains? (group/file-extensions %) ".clj") enabled-groups))

(defn- filter-groups [all-groups {:keys [disable only]}]
  (cond
    only (filter #(contains? only (group/group-id %)) all-groups)
    (seq disable) (remove #(contains? disable (group/group-id %)) all-groups)
    :else all-groups))

(def cli-options
  "The CLI option table.

  Public because tools.cli derives each option's id from its string, so the only thing
  tying a flag to the key the rest of the code reads is text a test should be able to see."
  [[nil "--only GROUPS" "Enable only these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--disable GROUPS" "Disable these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--output FORMAT" "Output format: html, edn, or markdown"
    :parse-fn keyword]
   [nil "--files FILES" "Limit results to these files (comma-separated)"
    :parse-fn #(str/split % #",")]
   [nil "--baseline-write" "Write current issues to baseline file and exit 0"]
   [nil "--allow-new" "Let --baseline-write add issues the existing baseline does not have"]
   [nil "--baseline" "Compare against baseline; exit 1 only on new issues"]
   [nil "--strict-baseline" "Also fail if baseline issues are no longer present"]
   [nil "--quiet-baseline" "Only print new issues, suppress baseline issues"]
   [nil "--fail-on TIERS_OR_RULES"
    "Comma-separated list of tiers (bugs/deprecations/cleanup), rule keys, or 'all'"]
   [nil "--css-order ORDER"
    (str "Property-order table for the css-order group: "
         (str/join ", " (map name orders/names)) " (default " (name orders/default-order) ")")
    :parse-fn keyword]
   [nil "--[no-]experimental-clj" "Experimental: also read .clj files (docstrings, tests, layout and idioms groups only)"]
   [nil "--list-rules" "Print all rules grouped by tier and exit"]
   ["-h" "--help"]])

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
  (doseq [[rule-group result] (map vector enabled-groups group-results)]
    (doseq [[label issue-count] (group/summary-lines rule-group result)]
      (println (format "  %-30s %d" label issue-count)))))

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
             (some (fn [[rule-group result]] (group/failed? rule-group result))
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

(defn- analyze-groups [source-dir parsed-data enabled-groups]
  {:source-dir source-dir
   :group-results (mapv (fn [group]
                          (->> parsed-data
                               (group/analyze group)
                               (severity/annotate-tiers group)))
                        enabled-groups)})

(defn run-all
  "Analyze each of source-dirs with enabled-groups, return one {:source-dir … :group-results [...]} per directory.
  Each directory is judged on its own findings, while `:usages-in-run` hands every group the
  usages of all of them, for a rule whose evidence may sit in another directory of the run."
  [source-dirs enabled-groups]
  (let [parsed (mapv #(parser/analyze-project % enabled-groups) source-dirs)
        usages-in-run (vec (mapcat :usages parsed))]
    (mapv (fn [source-dir {:keys [declarations dynamic-sites usages]}]
            (analyze-groups source-dir
                            {:declarations declarations
                             :dynamic-sites dynamic-sites
                             :usages usages
                             :usages-in-run usages-in-run}
                            enabled-groups))
          source-dirs
          parsed)))

(defn run
  "Analyze source-dir with enabled-groups, return {:source-dir source-dir :group-results [...]}."
  [source-dir enabled-groups]
  (first (run-all [source-dir] enabled-groups)))

(defn baseline-write-refusal
  "Decide whether --baseline-write must leave the baseline at `path` as it is.
  Returns nil to write `found`, else {:error message} for a baseline that cannot be compared
  or {:added identities} for the findings it lacks. `allow-new` writes regardless."
  [path found allow-new]
  (when-not allow-new
    (let [{:keys [added error]
           :as check} (baseline/find-added-identities path found)]
      (when (or error (seq added))
        check))))

(defn console-findings
  "Shape every finding of run-results as a console row, labeled by `severity-of`.
  It receives {:group :source-dir :rule :items :item}, items being all of that rule's findings."
  [enabled-groups run-results severity-of]
  (for [{:keys [source-dir group-results]} run-results
        [rule-group result] (map vector enabled-groups group-results)
        [rule-key items] result
        :when (sequential? items)
        item items]
    (console/finding rule-key
                     (severity-of {:group rule-group
                                   :item item
                                   :items items
                                   :rule rule-key
                                   :source-dir source-dir})
                     item)))

(defn- docs-by-rule [enabled-groups]
  (into {} (mapcat group/docs-urls) enabled-groups))

(defn standalone-severity
  "Label a finding as the exit code treats it: error when it fails the run, info for a rule that never does."
  [fail-on-rules]
  (fn [{:keys [group rule items]}]
    (cond
      (contains? fail-on-rules rule) "error"
      (nil? (get (group/rule->tier group) rule)) "info"
      (seq fail-on-rules) "warning"
      (group/failed? group {rule items}) "error"
      :else "warning")))

(defn baseline-severity
  "Label a new finding as baseline-failed? treats it, and any other as baseline."
  [new-issues fail-on-rules]
  (fn [{:keys [source-dir rule item]}]
    (cond
      (not (contains? new-issues (baseline/issue->identity rule item source-dir))) "baseline"
      (or (empty? fail-on-rules) (contains? fail-on-rules rule)) "error"
      :else "warning")))

(defn baseline-console-rows
  "Return {:findings rows to print :present rows in the baseline}; quiet leaves the baselined rows out of :findings."
  [enabled-groups run-results new-issues fail-on-rules quiet?]
  (let [findings (console-findings enabled-groups run-results (baseline-severity new-issues fail-on-rules))
        baselined? #(= "baseline" (:severity %))]
    {:findings (cond->> findings
                 quiet? (remove baselined?))
     :present (count (filter baselined? findings))}))

(defn- refuse-baseline-write! [enabled-groups run-results path added]
  (console/print-report (:findings (baseline-console-rows enabled-groups run-results added nil true))
                        {:docs (docs-by-rule enabled-groups)})
  (println (str "\nRefusing to write " path ": the findings above are not in it."
                " Fix them, or pass --allow-new to add them."))
  (System/exit 1))

(defn- run-baseline-write! [enabled-groups run-results opts dirs rule->tier]
  (let [identities (baseline/collect-identities run-results)
        path (baseline/resolve-baseline-path (:baseline-path opts) dirs)]
    (when-let [{:keys [added error]} (baseline-write-refusal path identities (:allow-new opts))]
      (if error
        (do
          (println (str "Error: " error))
          (System/exit 1))
        (refuse-baseline-write! enabled-groups run-results path added)))
    (baseline/write-baseline path identities rule->tier)
    (println (str "Wrote baseline with " (count identities) " entries to " path))
    (System/exit 0)))

(defn- run-baseline-compare! [enabled-groups run-results opts dirs rule->tier]
  (let [path (baseline/resolve-baseline-path (:baseline-path opts) dirs)
        {:keys [ok error]} (baseline/read-baseline path)]
    (when error
      (println (str "Error: " error))
      (System/exit 1))
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
        (System/exit 1))
      (case (:output opts)
        :edn (edn-reporter/print-baseline-report
              dirs new-issues present fixed exit-code
              fail-on-rules rule->tier)
        :html (let [blocking-count (count (filter #(contains? fail-on-rules (:rule %)) new-issues))
                    warning-count (- (count new-issues) blocking-count)]
                (html-reporter/write-baseline-report
                 enabled-groups run-results "report.html"
                 new-issues (count fixed)
                 fail-on-rules blocking-count warning-count)
                (println "Report written to report.html"))
        (let [{:keys [findings present]} (baseline-console-rows enabled-groups run-results new-issues fail-on-rules
                                                                (:quiet-baseline opts))]
          (console/print-report findings
                                {:baseline {:fixed (count fixed)
                                            :present present
                                            :strict? (boolean (:strict-baseline opts))}
                                 :docs (docs-by-rule enabled-groups)})))
      (System/exit exit-code))))

(defn- run-standalone! [enabled-groups run-results opts dirs]
  (let [fail-on-rules (:fail-on-rules opts)
        any-failed? (standalone-failed? {:enabled-groups enabled-groups
                                         :run-results run-results
                                         :fail-on-rules fail-on-rules})]
    (case (:output opts)
      :html (do
              (html-reporter/write-report enabled-groups run-results "report.html" fail-on-rules)
              (println "Report written to report.html")
              (doseq [{:keys [group-results]} run-results]
                (print-summary enabled-groups group-results)))
      :edn (edn-reporter/print-report enabled-groups dirs run-results fail-on-rules)
      :markdown (md-reporter/print-report enabled-groups dirs run-results)
      (console/print-report (console-findings enabled-groups run-results (standalone-severity fail-on-rules))
                            {:docs (docs-by-rule enabled-groups)}))
    (System/exit (if any-failed? 1 0))))

(defn -main [& args]
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
    (when (and (:allow-new options) (not (:baseline-write options)))
      (println "Error: --allow-new only applies to --baseline-write.")
      (System/exit 1))
    (when (and (:baseline-write options) (:files options))
      (println "Error: --baseline-write cannot be used with --files (would write a partial baseline).")
      (System/exit 1))
    (let [config (cond-> (baseline/read-config)
                   (:css-order options) (assoc-in [:css-order :order] (:css-order options)))
          base-opts (baseline/merge-config
                     config
                     (select-keys options [:only :disable :output :files
                                           :baseline-write :allow-new :baseline :strict-baseline
                                           :quiet-baseline]))
          dirs arguments
          clj? (experimental-clj? options config)
          all-groups (assemble-groups config clj?)
          enabled-groups (filter-groups all-groups base-opts)
          fail-on-input (or (:fail-on options) (:fail-on config))
          rule->tier (severity/collect-rule->tier enabled-groups)
          {:keys [ok error]} (severity/parse-fail-on fail-on-input rule->tier)
          _ (when error
              (println (str "Error: " error))
              (System/exit 1))
          opts (assoc base-opts :fail-on-rules (or ok #{}))]
      (when (:list-rules options)
        (println (severity/format-rules (severity/list-rules enabled-groups)))
        (System/exit 0))
      (when (empty? dirs)
        (println "Error: no source directories specified")
        (System/exit 1))
      (when-let [missing (seq (fs/missing-dirs dirs))]
        (println (str "Error: no such path: " (str/join ", " missing)))
        (System/exit 1))
      (when clj?
        (binding [*out* *err*]
          (if-let [reading (seq (clj-reading-groups enabled-groups))]
            (println (str "Experimental: reading .clj files with "
                          (str/join " and " (map (comp name group/group-id) reading))
                          "."))
            (println (str "WARN: --experimental-clj does nothing here. No enabled group reads .clj; "
                          "the groups that can are docstrings, tests, layout and idioms.")))))
      (let [run-results (cond-> (run-all dirs enabled-groups)
                          (:files opts) (filter-run-results (:files opts)))]
        (cond
          (:baseline-write opts)
          (run-baseline-write! enabled-groups run-results opts dirs rule->tier)

          (:baseline opts)
          (run-baseline-compare! enabled-groups run-results opts dirs rule->tier)

          :else
          (run-standalone! enabled-groups run-results opts dirs))))))
