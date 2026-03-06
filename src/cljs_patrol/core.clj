(ns cljs-patrol.core
  "Entry point for the cljs-patrol CLI tool.

  Detects unused re-frame subscriptions, events, and Spade style declarations via
  static analysis of ClojureScript source files. Exits with code 1 when unused
  code is found, making it suitable for use in CI pipelines."
  (:gen-class)
  (:require
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [cljs-patrol.reporters.html :as html-reporter]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]))

(def ^:private all-groups [re-frame/group spade/group])

(defn- filter-groups [{:keys [disable only]}]
  (cond
    only (filter #(contains? only (:id %)) all-groups)
    (seq disable) (remove #(contains? disable (:id %)) all-groups)
    :else all-groups))

(def ^:private cli-options
  [[nil "--only GROUPS" "Enable only these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--disable GROUPS" "Disable these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--output FORMAT" "Output format: html or edn"
    :parse-fn keyword]
   [nil "--files FILES" "Limit results to these files (comma-separated)"
    :parse-fn #(str/split % #",")]
   ["-h" "--help"]])

(defn- parse-args
  "Parse CLI args using tools.cli, returning [opts dirs]."
  [args]
  (let [{:keys [options arguments]} (cli/parse-opts args cli-options)]
    [(select-keys options [:only :disable :output :files]) arguments]))

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
    (doseq [[label cnt] ((:summary-lines g) r)]
      (println (format "  %-30s %d" label cnt)))))

(defn run
  "Analyze source-dir with enabled-groups, return {:source-dir source-dir :group-results [...]}."
  [source-dir enabled-groups]
  (let [{:keys [declarations dynamic-sites usages]} (parser/analyze-project source-dir enabled-groups)
        group-results (mapv (fn [g]
                              ((:analyze g) {:declarations declarations
                                             :dynamic-sites dynamic-sites
                                             :usages usages}))
                            enabled-groups)]
    {:source-dir source-dir :group-results group-results}))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (when (or (:help options) (empty? args))
      (println "Usage: cljs-patrol [options] <source-dir> [<source-dir> ...]")
      (println)
      (println "  Detects unused re-frame subscriptions, events, and Spade style declarations.")
      (println "  Exits with code 1 when unused code is found.")
      (println)
      (println "Options:")
      (println summary)
      (System/exit 0))
    (when errors
      (doseq [e errors] (println e))
      (System/exit 1))
    (let [opts (select-keys options [:only :disable :output :files])
          dirs arguments
          enabled-groups (filter-groups opts)]
      (when (empty? dirs)
        (println "Error: no source directories specified")
        (System/exit 1))
      (let [run-results (cond-> (mapv #(run % enabled-groups) dirs)
                          (:files opts) (filter-run-results (:files opts)))
            any-failed? (some (fn [{:keys [group-results]}]
                                (some (fn [[g r]] ((:failed? g) r))
                                      (map vector enabled-groups group-results)))
                              run-results)]
        (case (:output opts)
          :html (do
                  (html-reporter/write-report enabled-groups run-results "report.html")
                  (println "Report written to report.html")
                  (doseq [{:keys [group-results]} run-results]
                    (print-summary enabled-groups group-results)))
          :edn  (edn-reporter/print-report enabled-groups dirs run-results)
          (doseq [{:keys [group-results]} run-results]
            (doseq [[g r] (map vector enabled-groups group-results)]
              ((:report g) r))
            (print-summary enabled-groups group-results)))
        (System/exit (if any-failed? 1 0))))))
