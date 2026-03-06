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

(def ^:private builtin-groups [re-frame/group spade/group])

(defn- filter-groups [{:keys [disable only]} groups]
  (cond
    only (filter #(contains? only (:id %)) groups)
    (seq disable) (remove #(contains? disable (:id %)) groups)
    :else groups))

(def ^:private cli-options
  [[nil "--only GROUPS" "Enable only these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--disable GROUPS" "Disable these groups (comma-separated)"
    :parse-fn #(set (map keyword (str/split % #",")))]
   [nil "--output FORMAT" "Output format: html or edn"
    :parse-fn keyword]
   [nil "--files FILES" "Limit results to these files (comma-separated)"
    :parse-fn #(str/split % #",")]
   [nil "--extra-groups VARS" "Extra group vars to load (comma-separated qualified symbols)"
    :parse-fn #(str/split % #",")]
   ["-h" "--help"]])

(defn- load-extra-groups [qualified-syms]
  (mapv (fn [sym-str]
          (let [sym (symbol sym-str)]
            (when-not (namespace sym)
              (throw (ex-info (str "--extra-groups requires qualified symbols, got: " sym-str) {})))
            (require (symbol (namespace sym)))
            (let [v (resolve sym)]
              (when-not v
                (throw (ex-info (str "Could not resolve extra group var: " sym-str) {})))
              (var-get v))))
        qualified-syms))

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
    (let [opts (select-keys options [:only :disable :output :files :extra-groups])
          extra-groups (when-let [eg (:extra-groups opts)]
                         (load-extra-groups eg))
          all-groups (concat builtin-groups extra-groups)
          dirs arguments
          enabled-groups (filter-groups opts all-groups)]
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
