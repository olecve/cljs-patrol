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
   [clojure.string :as str]))

(def ^:private all-groups [re-frame/group spade/group])

(defn- filter-groups [{:keys [disabled only]}]
  (cond
    only (filter #(contains? only (:id %)) all-groups)
    (seq disabled) (remove #(contains? disabled (:id %)) all-groups)
    :else all-groups))

(defn- parse-args
  "Parse CLI args, returning [opts dirs].
  Supports --only group1,group2, --disable group1,group2, --output <format>,
  and --files file1,file2 flags."
  [args]
  (loop [remaining args
         opts {:only nil :disabled nil :output nil :files nil}
         dirs []]
    (cond
      (empty? remaining)
      [opts dirs]

      (= "--only" (first remaining))
      (let [groups (set (map keyword (str/split (second remaining) #",")))]
        (recur (drop 2 remaining) (assoc opts :only groups) dirs))

      (= "--disable" (first remaining))
      (let [groups (set (map keyword (str/split (second remaining) #",")))]
        (recur (drop 2 remaining) (assoc opts :disabled groups) dirs))

      (= "--output" (first remaining))
      (let [fmt (keyword (second remaining))]
        (recur (drop 2 remaining) (assoc opts :output fmt) dirs))

      (= "--files" (first remaining))
      (let [files (str/split (second remaining) #",")]
        (recur (drop 2 remaining) (assoc opts :files files) dirs))

      :else
      (recur (rest remaining) opts (conj dirs (first remaining))))))

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
  (when (empty? args)
    (println "Usage: cljs-patrol [--only g1,g2] [--disable g1,g2] [--output html] <source-dir> [<source-dir> ...]")
    (println)
    (println "  Detects unused re-frame subscriptions, events, and Spade style declarations.")
    (println "  Exits with code 1 when unused code is found.")
    (println)
    (println "Groups: reframe, spade")
    (println)
    (println "Options:")
    (println "  --only g1,g2    Enable only the specified groups")
    (println "  --disable g1,g2 Disable the specified groups")
    (println "  --output html   Write report.html instead of console output")
    (println "  --output edn    Print EDN data to stdout (for programmatic/AI use)")
    (println "  --files f1,f2   Limit results to these files (full context is still used)")
    (println)
    (println "Example:")
    (println "  clojure -M:run src/cljs/myapp")
    (println "  clojure -M:run --only reframe src/cljs/myapp")
    (println "  clojure -M:run --output html src/cljs/myapp")
    (System/exit 0))
  (let [[opts dirs] (parse-args args)
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
      (System/exit (if any-failed? 1 0)))))
