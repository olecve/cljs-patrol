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
   [clojure.string :as str]))

(def ^:private all-groups [re-frame/group spade/group])

(defn- filter-groups [{:keys [disabled only]}]
  (cond
    only (filter #(contains? only (:id %)) all-groups)
    (seq disabled) (remove #(contains? disabled (:id %)) all-groups)
    :else all-groups))

(defn- parse-args
  "Parse CLI args, returning [opts dirs].
  Supports --only group1,group2 and --disable group1,group2 flags."
  [args]
  (loop [remaining args
         opts {:only nil :disabled nil}
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

      :else
      (recur (rest remaining) opts (conj dirs (first remaining))))))

(defn- print-summary [enabled-groups group-results]
  (println "\n=== SUMMARY ===")
  (doseq [[g r] (map vector enabled-groups group-results)]
    (doseq [[label cnt] ((:summary-lines g) r)]
      (println (format "  %-30s %d" label cnt)))))

(defn run
  "Analyze source-dir with enabled-groups, print reports and summary, return group results."
  [source-dir enabled-groups]
  (let [{:keys [declarations dynamic-sites usages]} (parser/analyze-project source-dir enabled-groups)
        group-results (mapv (fn [g]
                              ((:analyze g) {:declarations declarations
                                             :dynamic-sites dynamic-sites
                                             :usages usages}))
                            enabled-groups)]
    (doseq [[g r] (map vector enabled-groups group-results)]
      ((:report g) r))
    (print-summary enabled-groups group-results)
    group-results))

(defn -main
  [& args]
  (when (empty? args)
    (println "Usage: cljs-patrol [--only g1,g2] [--disable g1,g2] <source-dir> [<source-dir> ...]")
    (println)
    (println "  Detects unused re-frame subscriptions, events, and Spade style declarations.")
    (println "  Exits with code 1 when unused code is found.")
    (println)
    (println "Groups: reframe, spade")
    (println)
    (println "Options:")
    (println "  --only g1,g2    Enable only the specified groups")
    (println "  --disable g1,g2 Disable the specified groups")
    (println)
    (println "Example:")
    (println "  clojure -M:run src/cljs/myapp")
    (println "  clojure -M:run --only reframe src/cljs/myapp")
    (System/exit 0))
  (let [[opts dirs] (parse-args args)
        enabled-groups (filter-groups opts)]
    (when (empty? dirs)
      (println "Error: no source directories specified")
      (System/exit 1))
    (let [all-group-results (mapv #(run % enabled-groups) dirs)
          any-failed? (some (fn [group-results]
                              (some (fn [[g r]] ((:failed? g) r))
                                    (map vector enabled-groups group-results)))
                            all-group-results)]
      (System/exit (if any-failed? 1 0)))))
