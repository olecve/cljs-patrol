(ns cljs-patrol.core
  "Entry point for the cljs-patrol CLI tool.

  Detects unused re-frame subscriptions and events via static analysis of
  ClojureScript source files. Exits with code 1 when unused subs or events
  are found, making it suitable for use in CI pipelines."
  (:gen-class)
  (:require
   [cljs-patrol.analyzer :as analyzer]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporter :as reporter]))

(defn run
  "Run the analysis on source-dir and return the dead-code map."
  [source-dir]
  (let [{:keys [declarations dynamic-sites usages]} (parser/analyze-project source-dir)
        dead-code (analyzer/compute-dead-code {:declarations declarations :usages usages})]
    (reporter/print-report dead-code dynamic-sites)
    dead-code))

(defn -main
  [& args]
  (when (empty? args)
    (println "Usage: cljs-patrol <source-dir> [<source-dir> ...]")
    (println)
    (println "  Detects unused re-frame subscriptions and events.")
    (println "  Exits with code 1 when unused subs or events are found.")
    (println)
    (println "Example:")
    (println "  clojure -M:run src/cljs/myapp")
    (System/exit 0))
  (let [results (mapv run args)
        any-unused? (some #(or (seq (:unused-subs %)) (seq (:unused-events %))) results)]
    (System/exit (if any-unused? 1 0))))
