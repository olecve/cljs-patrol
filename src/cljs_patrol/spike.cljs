(ns cljs-patrol.spike
  "Phase-1 driver: run the Spade rule group over a fixture directory from Node."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.parser :as parser]))

(defn- run [source-dir enabled-groups]
  (let [parsed (parser/analyze-project source-dir enabled-groups)]
    (mapv #(group/analyze % parsed) enabled-groups)))

(defn -main [& args]
  (let [dir (or (first args) "test/projects/re-frame-spade-app/src/webapp")
        results (run dir [spade/group])
        summary (group/summary-lines spade/group (first results))]
    (println "cljs-patrol (cljs) — spade only — " dir)
    (println)
    (doseq [[label cnt] summary]
      (println " " label cnt))
    (println)
    (println "Full result:")
    (println (pr-str (first results)))))
