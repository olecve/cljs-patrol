(ns cljs-patrol.spike
  "Phase-2 driver: run every rule group over a fixture directory from Node."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.docstrings :as docstrings]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.groups.typography :as typography]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.severity :as severity]))

(def all-groups
  [re-frame/group spade/group reagent/group typography/group a11y/group docstrings/group])

(defn- run [source-dir enabled-groups]
  (let [parsed (parser/analyze-project source-dir enabled-groups)]
    (mapv (fn [g] (severity/annotate-tiers g (group/analyze g parsed)))
          enabled-groups)))

(defn -main [& args]
  (let [dir (or (first args) "test/projects/re-frame-spade-app/src/webapp")
        results (run dir all-groups)]
    (println "cljs-patrol (cljs) —" dir)
    (println)
    (doseq [[g result] (map vector all-groups results)]
      (println (str "== " (group/group-name g) " =="))
      (doseq [[label cnt] (group/summary-lines g result)]
        (println " " label cnt))
      (println))))
