(ns cljs-patrol.reporters.console
  "Console output formatting for cljs-patrol analysis results."
  (:require
   [clojure.string :as str]))

(defn format-entry [{:keys [file kw row]}]
  (format "  %-60s %s:%d" (str kw) file row))

(defn print-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [item (sort-by (comp str :kw) items)]
      (println (format-entry item)))))

(defn print-dynamic-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [{:keys [file form row]} (sort-by :file items)]
      (println (format "  %s:%d  %s" file row (str/trim form))))))

(defn- key->title [k]
  (-> (name k)
      (str/replace #"-" " ")
      str/capitalize))

(defn report [result]
  (doseq [[k items] result
          :when (sequential? items)]
    (if (and (seq items) (:form (first items)))
      (print-dynamic-section (key->title k) items)
      (print-section (key->title k) items))))
