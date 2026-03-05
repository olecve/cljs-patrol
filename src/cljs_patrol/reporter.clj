(ns cljs-patrol.reporter
  "Shared formatting utilities for cljs-patrol analysis output."
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
