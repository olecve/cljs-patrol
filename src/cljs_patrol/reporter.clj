(ns cljs-patrol.reporter
  "Formats and prints cljs-patrol analysis results."
  (:require
   [clojure.string :as str]))

(defn- format-entry [{:keys [file kw row]}]
  (format "  %-60s %s:%d" (str kw) file row))

(defn- print-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [item (sort-by (comp str :kw) items)]
      (println (format-entry item)))))

(defn- print-dynamic-section [title items]
  (println (str "\n=== " title " (" (count items) ") ==="))
  (if (empty? items)
    (println "  (none)")
    (doseq [{:keys [file form row]} (sort-by :file items)]
      (println (format "  %s:%d  %s" file row (str/trim form))))))

(defn print-report
  "Prints a human-readable report of unused subs/events, phantom refs, and dynamic sites."
  [{:keys [phantom-events phantom-subs unused-events unused-styles unused-subs]} dynamic-sites]
  (print-section "UNUSED SUBSCRIPTIONS" unused-subs)
  (print-section "UNUSED EVENTS" unused-events)
  (print-section "UNUSED STYLES (defclass/defattrs)" unused-styles)
  (print-section "PHANTOM SUBSCRIPTIONS (subscribed but never declared)" phantom-subs)
  (print-section "PHANTOM EVENTS (dispatched but never declared)" phantom-events)
  (print-dynamic-section "DYNAMIC DISPATCH/SUBSCRIBE SITES (manual review needed)" dynamic-sites)
  (println "\n=== SUMMARY ===")
  (println (format "  %-30s %d" "Unused subscriptions:" (count unused-subs)))
  (println (format "  %-30s %d" "Unused events:" (count unused-events)))
  (println (format "  %-30s %d" "Unused styles:" (count unused-styles)))
  (println (format "  %-30s %d" "Phantom subscriptions:" (count phantom-subs)))
  (println (format "  %-30s %d" "Phantom events:" (count phantom-events)))
  (println (format "  %-30s %d" "Dynamic sites:" (count dynamic-sites))))
