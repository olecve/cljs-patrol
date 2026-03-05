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
  [{:keys [unused-subs unused-events phantom-subs phantom-events]} dynamic-sites]
  (print-section "UNUSED SUBSCRIPTIONS" unused-subs)
  (print-section "UNUSED EVENTS" unused-events)
  (print-section "PHANTOM SUBSCRIPTIONS (subscribed but never declared)" phantom-subs)
  (print-section "PHANTOM EVENTS (dispatched but never declared)" phantom-events)
  (print-dynamic-section "DYNAMIC DISPATCH/SUBSCRIBE SITES (manual review needed)" dynamic-sites)
  (println)
  (println (str "Summary: "
                (count unused-subs) " unused subs, "
                (count unused-events) " unused events, "
                (count phantom-subs) " phantom subs, "
                (count phantom-events) " phantom events, "
                (count dynamic-sites) " dynamic sites")))
