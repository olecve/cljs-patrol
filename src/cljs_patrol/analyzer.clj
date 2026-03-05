(ns cljs-patrol.analyzer
  "Cross-file analysis: identify unused and phantom re-frame subscriptions and events.")

(defn- distinct-by [key-fn coll]
  (vals (into {} (map (juxt key-fn identity) coll))))

(defn compute-dead-code
  "Cross-reference declarations against usages to find:
  - unused-subs:    declared with reg-sub but never subscribed to
  - unused-events:  declared with reg-event-* but never dispatched
  - phantom-subs:   subscribed to but no reg-sub declaration found
  - phantom-events: dispatched but no reg-event-* declaration found

  Each entry is a map with :kw, :type, :file, :row."
  [{:keys [declarations usages]}]
  (let [sub-decls (filter #(= :sub (:type %)) declarations)
        event-decls (filter #(= :event (:type %)) declarations)

        sub-usage-kws (set (map :kw (filter #(= :sub (:type %)) usages)))
        event-usage-kws (set (map :kw (filter #(= :event (:type %)) usages)))

        unused-subs (remove #(contains? sub-usage-kws (:kw %)) sub-decls)
        unused-events (remove #(contains? event-usage-kws (:kw %)) event-decls)

        declared-sub-kws (set (map :kw sub-decls))
        declared-event-kws (set (map :kw event-decls))

        phantom-subs (remove #(contains? declared-sub-kws (:kw %))
                             (filter #(= :sub (:type %)) usages))
        phantom-events (remove #(contains? declared-event-kws (:kw %))
                               (filter #(= :event (:type %)) usages))]
    {:unused-subs (distinct-by :kw unused-subs)
     :unused-events (distinct-by :kw unused-events)
     :phantom-subs (distinct-by :kw phantom-subs)
     :phantom-events (distinct-by :kw phantom-events)}))
