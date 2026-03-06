(ns cljs-patrol.groups.re-frame
  "Re-frame rule group: detects unused and phantom subscriptions and events."
  (:require
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporters.console :as reporter]
   [rewrite-clj.zip :as z]))

(def ^:private decl-fn->type
  {"reg-sub" :sub
   "reg-event-db" :event
   "reg-event-fx" :event
   "reg-event-ctx" :event
   "reg-fx" :fx
   "reg-cofx" :cofx})

(def ^:private dispatch-fns #{"dispatch" "dispatch-sync"})

(def ^:private http-callback-keys #{":on-success" ":on-failure" ":on-error"})

(defn- handle-list
  "Detect re-frame declarations and usages from list nodes.
  Handles: reg-sub, reg-event-*, reg-fx, reg-cofx, subscribe, dispatch, dispatch-sync."
  [loc ns-name aliases file]
  (let [op-token (z/down loc)
        operator (parser/sym-name op-token)
        row (parser/position-row loc)]
    (cond
      (contains? decl-fn->type operator)
      (let [kw-loc (z/right (z/down loc))
            decl-type (get decl-fn->type operator)]
        (when (parser/kw-node? kw-loc)
          (when-let [resolved (parser/resolve-kw (parser/raw kw-loc) ns-name aliases)]
            {:decls [{:kw resolved :type decl-type :file file :row (parser/position-row kw-loc)}]
             :usages [] :dynamics []})))

      (= "subscribe" operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls [] :usages [] :dynamics [{:form (parser/raw loc) :file file :row row}]}
              {:decls [] :dynamics []
               :usages (when kw [{:kw kw :type :sub :file file :row row}])}))))

      (contains? dispatch-fns operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls [] :usages [] :dynamics [{:form (parser/raw loc) :file file :row row}]}
              {:decls [] :dynamics []
               :usages (when kw [{:kw kw :type :event :file file :row row}])}))))

      :else nil)))

(defn- handle-vector
  "Detect event usages from :fx tuple vectors.
  Handles: [:dispatch [::kw]] / [:dispatch-n [[::kw]...]] / [:dispatch-later {:dispatch [::kw]}]."
  [loc ns-name aliases file]
  (let [first-elem (z/down loc)
        row (parser/position-row loc)]
    (when (parser/kw-node? first-elem)
      (let [first-raw (parser/raw first-elem)]
        (cond
          ;; [:dispatch [::kw args...]]
          (= ":dispatch" first-raw)
          (when-let [vec-loc (z/right first-elem)]
            (when (= :vector (z/tag vec-loc))
              (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
                (if dynamic?
                  {:decls [] :usages [] :dynamics [{:form (parser/raw loc) :file file :row row}]}
                  (when kw {:decls [] :dynamics []
                            :usages [{:kw kw :type :event :file file :row row}]})))))

          ;; [:dispatch-n [[::kw1] [::kw2] ...]]
          (= ":dispatch-n" first-raw)
          (when-let [events-loc (z/right first-elem)]
            (when (= :vector (z/tag events-loc))
              (let [usages (loop [ev-loc (z/down events-loc) acc []]
                             (if (nil? ev-loc)
                               acc
                               (let [result (when (= :vector (z/tag ev-loc))
                                              (parser/extract-kw-from-vector ev-loc ns-name aliases))]
                                 (if (and result (not (:dynamic? result)) (:kw result))
                                   (recur (z/right ev-loc)
                                          (conj acc {:kw (:kw result) :type :event :file file :row row}))
                                   (recur (z/right ev-loc) acc)))))]
                {:decls [] :dynamics [] :usages usages})))

          ;; [:dispatch-later {:ms N :dispatch [::kw]}]
          (= ":dispatch-later" first-raw)
          (when-let [map-loc (z/right first-elem)]
            (when (= :map (z/tag map-loc))
              (loop [kv-loc (z/down map-loc)]
                (when kv-loc
                  (if (and (parser/kw-node? kv-loc) (= ":dispatch" (parser/raw kv-loc)))
                    (when-let [v-loc (z/right kv-loc)]
                      (when (= :vector (z/tag v-loc))
                        (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector v-loc ns-name aliases)]
                          (if dynamic?
                            {:decls [] :usages [] :dynamics [{:form (parser/raw loc) :file file :row row}]}
                            (when kw {:decls [] :dynamics []
                                      :usages [{:kw kw :type :event :file file :row row}]})))))
                    (recur (z/right kv-loc)))))))

          :else nil)))))

(defn- handle-token
  "Detect usages from keyword tokens.
  Handles: :<- signal inputs in reg-sub, :on-success/:on-failure/:on-error http callbacks."
  [loc ns-name aliases file]
  (when (parser/kw-node? loc)
    (let [raw-str (parser/raw loc)
          row (parser/position-row loc)]
      (cond
        ;; :<- [::dep-kw] — subscription signal input in reg-sub
        (= ":<-" raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls [] :dynamics []
                 :usages [{:kw kw :type :sub :file file :row row}]}))))

        ;; :dispatch-n — deprecated effect, use :fx instead
        (= ":dispatch-n" raw-str)
        {:decls [] :usages []
         :dynamics [{:type :deprecated :effect ":dispatch-n" :form raw-str :file file :row row}]}

        ;; :on-success / :on-failure / :on-error [::event-kw] — http effect callbacks
        (contains? http-callback-keys raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls [] :dynamics []
                 :usages [{:kw kw :type :event :file file :row row}]}))))

        :else nil))))

(defn- find-duplicates [decls]
  (->> decls
       (group-by :kw)
       (filter #(> (count (val %)) 1))
       (mapcat val)
       vec))

(defn analyze
  "Compute re-frame dead code from parsed declarations, usages, and dynamic-sites."
  [{:keys [declarations dynamic-sites usages]}]
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
                               (filter #(= :event (:type %)) usages))

        deprecated-effects (filter #(= :deprecated (:type %)) dynamic-sites)
        dynamic-dispatch (remove #(= :deprecated (:type %)) dynamic-sites)]
    {:deprecated-effects deprecated-effects
     :duplicate-subs (find-duplicates sub-decls)
     :duplicate-events (find-duplicates event-decls)
     :dynamic-sites dynamic-dispatch
     :phantom-events (parser/distinct-by :kw phantom-events)
     :phantom-subs (parser/distinct-by :kw phantom-subs)
     :unused-events (parser/distinct-by :kw unused-events)
     :unused-subs (parser/distinct-by :kw unused-subs)}))

(defn report
  "Print the re-frame analysis report sections."
  [{:keys [deprecated-effects duplicate-events duplicate-subs dynamic-sites
           phantom-events phantom-subs unused-events unused-subs]}]
  (reporter/print-section "DUPLICATE SUBSCRIPTIONS" duplicate-subs)
  (reporter/print-section "DUPLICATE EVENTS" duplicate-events)
  (reporter/print-section "UNUSED SUBSCRIPTIONS" unused-subs)
  (reporter/print-section "UNUSED EVENTS" unused-events)
  (reporter/print-section "PHANTOM SUBSCRIPTIONS (subscribed but never declared)" phantom-subs)
  (reporter/print-section "PHANTOM EVENTS (dispatched but never declared)" phantom-events)
  (reporter/print-dynamic-section "DEPRECATED EFFECTS (use :fx instead)" deprecated-effects)
  (reporter/print-dynamic-section "DYNAMIC DISPATCH/SUBSCRIBE SITES (manual review needed)" dynamic-sites))

(defn summary-lines
  "Return [[label count] ...] for the summary section."
  [{:keys [deprecated-effects duplicate-events duplicate-subs dynamic-sites
           phantom-events phantom-subs unused-events unused-subs]}]
  [["Duplicate subscriptions:" (count duplicate-subs)]
   ["Duplicate events:" (count duplicate-events)]
   ["Unused subscriptions:" (count unused-subs)]
   ["Unused events:" (count unused-events)]
   ["Phantom subscriptions:" (count phantom-subs)]
   ["Phantom events:" (count phantom-events)]
   ["Deprecated effects:" (count deprecated-effects)]
   ["Dynamic sites:" (count dynamic-sites)]])

(defn failed?
  "Return true if there are duplicate registrations, unused subscriptions, events, or deprecated effects."
  [{:keys [deprecated-effects duplicate-events duplicate-subs unused-events unused-subs]}]
  (or (seq duplicate-subs) (seq duplicate-events)
      (seq unused-subs) (seq unused-events)
      (seq deprecated-effects)))

(def group
  "Re-frame rule group map."
  {:id :re-frame
   :name "Re-frame"
   :parse {:handle-list handle-list
           :handle-vector handle-vector
           :handle-token handle-token}
   :analyze analyze
   :report report
   :summary-lines summary-lines
   :failed? failed?
   :suggestions
   {:duplicate-subs
    "Two reg-sub calls share the same keyword - the second silently overwrites the first at runtime. Remove the duplicate declaration."
    :duplicate-events
    "Two reg-event-* calls share the same keyword - the second silently overwrites the first at runtime. Remove the duplicate declaration."
    :unused-subs
    "Registered with reg-sub but never subscribed to. Remove the reg-sub declaration, or add a (rf/subscribe [::kw]) call where the value is needed."
    :unused-events
    "Registered with reg-event-* but never dispatched. Remove the declaration, or add a (rf/dispatch [::kw]) call where the event should be triggered."
    :phantom-subs
    "Subscribed to via (rf/subscribe [::kw]) but never declared with reg-sub. Usually a keyword typo or wrong namespace alias. Fix the keyword at the subscribe call site."
    :phantom-events
    "Dispatched via (rf/dispatch [::kw]) but never declared with reg-event-*. Fix the keyword at the dispatch call site, or add the missing reg-event-* declaration."
    :deprecated-effects
    "Usage of :dispatch-n, which is deprecated. Replace with :fx. Example: {:dispatch-n [[::event-a arg] [::event-b]]} becomes {:fx [[:dispatch [::event-a arg]] [:dispatch [::event-b]]]}."
    :dynamic-sites
    "Dispatch or subscribe call with a non-literal keyword - cannot be statically resolved. Requires manual review to confirm the correct handler is being used."}
   :html-sections [{:title "Duplicate Subscriptions"
                    :description "Registered more than once with reg-sub — the second registration silently overwrites the first."
                    :data-fn :duplicate-subs
                    :columns [:keyword :file :line]}
                   {:title "Duplicate Events"
                    :description "Registered more than once with reg-event-* — the second registration silently overwrites the first."
                    :data-fn :duplicate-events
                    :columns [:keyword :file :line]}
                   {:title "Unused Subscriptions"
                    :description "Registered with reg-sub but never subscribed to via subscribe."
                    :data-fn :unused-subs
                    :columns [:keyword :file :line]}
                   {:title "Unused Events"
                    :description "Registered with reg-event-* but never dispatched via dispatch or dispatch-sync."
                    :data-fn :unused-events
                    :columns [:keyword :file :line]}
                   {:title "Phantom Subscriptions"
                    :description "Used via subscribe but never declared with reg-sub. Likely a bug or missing import."
                    :data-fn :phantom-subs
                    :columns [:keyword :file :line]}
                   {:title "Phantom Events"
                    :description "Dispatched via dispatch/dispatch-sync but never declared with reg-event-*. Likely a bug or missing import."
                    :data-fn :phantom-events
                    :columns [:keyword :file :line]}
                   {:title "Deprecated Effects"
                    :description "Usage of :dispatch-n, which is deprecated. Use :fx instead."
                    :data-fn :deprecated-effects
                    :columns [:form :file :line]}
                   {:title "Dynamic Sites"
                    :description "Dispatch or subscribe calls where the keyword is not a literal — the actual handler cannot be statically determined. Requires manual review."
                    :data-fn :dynamic-sites
                    :columns [:form :file :line]}]})
