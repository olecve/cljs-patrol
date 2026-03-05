(ns cljs-patrol.groups.re-frame
  "Re-frame rule group: detects unused and phantom subscriptions and events."
  (:require
   [cljs-patrol.parser :as parser]
   [cljs-patrol.reporter :as reporter]
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

        ;; :on-success / :on-failure / :on-error [::event-kw] — http effect callbacks
        (contains? http-callback-keys raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls [] :dynamics []
                 :usages [{:kw kw :type :event :file file :row row}]}))))

        :else nil))))

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
                               (filter #(= :event (:type %)) usages))]
    {:dynamic-sites dynamic-sites
     :phantom-events (parser/distinct-by :kw phantom-events)
     :phantom-subs (parser/distinct-by :kw phantom-subs)
     :unused-events (parser/distinct-by :kw unused-events)
     :unused-subs (parser/distinct-by :kw unused-subs)}))

(defn report
  "Print the re-frame analysis report sections."
  [{:keys [dynamic-sites phantom-events phantom-subs unused-events unused-subs]}]
  (reporter/print-section "UNUSED SUBSCRIPTIONS" unused-subs)
  (reporter/print-section "UNUSED EVENTS" unused-events)
  (reporter/print-section "PHANTOM SUBSCRIPTIONS (subscribed but never declared)" phantom-subs)
  (reporter/print-section "PHANTOM EVENTS (dispatched but never declared)" phantom-events)
  (reporter/print-dynamic-section "DYNAMIC DISPATCH/SUBSCRIBE SITES (manual review needed)" dynamic-sites))

(defn summary-lines
  "Return [[label count] ...] for the summary section."
  [{:keys [dynamic-sites phantom-events phantom-subs unused-events unused-subs]}]
  [["Unused subscriptions:" (count unused-subs)]
   ["Unused events:" (count unused-events)]
   ["Phantom subscriptions:" (count phantom-subs)]
   ["Phantom events:" (count phantom-events)]
   ["Dynamic sites:" (count dynamic-sites)]])

(defn failed?
  "Return true if there are unused subscriptions or events."
  [{:keys [unused-events unused-subs]}]
  (or (seq unused-subs) (seq unused-events)))

(def group
  "Re-frame rule group map."
  {:id :re-frame
   :parse {:handle-list handle-list
           :handle-vector handle-vector
           :handle-token handle-token}
   :analyze analyze
   :report report
   :summary-lines summary-lines
   :failed? failed?})
