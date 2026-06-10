(ns cljs-patrol.groups.re-frame
  "Re-frame rule group: detects unused and phantom subscriptions and events."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
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

(def ^:private known-1-arity-fns
  "Built-in 1-arity functions commonly misused with `:=>` sugar in reg-sub.
  When the sub's body is a single 1-arity fn, the correct sugar is `:->`
  (which calls it with one arg); `:=>` calls it with two args, and CLJS
  silently ignores the extra one."
  #{"first" "second" "last" "ffirst" "fnext" "next" "rest" "butlast"
    "count" "empty" "seq" "vec" "set" "sort" "sort-by"
    "keys" "vals" "name" "namespace" "key" "val"
    "empty?" "nil?" "some?" "any?" "boolean" "not" "true?" "false?"
    "inc" "dec" "pos?" "neg?" "zero?" "abs"
    "identity"
    "reverse" "shuffle" "frequencies" "flatten" "distinct"})

(defn- find-=>-1-arity-misuse
  "Walk `reg-sub` siblings looking for `:=> SYM` where SYM is in known-1-arity-fns.
  Returns the name string of the misused fn (or nil)."
  [reg-sub-loc]
  (loop [cur (z/down reg-sub-loc)]
    (cond
      (nil? cur) nil

      (and (parser/kw-node? cur) (= ":=>" (parser/raw cur)))
      (let [next-loc (z/right cur)
            next-name (when (and next-loc (= :token (z/tag next-loc)))
                        (parser/sym-name next-loc))]
        (if (contains? known-1-arity-fns next-name)
          next-name
          (recur (z/right cur))))

      :else
      (recur (z/right cur)))))

(defn- fn-form?
  "True if `loc` is a list whose first child is `fn` or `fn*`."
  [loc]
  (and loc
       (= :list (z/tag loc))
       (let [head (z/down loc)]
         (and head
              (= :token (z/tag head))
              (#{"fn" "fn*"} (parser/sym-name head))))))

(defn- last-fn-form
  "Return the last child of `loc` that is a (fn ...) form, or nil."
  [loc]
  (loop [cur (z/down loc)
         found nil]
    (if (nil? cur)
      found
      (recur (z/right cur)
             (if (fn-form? cur) cur found)))))

(defn- last-sibling
  "Walk to the rightmost sibling starting from `loc`."
  [loc]
  (loop [cur loc
         last-loc loc]
    (let [nxt (z/right cur)]
      (if (nil? nxt)
        last-loc
        (recur nxt nxt)))))

(defn- fn-body-last
  "Given a `(fn [args] body...)` zip loc, return the last body expression loc."
  [fn-loc]
  (when-let [args-loc (some-> (z/down fn-loc) z/right)]
    (when (= :vector (z/tag args-loc))
      (some-> (z/right args-loc) last-sibling))))

(defn- classify-event-fx-return
  "Inspect `reg-event-fx` body's last expression.
  Returns :empty when it's an empty map or nil literal, :db-only when it's a
  map whose only key is :db, otherwise nil."
  [reg-event-fx-loc]
  (when-let [fn-loc (last-fn-form reg-event-fx-loc)]
    (when-let [body-last (fn-body-last fn-loc)]
      (cond
        (and (= :token (z/tag body-last))
             (= "nil" (parser/raw body-last)))
        :empty

        (= :map (z/tag body-last))
        (let [keys-set (->> (z/child-sexprs body-last)
                            (take-nth 2)
                            set)]
          (cond
            (empty? keys-set) :empty
            (= #{:db} keys-set) :db-only))))))

(defn- reg-event-db-empty-return?
  "True if `reg-event-db`'s last fn body returns nil or {}.
  Such a handler clobbers the entire app-db, which is almost always a mistake."
  [reg-event-db-loc]
  (when-let [fn-loc (last-fn-form reg-event-db-loc)]
    (when-let [body-last (fn-body-last fn-loc)]
      (or (and (= :token (z/tag body-last))
               (= "nil" (parser/raw body-last)))
          (and (= :map (z/tag body-last))
               (empty? (z/child-sexprs body-last)))))))

(defn- db-keyed-map?
  "True if `loc` is a map literal containing `:db` as a key."
  [loc]
  (and (= :map (z/tag loc))
       (try
         (contains? (->> (z/child-sexprs loc)
                         (take-nth 2)
                         set)
                    :db)
         (catch Exception _ false))))

(defn- collect-tail-locs
  "Return all tail-position locs reachable from `loc` by unwrapping let/do/if/
  when at the structural top. Does NOT recurse into nested fn forms, so the
  inner accumulator in `(reduce (fn [db item] ...) db items)` is not visited.
  Stops at the first non-control-flow form."
  [loc]
  (if-not (and loc (= :list (z/tag loc)))
    (when loc [loc])
    (let [op (parser/sym-name (z/down loc))]
      (cond
        (#{"let" "let*" "letfn" "binding" "when-let" "if-let" "when-some" "if-some"} op)
        (some-> loc z/down z/right z/right last-sibling collect-tail-locs)

        (= "do" op)
        (some-> loc z/down z/right last-sibling collect-tail-locs)

        (#{"when" "when-not" "when-first"} op)
        (some-> loc z/down z/right z/right last-sibling collect-tail-locs)

        (#{"if" "if-not"} op)
        (let [then-loc (some-> loc z/down z/right z/right)
              else-loc (some-> then-loc z/right)]
          (concat (collect-tail-locs then-loc)
                  (when else-loc (collect-tail-locs else-loc))))

        :else [loc]))))

(defn- reg-event-db-returning-effects?
  "True if a `reg-event-db` handler has any tail-position value that is a map
  literal with `:db` as a key — i.e., it returns an effects-style map instead
  of a new db. This silently replaces app-db with the effects map and drops
  all extra effects (toasts, dispatches, ...), which is almost always a bug."
  [reg-event-db-loc]
  (when-let [fn-loc (last-fn-form reg-event-db-loc)]
    (when-let [body-last (fn-body-last fn-loc)]
      (boolean (some db-keyed-map? (collect-tail-locs body-last))))))

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
            (let [misused-fn (when (= "reg-sub" operator)
                               (find-=>-1-arity-misuse loc))
                  event-fx-shape (when (= "reg-event-fx" operator)
                                   (classify-event-fx-return loc))
                  event-db-empty? (when (= "reg-event-db" operator)
                                    (reg-event-db-empty-return? loc))
                  event-db-effects? (when (= "reg-event-db" operator)
                                      (reg-event-db-returning-effects? loc))
                  base {:decls [{:kw resolved
                                 :type decl-type
                                 :file file
                                 :row (parser/position-row kw-loc)}]
                        :usages []
                        :dynamics []}]
              (cond-> base
                misused-fn
                (update :usages conj {:kw resolved
                                      :type :sugar-mismatch
                                      :fn misused-fn
                                      :file file
                                      :row (parser/position-row kw-loc)})

                event-fx-shape
                (update :usages conj {:kw resolved
                                      :type (case event-fx-shape
                                              :db-only :event-fx-db-only
                                              :empty :event-fx-empty)
                                      :file file
                                      :row (parser/position-row kw-loc)})

                event-db-empty?
                (update :usages conj {:kw resolved
                                      :type :event-db-empty
                                      :file file
                                      :row (parser/position-row kw-loc)})

                event-db-effects?
                (update :usages conj {:kw resolved
                                      :type :event-db-returning-effects
                                      :file file
                                      :row (parser/position-row kw-loc)}))))))

      (= "subscribe" operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls []
               :usages []
               :dynamics [{:form (parser/raw loc)
                           :file file
                           :row row}]}
              {:decls []
               :dynamics []
               :usages (when kw [{:kw kw
                                  :type :sub
                                  :file file
                                  :row row}])}))))

      (contains? dispatch-fns operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls []
               :usages []
               :dynamics [{:form (parser/raw loc)
                           :file file
                           :row row}]}
              {:decls []
               :dynamics []
               :usages (when kw [{:kw kw
                                  :type :event
                                  :file file
                                  :row row}])}))))

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
                  {:decls []
                   :usages []
                   :dynamics [{:form (parser/raw loc)
                               :file file
                               :row row}]}
                  (when kw {:decls []
                            :dynamics []
                            :usages [{:kw kw
                                      :type :event
                                      :file file
                                      :row row}]})))))

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
                                          (conj acc {:kw (:kw result)
                                                     :type :event
                                                     :file file
                                                     :row row}))
                                   (recur (z/right ev-loc) acc)))))]
                {:decls []
                 :dynamics []
                 :usages usages})))

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
                            {:decls []
                             :usages []
                             :dynamics [{:form (parser/raw loc)
                                         :file file
                                         :row row}]}
                            (when kw {:decls []
                                      :dynamics []
                                      :usages [{:kw kw
                                                :type :event
                                                :file file
                                                :row row}]})))))
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
                {:decls []
                 :dynamics []
                 :usages [{:kw kw
                           :type :sub
                           :file file
                           :row row}]}))))

        ;; :dispatch-n — deprecated effect, use :fx instead
        (= ":dispatch-n" raw-str)
        {:decls []
         :usages []
         :dynamics [{:type :deprecated
                     :effect ":dispatch-n"
                     :form raw-str
                     :file file
                     :row row}]}

        ;; :on-success / :on-failure / :on-error [::event-kw] — http effect callbacks
        (contains? http-callback-keys raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (parser/extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls []
                 :dynamics []
                 :usages [{:kw kw
                           :type :event
                           :file file
                           :row row}]}))))

        :else nil))))

(defn- find-duplicates [decls]
  (->> decls
       (group-by :kw)
       (filter #(> (count (val %)) 1))
       (mapcat val)
       vec))

(defn- analyze* [{:keys [declarations dynamic-sites usages]}]
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
        dynamic-dispatch (remove #(= :deprecated (:type %)) dynamic-sites)
        sugar-mismatches (filter #(= :sugar-mismatch (:type %)) usages)
        event-fx-db-only (filter #(= :event-fx-db-only (:type %)) usages)
        event-fx-empty (filter #(= :event-fx-empty (:type %)) usages)
        event-db-empty (filter #(= :event-db-empty (:type %)) usages)
        event-db-returning-effects (filter #(= :event-db-returning-effects (:type %)) usages)]
    {:duplicate-subs (find-duplicates sub-decls)
     :duplicate-events (find-duplicates event-decls)
     :unused-subs (parser/distinct-by :kw unused-subs)
     :unused-events (parser/distinct-by :kw unused-events)
     :phantom-subs (parser/distinct-by :kw phantom-subs)
     :phantom-events (parser/distinct-by :kw phantom-events)
     :deprecated-effects deprecated-effects
     :dynamic-sites dynamic-dispatch
     :reg-sub-=>-1-arity (parser/distinct-by :kw sugar-mismatches)
     :reg-event-fx-db-only (parser/distinct-by :kw event-fx-db-only)
     :reg-event-fx-empty (parser/distinct-by :kw event-fx-empty)
     :reg-event-db-empty (parser/distinct-by :kw event-db-empty)
     :reg-event-db-returning-effects (parser/distinct-by :kw event-db-returning-effects)}))

(defn- summary-lines*
  [{:keys [deprecated-effects duplicate-events duplicate-subs dynamic-sites
           phantom-events phantom-subs unused-events unused-subs
           reg-sub-=>-1-arity reg-event-fx-db-only reg-event-fx-empty
           reg-event-db-empty reg-event-db-returning-effects]}]
  [["Duplicate subscriptions:" (count duplicate-subs)]
   ["Duplicate events:" (count duplicate-events)]
   ["Unused subscriptions:" (count unused-subs)]
   ["Unused events:" (count unused-events)]
   ["Phantom subscriptions:" (count phantom-subs)]
   ["Phantom events:" (count phantom-events)]
   ["Deprecated effects:" (count deprecated-effects)]
   ["reg-sub :=> with 1-arity fn:" (count reg-sub-=>-1-arity)]
   ["reg-event-fx returns only :db:" (count reg-event-fx-db-only)]
   ["reg-event-fx empty effects:" (count reg-event-fx-empty)]
   ["reg-event-db clobbers db:" (count reg-event-db-empty)]
   ["reg-event-db returns effects map:" (count reg-event-db-returning-effects)]
   ["Dynamic sites:" (count dynamic-sites)]])

(defn- failed?* [{:keys [deprecated-effects duplicate-events duplicate-subs unused-events unused-subs
                         reg-event-db-returning-effects]}]
  (or (seq duplicate-subs) (seq duplicate-events)
      (seq unused-subs) (seq unused-events)
      (seq deprecated-effects)
      (seq reg-event-db-returning-effects)))

(defrecord ReFrameGroup []
  group/RuleGroup
  (group-id [_] :re-frame)
  (group-name [_] "Re-frame")
  (parse-handlers [_]
    {:handle-list handle-list
     :handle-vector handle-vector
     :handle-token handle-token})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
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
     "Dispatch or subscribe call with a non-literal keyword - cannot be statically resolved. Requires manual review to confirm the correct handler is being used."
     :reg-sub-=>-1-arity
     "reg-sub uses :=> sugar with a 1-arity function. :=> calls the fn with (signal-value query-vector), so the query-vector is silently ignored on CLJS. Use :-> instead, which calls the fn with just the signal value."
     :reg-event-fx-db-only
     "reg-event-fx returns only :db. Use reg-event-db, which takes a handler returning the new db directly - simpler and clearer."
     :reg-event-fx-empty
     "reg-event-fx returns an empty effects map (or nil). The handler does nothing - either remove it, or return meaningful effects."
     :reg-event-db-empty
     "reg-event-db returns nil or {}, which clobbers the entire app-db. If a full reset is intended, prefer (assoc db ...) or document the intent; otherwise this is a bug."
     :reg-event-db-returning-effects
     "reg-event-db handler returns an effects-style map with :db (e.g. {:db ... :dispatch ...}). The whole map becomes the new app-db, replacing it; extra effect keys are silently dropped. Switch to reg-event-fx, which expects this shape."})
  (rule->tier [_]
    {:duplicate-subs :bugs
     :duplicate-events :bugs
     :reg-event-fx-empty :bugs
     :reg-event-db-empty :bugs
     :reg-event-db-returning-effects :bugs
     :deprecated-effects :deprecations
     :reg-sub-=>-1-arity :cleanup
     :reg-event-fx-db-only :cleanup
     :unused-subs :cleanup
     :unused-events :cleanup
     :phantom-subs :cleanup
     :phantom-events :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReFrameGroup))
