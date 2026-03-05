(ns cljs-patrol.parser
  "Performs per-file static analysis to extract re-frame declarations and usages from ClojureScript source."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(def decl-fn->type
  "Maps re-frame registration function names to their semantic type."
  {"reg-sub" :sub
   "reg-event-db" :event
   "reg-event-fx" :event
   "reg-event-ctx" :event
   "reg-fx" :fx
   "reg-cofx" :cofx})

(def ^:private dispatch-fns #{"dispatch" "dispatch-sync"})

(def ^:private http-callback-keys #{":on-success" ":on-failure" ":on-error"})

(defn- raw
  "Return the raw source string of a node, preserving :: prefixes and aliases."
  [zloc]
  (n/string (z/node zloc)))

(defn- kw-node?
  "True if the node is a keyword token (including :: prefixed ones)."
  [zloc]
  (and zloc
       (= :token (z/tag zloc))
       (str/starts-with? (raw zloc) ":")))

(defn- sym-name
  "Return the name string of a symbol token, or nil."
  [zloc]
  (when (and zloc (= :token (z/tag zloc)))
    (let [sexpr-value (try (z/sexpr zloc) (catch Exception _ nil))]
      (when (symbol? sexpr-value) (name sexpr-value)))))

(defn- position-row
  [zloc]
  (try (first (z/position zloc)) (catch Exception _ 0)))

(defn- parse-require-alias
  "Extract [alias-str full-ns-str] from a require vector like [full.ns :as alias], or nil."
  [req-vec]
  (when (vector? req-vec)
    (let [ns-sym (first req-vec)
          pairs (rest req-vec)]
      (when ns-sym
        (loop [remaining-pairs pairs]
          (when (>= (count remaining-pairs) 2)
            (if (= :as (first remaining-pairs))
              [(str (second remaining-pairs)) (str ns-sym)]
              (recur (rest remaining-pairs)))))))))

(defn- parse-ns-form
  "Extract {:ns-name str :aliases {alias-str full-ns-str}} from a (ns ...) sexpr."
  [ns-sexpr]
  (let [ns-name (str (second ns-sexpr))
        aliases (into {}
                      (for [clause (rest ns-sexpr)
                            :when (and (seq? clause) (= :require (first clause)))
                            req   (rest clause)
                            :let  [pair (parse-require-alias req)]
                            :when pair]
                        pair))]
    {:ns-name ns-name :aliases aliases}))

(defn- find-ns-info
  "Find and parse the ns form from a rewrite-clj zip.
  z/of-file positions at the first top-level form (not a :forms wrapper).
  Walk top-level siblings with z/right until the ns form is found."
  [zloc]
  (loop [loc zloc]
    (when (and loc (not (z/end? loc)))
      (if (and (= :list (z/tag loc))
               (= "ns" (sym-name (z/down loc))))
        (try (parse-ns-form (z/sexpr loc))
             (catch Exception _ {:ns-name "unknown" :aliases {}}))
        (recur (z/right loc))))))

(defn- resolve-kw
  "Resolve a raw keyword string to a fully-qualified Clojure keyword.
  - ::alias/name  -> :full.ns/name  (via require alias map)
  - ::local-name  -> :current.ns/name
  - :ns/name      -> :ns/name       (unchanged)
  - :plain        -> :plain         (unchanged)
  Returns nil if the alias is unknown."
  [kw-str ns-name aliases]
  (cond
    (and (str/starts-with? kw-str "::") (str/includes? kw-str "/"))
    (let [after (subs kw-str 2)
          slash (str/index-of after "/")
          alias-part (subs after 0 slash)
          name-part (subs after (inc slash))
          full-ns (get aliases alias-part)]
      (when full-ns (keyword full-ns name-part)))

    (str/starts-with? kw-str "::")
    (keyword ns-name (subs kw-str 2))

    :else
    (keyword (subs kw-str 1))))

(defn- extract-kw-from-vector
  "Return {:kw resolved-kw :dynamic? bool} based on the first element of vec-zloc.
  Sets :dynamic? true when the first element is not a literal keyword."
  [vec-zloc ns-name aliases]
  (when (= :vector (z/tag vec-zloc))
    (when-let [first-elem (z/down vec-zloc)]
      (if (kw-node? first-elem)
        {:kw (resolve-kw (raw first-elem) ns-name aliases) :dynamic? false}
        {:kw nil :dynamic? true}))))

(defn- handle-list
  "Detect declarations and usages from list and anonymous-fn nodes.
  Handles: reg-sub, reg-event-*, reg-fx, reg-cofx, subscribe, dispatch, dispatch-sync."
  [loc ns-name aliases file]
  (let [operator (sym-name (z/down loc))
        row (position-row loc)]
    (cond
      (contains? decl-fn->type operator)
      (let [kw-loc (z/right (z/down loc))
            decl-type (get decl-fn->type operator)]
        (when (kw-node? kw-loc)
          (when-let [resolved (resolve-kw (raw kw-loc) ns-name aliases)]
            {:decls [{:kw resolved :type decl-type :file file :row (position-row kw-loc)}]
             :usages [] :dynamics []})))

      (= "subscribe" operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls [] :usages [] :dynamics [{:form (raw loc) :file file :row row}]}
              {:decls [] :dynamics []
               :usages (when kw [{:kw kw :type :sub :file file :row row}])}))))

      (contains? dispatch-fns operator)
      (let [vec-loc (z/right (z/down loc))]
        (when (= :vector (z/tag vec-loc))
          (let [{:keys [dynamic? kw]} (extract-kw-from-vector vec-loc ns-name aliases)]
            (if dynamic?
              {:decls [] :usages [] :dynamics [{:form (raw loc) :file file :row row}]}
              {:decls [] :dynamics []
               :usages (when kw [{:kw kw :type :event :file file :row row}])}))))

      :else nil)))

(defn- handle-vector
  "Detect event usages from :fx tuple vectors.
  Handles: [:dispatch [::kw]] / [:dispatch-n [[::kw]...]] / [:dispatch-later {:dispatch [::kw]}]."
  [loc ns-name aliases file]
  (let [first-elem (z/down loc)
        row (position-row loc)]
    (when (kw-node? first-elem)
      (let [first-raw (raw first-elem)]
        (cond
          ;; [:dispatch [::kw args...]]
          (= ":dispatch" first-raw)
          (when-let [vec-loc (z/right first-elem)]
            (when (= :vector (z/tag vec-loc))
              (let [{:keys [dynamic? kw]} (extract-kw-from-vector vec-loc ns-name aliases)]
                (if dynamic?
                  {:decls [] :usages [] :dynamics [{:form (raw loc) :file file :row row}]}
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
                                              (extract-kw-from-vector ev-loc ns-name aliases))]
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
                  (if (and (kw-node? kv-loc) (= ":dispatch" (raw kv-loc)))
                    (when-let [v-loc (z/right kv-loc)]
                      (when (= :vector (z/tag v-loc))
                        (let [{:keys [kw dynamic?]} (extract-kw-from-vector v-loc ns-name aliases)]
                          (if dynamic?
                            {:decls [] :usages [] :dynamics [{:form (raw loc) :file file :row row}]}
                            (when kw {:decls [] :dynamics []
                                      :usages [{:kw kw :type :event :file file :row row}]})))))
                    (recur (z/right kv-loc)))))))

          :else nil)))))

(defn- handle-token
  "Detect usages from keyword tokens.
  Handles: :<- signal inputs in reg-sub, :on-success/:on-failure/:on-error http callbacks."
  [loc ns-name aliases file]
  (when (kw-node? loc)
    (let [raw-str (raw loc)
          row (position-row loc)]
      (cond
        ;; :<- [::dep-kw] — subscription signal input in reg-sub
        (= ":<-" raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls [] :dynamics []
                 :usages [{:kw kw :type :sub :file file :row row}]}))))

        ;; :on-success / :on-failure / :on-error [::event-kw] — http effect callbacks
        (contains? http-callback-keys raw-str)
        (when-let [vec-loc (z/right loc)]
          (when (= :vector (z/tag vec-loc))
            (let [{:keys [dynamic? kw]} (extract-kw-from-vector vec-loc ns-name aliases)]
              (when (and (not dynamic?) kw)
                {:decls [] :dynamics []
                 :usages [{:kw kw :type :event :file file :row row}]}))))

        :else nil))))

(def ^:private empty-result {:decls [] :usages [] :dynamics []})

(defn- merge-result
  [acc node-result]
  (if node-result
    {:decls (into (:decls acc) (:decls node-result))
     :usages (into (:usages acc) (:usages node-result))
     :dynamics (into (:dynamics acc) (:dynamics node-result))}
    acc))

(defn analyze-file
  "Parse a single .cljs/.cljc file and return {:declarations :usages :dynamic-sites}."
  [file]
  (let [file-path (str file)
        zloc (try (z/of-file file {:track-position? true})
                  (catch Exception e
                    (binding [*out* *err*]
                      (println "WARN: could not parse" file-path ":" (.getMessage e)))
                    nil))]
    (when zloc
      (let [{:keys [aliases ns-name] :or {ns-name "unknown" aliases {}}}
            (or (find-ns-info zloc) {:ns-name "unknown" :aliases {}})]
        (loop [loc zloc
               result empty-result]
          (if (z/end? loc)
            {:declarations (:decls result)
             :usages (:usages result)
             :dynamic-sites (:dynamics result)}
            (let [tag (z/tag loc)
                  node-result (cond
                                ;; :fn handles #(dispatch ...) anonymous function shorthand
                                (#{:list :fn} tag) (handle-list loc ns-name aliases file-path)
                                (= :vector tag) (handle-vector loc ns-name aliases file-path)
                                (= :token tag) (handle-token loc ns-name aliases file-path)
                                :else nil)]
              (recur (z/next loc) (merge-result result node-result)))))))))

(defn find-source-files
  "Recursively find all .cljs and .cljc files under root-dir."
  [root-dir]
  (->> (file-seq (io/file root-dir))
       (filter #(and (.isFile %)
                     (let [file-name (.getName %)]
                       (or (str/ends-with? file-name ".cljs")
                           (str/ends-with? file-name ".cljc")))))))

(defn analyze-project
  "Analyze all ClojureScript source files under root-dir.
  Returns {:declarations :usages :dynamic-sites} across all files."
  [root-dir]
  (let [files (find-source-files root-dir)]
    (println (str "Analyzing " (count files) " files under " root-dir " ..."))
    (let [results (keep analyze-file files)]
      {:declarations (mapcat :declarations results)
       :usages (mapcat :usages results)
       :dynamic-sites (mapcat :dynamic-sites results)})))
