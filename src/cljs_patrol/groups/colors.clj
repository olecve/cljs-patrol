(ns cljs-patrol.groups.colors
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.tests :as tests]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private hex-pattern #"(?i)(?<![^ (,])#[0-9a-f]{3}(?:[0-9a-f]{5}|[0-9a-f]{3}|[0-9a-f])?(?=\z|[ ),;])")
(def ^:private exact-hex #"(?i)#[0-9a-f]{3}(?:[0-9a-f]{5}|[0-9a-f]{3}|[0-9a-f])?")
(def ^:private function-pattern #"(?i)(?:rgba?|hsla?)\(")
(def ^:private doc-heads #{"def" "defonce" "defn" "defn-" "defmacro" "defmulti" "defprotocol"})

(defn- unwrap-meta [loc]
  (if (= :meta (z/tag loc))
    (recur (last (parser/written-child-locs loc)))
    loc))

(defn- effective-children [loc]
  (map unwrap-meta (parser/written-child-locs loc)))

(defn- branch-reader [loc]
  (let [reader (some-> loc z/up z/up)]
    (when (some #(= loc (second %)) (parser/reader-conditional-branches reader))
      reader)))

(defn- splice-reader [loc]
  (let [parent (z/up loc)]
    (cond
      (and (= :meta (z/tag parent)) (= loc (last (parser/written-child-locs parent)))) (recur parent)
      :else (when-let [reader (branch-reader loc)]
              (if (= "?@" (parser/reader-conditional-kind reader))
                reader
                (recur reader))))))

(defn- reader-site [loc]
  (if-let [wrapper (or (branch-reader loc) (splice-reader (z/up loc)))]
    (recur wrapper)
    loc))

(defn- enclosing-form [loc]
  (let [parent (z/up (reader-site loc))]
    (if (= :meta (z/tag parent))
      (recur parent)
      parent)))

(defn- active-branch? [feature loc]
  (loop [current loc]
    (if-let [parent (z/up current)]
      (if-let [reader (branch-reader current)]
        (and (= current (parser/reader-conditional-branch reader feature)) (recur reader))
        (recur parent))
      true)))

(defn- projected-forms [feature loc]
  (let [form (unwrap-meta loc)]
    (if-let [kind (parser/reader-conditional-kind form)]
      (when-let [branch (parser/reader-conditional-branch form feature)]
        (let [branches (projected-forms feature branch)]
          (if (= "?@" kind)
            (->> branches
                 (mapcat parser/written-child-locs)
                 (mapcat (partial projected-forms feature)))
            branches)))
      [form])))

(defn- projected-children [feature loc]
  (mapcat (partial projected-forms feature) (parser/written-child-locs loc)))

(defn- list-head [loc]
  (when (= :list (z/tag loc))
    (some-> loc effective-children first parser/sym-name)))

(defn- projected-list-head [features loc]
  (when (= :list (z/tag loc))
    (some->> (projected-children features loc) first parser/sym-name)))

(defn- string-value [loc]
  (when (contains? #{:token :multi-line} (z/tag loc))
    (let [value (z/sexpr loc)]
      (when (string? value) value))))

(defn- normalize-hex [text]
  (let [lower (str/lower-case text)]
    (if (contains? #{4 5} (count lower))
      (str "#" (apply str (mapcat #(repeat 2 %) (subs lower 1))))
      lower)))

(defn- docstring-loc [feature loc]
  (when (= :list (z/tag loc))
    (if (contains? doc-heads (projected-list-head feature loc))
      (let [[candidate & following] (drop 2 (projected-children feature loc))]
        (when (and candidate (string-value candidate) (seq following)) candidate))
      (when (= "defprotocol" (projected-list-head feature (enclosing-form loc)))
        (let [candidate (last (projected-children feature loc))]
          (when (string-value candidate) candidate))))))

(def ^:private attr-heads #{"defn" "defn-" "defmacro" "defmulti" "defprotocol"})

(defn- doc-map? [feature loc]
  (let [site (reader-site loc)
        parent (z/up site)
        form (enclosing-form loc)
        head (projected-list-head feature form)]
    (or (and (= :meta (z/tag parent)) (= site (first (parser/written-child-locs parent))))
        (when (contains? attr-heads head)
          (let [children (drop 2 (projected-children feature form))
                [attrs & body] (if (some-> children first string-value) (rest children) children)]
            (or (= loc attrs)
                (and (contains? #{"defn" "defn-" "defmacro"} head)
                     (= :list (some-> (if (= :map (z/tag attrs)) (first body) attrs) z/tag))
                     (= loc (last body)))))))))

(defn- doc-map-value? [feature loc parent]
  (and (= :map (z/tag parent))
       (doc-map? feature parent)
       (some (fn [[key value]] (and (= loc value) (= ":doc" (parser/raw key))))
             (partition 2 (projected-children feature parent)))))

(defn- skipped-context? [features loc]
  (->> (iterate z/up loc)
       (take-while some?)
       (some #(contains? #{"ns" "comment"} (projected-list-head features %)))))

(defn- active-feature-sets [loc feature-sets]
  (or (seq (filter #(active-branch? % loc) feature-sets))
      (when (active-branch? #{} loc) [#{}])))

(defn- documentation? [loc active-features]
  (let [parent (enclosing-form loc)]
    (and (seq active-features)
         (every? (fn [feature]
                   (or (= loc (docstring-loc feature parent))
                       (doc-map-value? feature loc parent)
                       (skipped-context? feature loc)))
                 active-features))))

(defn- strings [loc]
  (cond
    (or (= :uneval (z/tag loc))
        (contains? #{"ns" "comment"} (list-head (unwrap-meta loc)))) []
    (string-value loc) [loc]
    :else (mapcat strings (parser/written-child-locs loc))))

(defn- form-features [form]
  (let [custom (into #{}
                     (comp (mapcat parser/reader-conditional-branches)
                           (map (comp parser/raw first))
                           (remove #{":clj" ":cljs" ":default"}))
                     (tree-seq some? parser/written-child-locs form))
        feature-count (count custom)
        combinations (cond-> (into [#{}] (map hash-set) (sort custom))
                       (= 2 feature-count) (conj custom))]
    {:complete? (<= feature-count 2)
     :sets (for [language [":cljs" ":clj"]
                 features combinations]
             (conj features language))}))

(defn- top-level-name [loc features]
  (let [top (parser/top-level-loc loc)
        form (unwrap-meta top)
        definition (if (parser/reader-conditional-kind form)
                     (->> (iterate z/up loc)
                          (take-while #(not= % top))
                          (filter (partial projected-list-head features))
                          last)
                     form)]
    (when-let [head (projected-list-head features definition)]
      (let [[_ id] (projected-children features definition)]
        (if (str/starts-with? head "def")
          (parser/sym-name id)
          (when (parser/kw-node? id) (parser/raw id)))))))

(defn- non-executable-definition? [loc]
  (let [ancestors (->> (iterate z/up (z/up loc))
                       (take-while some?)
                       reverse)]
    (loop [[parent & remaining] ancestors
           syntax-depth 0]
      (let [tag (z/tag parent)
            head (when (= :list tag) (some->> (projected-children #{":cljs"} parent) first parser/raw))]
        (cond
          (nil? parent) (pos? syntax-depth)
          (= :syntax-quote tag) (recur remaining (inc syntax-depth))
          (or (contains? #{:unquote :unquote-splicing} tag)
              (contains? #{"clojure.core/unquote" "clojure.core/unquote-splicing"} head))
          (or (zero? syntax-depth) (recur remaining (dec syntax-depth)))
          (and (zero? syntax-depth) (or (= :quote tag) (contains? #{"quote" "comment"} head))) true
          :else (recur remaining syntax-depth))))))

(defn- handle-file [allowed paths loc ns-info file]
  (when-not (tests/test-file? paths ns-info file)
    (let [forms (take-while some? (iterate z/right loc))
          allowed? (contains? allowed (some-> (:ns-name ns-info) symbol))
          tokens (when allowed?
                   (for [form forms
                         value-loc (strings form)
                         :let [definition (enclosing-form value-loc)
                               value (string-value value-loc)]
                         :when (and (= "def" (projected-list-head #{":cljs"} definition))
                                    (re-matches exact-hex value)
                                    (active-branch? #{":cljs"} value-loc))
                         :let [[_ id & values] (projected-children #{":cljs"} definition)
                               token-name (parser/sym-name id)]
                         :when (and token-name
                                    (= value-loc (last values))
                                    (not (non-executable-definition? definition)))]
                     {:type :color-token
                      :value (normalize-hex value)
                      :token (symbol (:ns-name ns-info) token-name)}))
          literals (when-not allowed?
                     (let [cljc? (str/ends-with? file ".cljc")]
                       (for [form forms
                             :let [{:keys [complete? sets]} (if cljc?
                                                              (form-features form)
                                                              {:complete? true
                                                               :sets [#{":cljs"}]})]
                             string-loc (strings form)
                             :let [text (string-value string-loc)
                                   matches (concat (re-seq hex-pattern text)
                                                   (when (re-find function-pattern text) [text]))]
                             :when (seq matches)
                             :let [active-features (active-feature-sets string-loc sets)]
                             :when (not (and complete? (documentation? string-loc active-features)))
                             :let [[row col] (parser/position string-loc)]
                             literal matches]
                         {:type :color-literal
                          :kw :color-literal
                          :top-level (top-level-name string-loc (or (first active-features) #{":cljs"}))
                          :literal literal
                          :form (pr-str text)
                          :file file
                          :row row
                          :col col})))]
      {:usages (concat tokens literals)})))

(defn- analyze* [allowed {:keys [usages usages-in-run]}]
  (let [tokens (group-by :value (filter #(= :color-token (:type %)) (or usages-in-run usages)))
        fallback (if (seq allowed)
                   (str "Use a token from " (str/join ", " (sort allowed)) ".")
                   "Use a token from an allowed namespace.")]
    {:color-literal
     (mapv (fn [{:keys [literal]
                 :as finding}]
             (let [matches (when (re-matches exact-hex literal)
                             (->> (get tokens (normalize-hex literal)) (map :token) distinct (sort-by str)))]
               (assoc finding :hint (if (seq matches)
                                      (str "Use " (str/join ", " matches) ".")
                                      fallback))))
           (filter #(= :color-literal (:type %)) usages))}))

(defrecord ColorsGroup [allowed paths]
  group/RuleGroup
  (group-id [_] :colors)
  (group-name [_] "Colors")
  (parse-handlers [_] (if allowed {:handle-file (partial handle-file allowed paths)} {}))
  (analyze [_ data] (analyze* allowed data))
  (summary-lines [_ result] (when allowed [["Color literals:" (count (:color-literal result))]]))
  (failed? [_ _] false)
  (suggestions [_] {:color-literal "A color literal sits outside the token namespaces. Use a design token."})
  (docs-anchors [_] {:color-literal "color-literal"})
  (rule->tier [_] {:color-literal :cleanup})
  (file-extensions [_] group/cljs-extensions))

(defn make-group
  ([] (make-group nil nil))
  ([config] (make-group config nil))
  ([config paths]
   (when-not (or (nil? config)
                 (and (map? config)
                      (= #{:allowed-namespaces} (set (keys config)))
                      (set? (:allowed-namespaces config))
                      (every? simple-symbol? (:allowed-namespaces config))))
     (throw (ex-info (str ":colors :allowed-namespaces must be a set of namespace symbols, got " (pr-str config))
                     {:colors config})))
   (->ColorsGroup (:allowed-namespaces config) (tests/normalize-paths paths))))
