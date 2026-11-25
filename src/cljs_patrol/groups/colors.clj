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

(defn- enclosing-form [loc]
  (loop [parent (z/up loc)]
    (if (= :meta (z/tag parent))
      (recur (z/up parent))
      parent)))

(defn- list-head [loc]
  (when (= :list (z/tag loc))
    (some-> loc effective-children first parser/sym-name)))

(defn- string-value [loc]
  (when (contains? #{:token :multi-line} (z/tag loc))
    (let [value (z/sexpr loc)]
      (when (string? value) value))))

(defn- normalize-hex [text]
  (let [lower (str/lower-case text)]
    (if (contains? #{4 5} (count lower))
      (str "#" (apply str (mapcat #(repeat 2 %) (subs lower 1))))
      lower)))

(defn- docstring-loc [loc]
  (when (= :list (z/tag loc))
    (if (contains? doc-heads (list-head loc))
      (let [[candidate & following] (drop 2 (effective-children loc))]
        (when (and candidate (string-value candidate) (seq following)) candidate))
      (when (= "defprotocol" (list-head (enclosing-form loc)))
        (let [candidate (last (effective-children loc))]
          (when (string-value candidate) candidate))))))

(def ^:private attr-heads #{"defn" "defn-" "defmacro" "defmulti" "defprotocol"})

(defn- doc-map? [loc]
  (let [parent (z/up loc)
        form (enclosing-form loc)
        head (list-head form)]
    (or (and (= :meta (z/tag parent)) (= loc (first (parser/written-child-locs parent))))
        (when (contains? attr-heads head)
          (let [children (drop 2 (effective-children form))
                [attrs & body] (if (some-> children first string-value) (rest children) children)]
            (or (= loc attrs)
                (and (contains? #{"defn" "defn-" "defmacro"} head)
                     (= :list (some-> (if (= :map (z/tag attrs)) (first body) attrs) z/tag))
                     (= loc (last body)))))))))

(defn- doc-map-value? [loc parent]
  (and (= :map (z/tag parent))
       (doc-map? parent)
       (some (fn [[key value]] (and (= loc value) (= ":doc" (parser/raw key))))
             (partition 2 (effective-children parent)))))

(defn- strings [loc]
  (cond
    (or (= :uneval (z/tag loc))
        (contains? #{"ns" "comment"} (list-head (unwrap-meta loc)))) []
    (string-value loc) (let [parent (enclosing-form loc)]
                         (when-not (or (= loc (docstring-loc parent)) (doc-map-value? loc parent))
                           [loc]))
    :else (mapcat strings (parser/written-child-locs loc))))

(defn- top-level-name [loc]
  (let [top (parser/top-level-loc loc)
        form (unwrap-meta top)
        definition (if (= :reader-macro (z/tag form))
                     (->> (iterate z/up loc)
                          (take-while #(not= % top))
                          (filter list-head)
                          last)
                     form)]
    (when-let [head (list-head definition)]
      (let [[_ id] (effective-children definition)]
        (if (str/starts-with? head "def")
          (parser/sym-name id)
          (when (parser/kw-node? id) (parser/raw id)))))))

(defn- handle-file [allowed paths loc ns-info file]
  (when-not (tests/test-file? paths ns-info file)
    (let [forms (take-while some? (iterate z/right loc))
          allowed? (contains? allowed (some-> (:ns-name ns-info) symbol))
          tokens (when allowed?
                   (for [form forms
                         value-loc (strings form)
                         :let [definition (enclosing-form value-loc)
                               value (string-value value-loc)]
                         :when (and (= "def" (list-head definition)) (re-matches exact-hex value))
                         :let [[_ id & values] (effective-children definition)
                               token-name (parser/sym-name id)]
                         :when (and token-name (= value-loc (last values)))]
                     {:type :color-token
                      :value (normalize-hex value)
                      :token (symbol (:ns-name ns-info) token-name)}))
          literals (when-not allowed?
                     (for [form forms
                           string-loc (strings form)
                           :let [text (string-value string-loc)]
                           literal (concat (re-seq hex-pattern text)
                                           (when (re-find function-pattern text) [text]))
                           :let [[row col] (parser/position string-loc)]]
                       {:type :color-literal
                        :kw :color-literal
                        :top-level (top-level-name string-loc)
                        :literal literal
                        :form (pr-str text)
                        :file file
                        :row row
                        :col col}))]
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
   (->ColorsGroup (:allowed-namespaces config) paths)))
