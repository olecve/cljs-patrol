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
    (if (contains? doc-heads (some-> loc z/down parser/sym-name))
      (let [candidate (some-> loc z/down z/right z/right)]
        (when (and candidate (string-value candidate) (z/right candidate)) candidate))
      (when (= "defprotocol" (some-> loc z/up z/down parser/sym-name))
        (let [candidate (last (parser/written-child-locs loc))]
          (when (string-value candidate) candidate))))))

(def ^:private attr-heads #{"defn" "defn-" "defmacro" "defmulti" "defprotocol"})

(defn- doc-map? [loc]
  (let [parent (z/up loc)]
    (or (and (= :meta (some-> parent z/tag)) (= loc (z/down parent)))
        (when (and (= :list (some-> parent z/tag))
                   (contains? attr-heads (some-> parent z/down parser/sym-name)))
          (let [candidate (some-> parent z/down z/right z/right)
                attrs (if (and candidate (string-value candidate)) (z/right candidate) candidate)]
            (= loc attrs))))))

(defn- doc-map-value? [loc]
  (let [parent (z/up loc)]
    (and (= :map (some-> parent z/tag))
         (= ":doc" (some-> loc z/left parser/raw))
         (doc-map? parent))))

(defn- strings [loc]
  (let [head (when (= :list (z/tag loc)) (some-> loc z/down parser/sym-name))]
    (cond
      (or (= :uneval (z/tag loc)) (contains? #{"ns" "comment"} head)) []
      (string-value loc) (when-not (doc-map-value? loc) [loc])
      :else (let [doc (docstring-loc loc)]
              (->> (parser/written-child-locs loc)
                   (remove #(= % doc))
                   (mapcat strings))))))

(defn- handle-file [allowed paths loc ns-info file]
  (when-not (tests/test-file? paths ns-info file)
    (let [forms (take-while some? (iterate z/right loc))
          allowed? (contains? allowed (some-> (:ns-name ns-info) symbol))
          tokens (when allowed?
                   (for [form forms
                         value-loc (strings form)
                         :let [definition (z/up value-loc)
                               value (string-value value-loc)]
                         :when (and (= :list (z/tag definition))
                                    (= "def" (some-> definition z/down parser/sym-name))
                                    (re-matches exact-hex value))]
                     {:type :color-token
                      :value (normalize-hex value)
                      :token (symbol (:ns-name ns-info) (parser/sym-name (parser/declared-name-loc definition)))}))
          literals (when-not allowed?
                     (for [form forms
                           string-loc (strings form)
                           :let [text (string-value string-loc)]
                           literal (concat (re-seq hex-pattern text)
                                           (when (re-find function-pattern text) [text]))
                           :let [[row col] (parser/position string-loc)]]
                       {:type :color-literal
                        :kw :color-literal
                        :top-level (parser/top-level-name string-loc)
                        :literal literal
                        :form (pr-str text)
                        :file file
                        :row row
                        :col col}))]
      {:usages (concat tokens literals)})))

(defn- analyze* [allowed {:keys [usages]}]
  (let [tokens (group-by :value (filter #(= :color-token (:type %)) usages))
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
