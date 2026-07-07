(ns cljs-patrol.groups.a11y
  "Accessibility rule group: static checks on literal Hiccup vectors.

  Rules are conservative — we only flag when the Hiccup tag and its attribute
  map are both literals. Vectors with dynamically computed tags or attrs are
  skipped to keep the false-positive rate low."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private quoted-parent-tags
  "Parent tags that turn their child vector into a data literal, not code.
  We skip those to avoid flagging Hiccup used as test data or in macro bodies."
  #{:quote :syntax-quote :unquote :unquote-splicing})

(def ^:private dynamic-attr-tags
  "Zipper tags that indicate the attrs slot is a non-literal form: function
  calls, quoted / spliced forms, metadata-wrapped values, reader macros, etc."
  #{:list :fn :syntax-quote :unquote :unquote-splicing :reader-macro :meta})

(defn- hiccup-tag
  "Return the base HTML tag keyword from a Hiccup tag string.
  Handles plain (`:img`), class (`:img.hero`), id (`:img#logo`), and mixed
  (`:img.a.b#c`) forms. Returns nil for non-keyword tokens, namespaced
  keywords, or `::` aliases."
  [raw-str]
  (when (and raw-str
             (str/starts-with? raw-str ":")
             (not (str/starts-with? raw-str "::"))
             (not (str/includes? raw-str "/")))
    (let [body (subs raw-str 1)
          dot (str/index-of body ".")
          hash (str/index-of body "#")
          end (cond
                (and dot hash) (min dot hash)
                dot dot
                hash hash
                :else nil)
          tag-name (if end (subs body 0 end) body)]
      (when (seq tag-name)
        (keyword tag-name)))))

(defn- literal-map-keys
  "Return the set of literal keyword keys in a map zloc.
  Returns nil if the map has any non-keyword key (e.g. computed keys). Nested
  forms in values are fine — only keys need to be literal keywords for the
  presence check."
  [map-loc]
  (loop [key-loc (z/down map-loc)
         acc #{}]
    (cond
      (nil? key-loc) acc

      (not (parser/kw-node? key-loc)) nil

      :else
      (let [value-loc (z/right key-loc)]
        (recur (some-> value-loc z/right)
               (conj acc (z/sexpr key-loc)))))))

(defn- attrs-info
  "Classify the second child of a Hiccup vector.

  Returns one of:
    {:kind :absent}                       ; [:img] with no children
    {:kind :map :keys #{...}}             ; literal map — keys returned
    {:kind :map :keys nil}                ; literal map with non-kw keys
    {:kind :non-map}                      ; e.g. [:img \"child\"] — no attrs slot
    {:kind :dynamic}                      ; non-literal (e.g. (build-attrs))"
  [vec-loc]
  (let [second-child (some-> vec-loc z/down z/right)]
    (cond
      (nil? second-child) {:kind :absent}
      (= :map (z/tag second-child)) {:kind :map
                                     :keys (literal-map-keys second-child)}
      (contains? dynamic-attr-tags (z/tag second-child)) {:kind :dynamic}
      :else {:kind :non-map})))

(defn- img-alt-missing? [{:keys [kind]
                          attr-keys :keys}]
  (case kind
    :absent true
    :non-map true
    :map (and (some? attr-keys) (not (contains? attr-keys :alt)))
    :dynamic false))

(defn- inside-quoted-form? [loc]
  (some-> loc z/up z/tag quoted-parent-tags boolean))

(defn- handle-vector [loc _ns-name _aliases file]
  (let [first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (inside-quoted-form? loc)))
      (when-let [tag (hiccup-tag (parser/raw first-child))]
        (when (= :img tag)
          (let [attrs (attrs-info loc)]
            (when (img-alt-missing? attrs)
              (let [[row col] (try (z/position loc) (catch Exception _ [0 1]))]
                {:decls []
                 :dynamics []
                 :usages [{:type :img-alt-missing
                           :kw tag
                           :file file
                           :row row
                           :col col}]}))))))))

(defn- analyze* [{:keys [usages]}]
  ;; The parser pools :usages across ALL enabled groups into one seq before
  ;; each group's `analyze` is called (see parser/analyze-project + core/run).
  ;; This filter is REQUIRED to keep other groups' usages (:sub, :event,
  ;; :style-call, ...) out of the a11y result — not defensive code.
  {:img-alt-missing (filterv #(= :img-alt-missing (:type %)) usages)})

(defn- summary-lines* [{:keys [img-alt-missing]}]
  [["Img missing alt:" (count img-alt-missing)]])

(defn- failed?* [{:keys [img-alt-missing]}]
  (seq img-alt-missing))

(defrecord A11yGroup []
  group/RuleGroup
  (group-id [_] :a11y)
  (group-name [_] "A11y")
  (parse-handlers [_] {:handle-vector handle-vector})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:img-alt-missing
     (str "Every :img must set :alt. Use :alt \"\" for images that are purely decorative, "
          "otherwise supply text that conveys the image's meaning to assistive technologies. "
          "See: WCAG 2.1 SC 1.1.1 Non-text Content — "
          "https://www.w3.org/WAI/WCAG21/Understanding/non-text-content")})
  (rule->tier [_]
    {:img-alt-missing :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->A11yGroup))
