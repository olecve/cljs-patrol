(ns cljs-patrol.groups.a11y
  "Accessibility rule group: static checks on literal Hiccup vectors.

  Rules are conservative — we only flag when the Hiccup tag and its attribute
  map are both literals. Vectors with dynamically computed tags or attrs are
  skipped to keep the false-positive rate low."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [rewrite-clj.zip :as z]))

(defn- img-alt-missing? [{:keys [kind attrs]}]
  (case kind
    :absent true
    :non-map true
    :map (and (some? attrs) (not (contains? attrs :alt)))
    :dynamic false))

(defn- handle-vector [loc _ns-name _aliases file]
  (let [first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (hiccup/inside-quoted-form? loc)))
      (when-let [tag (hiccup/parse-tag (parser/raw first-child))]
        (when (= :img tag)
          (let [info (hiccup/attrs-info loc)]
            (when (img-alt-missing? info)
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
