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

(defn- img-alt-missing? [{:keys [kind attrs]} tag]
  (when (= :img tag)
    (case kind
      :absent true
      :non-map true
      :map (and (some? attrs) (not (contains? attrs :alt)))
      :dynamic false)))

(defn- non-positive-int? [x]
  (and (integer? x) (<= x 0)))

(defn- invalid-tabindex-value?
  "True when `value-loc` holds a literal value that is NOT a valid tabindex.
  Valid values are 0, negative integers, and nil (Reagent omits the attribute).
  Non-literal values (symbols, function calls, reader macros) are treated as
  unknown and skipped."
  [value-loc]
  (when value-loc
    (case (z/tag value-loc)
      (:token :multi-line)
      (let [sexpr (try (z/sexpr value-loc) (catch Exception _ ::skip))]
        (cond
          (= sexpr ::skip) false
          (nil? sexpr) false
          (symbol? sexpr) false
          (non-positive-int? sexpr) false
          :else true))
      false)))

(defn- invalid-tabindex? [{:keys [kind attrs]}]
  (when (and (= kind :map) (some? attrs))
    (or (invalid-tabindex-value? (get attrs :tab-index))
        (invalid-tabindex-value? (get attrs :tabIndex)))))

(defn- handle-vector [loc _ns-name _aliases file]
  (let [first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (hiccup/inside-quoted-form? loc)))
      (when-let [tag (hiccup/parse-tag (parser/raw first-child))]
        (let [info (hiccup/attrs-info loc)
              [row col] (try (z/position loc) (catch Exception _ [0 1]))
              base {:kw tag
                    :file file
                    :row row
                    :col col}
              usages (cond-> []
                       (img-alt-missing? info tag)
                       (conj (assoc base :type :img-alt-missing))

                       (invalid-tabindex? info)
                       (conj (assoc base :type :invalid-tabindex)))]
          (when (seq usages)
            {:decls []
             :dynamics []
             :usages usages}))))))

(defn- analyze* [{:keys [usages]}]
  ;; The parser pools :usages across ALL enabled groups into one seq before
  ;; each group's `analyze` is called (see parser/analyze-project + core/run).
  ;; This filter is REQUIRED to keep other groups' usages (:sub, :event,
  ;; :style-call, ...) out of the a11y result — not defensive code.
  (let [by-type (group-by :type usages)]
    {:img-alt-missing (vec (:img-alt-missing by-type))
     :invalid-tabindex (vec (:invalid-tabindex by-type))}))

(defn- summary-lines* [{:keys [img-alt-missing invalid-tabindex]}]
  [["Img missing alt:" (count img-alt-missing)]
   ["Invalid tabindex:" (count invalid-tabindex)]])

(defn- failed?* [{:keys [img-alt-missing invalid-tabindex]}]
  (or (seq img-alt-missing) (seq invalid-tabindex)))

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
          "https://www.w3.org/WAI/WCAG21/Understanding/non-text-content")
     :invalid-tabindex
     (str "tabindex must be 0 or a negative integer. Positive integers break the natural "
          "focus order; non-integer values (strings, floats, booleans, keywords) may not "
          "produce a focusable element at all. "
          "See: WCAG 2.1 SC 2.4.3 Focus Order — "
          "https://www.w3.org/WAI/WCAG21/Understanding/focus-order")})
  (rule->tier [_]
    {:img-alt-missing :bugs
     :invalid-tabindex :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->A11yGroup))
