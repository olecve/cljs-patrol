(ns cljs-patrol.groups.a11y
  "Accessibility rule group: static checks on literal Hiccup vectors.

  Rules are conservative — we only flag when the Hiccup tag and its attribute
  map are both literals. Vectors with dynamically computed tags or attrs are
  skipped to keep the false-positive rate low."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private snippet-max-length 120)

(defn- source-snippet
  "Return a display-friendly snippet of loc's source form.
  Whitespace is collapsed to single spaces, then truncated to
  `snippet-max-length` with an ellipsis if needed. Used as the finding's
  `:form` field so reporters show the actual Hiccup vector instead of
  just the tag."
  [loc]
  (let [raw (try (z/string loc) (catch Exception _ ""))
        collapsed (str/replace raw #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

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

(def ^:private non-interactive-tags
  "HTML tags that carry no built-in click / keyboard semantics.
  Attaching a mouse / pointer interaction to these without a :role hint or
  a keyboard handler produces something that looks clickable but isn't
  reachable via keyboard."
  #{:div :span :li :p :section :article :header :footer :main :aside})

(def ^:private interaction-keys
  "Attribute keys that attach a mouse / pointer / touch interaction.
  Includes both kebab-case (Reagent idiomatic) and camelCase (React-style)
  spellings."
  #{:on-click :onClick
    :on-mouse-down :onMouseDown
    :on-mouse-up :onMouseUp
    :on-pointer-down :onPointerDown
    :on-pointer-up :onPointerUp
    :on-touch-start :onTouchStart
    :on-touch-end :onTouchEnd})

(def ^:private keyboard-handler-keys
  #{:on-key-down :on-key-press :on-key-up
    :onKeyDown :onKeyPress :onKeyUp})

(def ^:private no-op-role-values
  "Role values that don't confer interactive semantics.
  Either effectively absent (nil, empty string) or explicitly remove
  semantics (\"presentation\", \"none\")."
  #{nil "" "presentation" "none"})

(defn- literal-sexpr
  "Return the sexpr of value-loc when it holds a literal token or string.
  Returns `::absent` when value-loc is nil, or `::non-literal` for anything
  else (lists, maps, symbols, meta forms, reader macros)."
  [value-loc]
  (if (nil? value-loc)
    ::absent
    (case (z/tag value-loc)
      (:token :multi-line)
      (try (z/sexpr value-loc) (catch Exception _ ::non-literal))
      ::non-literal)))

(defn- meaningful-role?
  "True when attrs has a :role value that confers interactive semantics.
  Non-literal values (variables, expressions) are optimistically accepted."
  [attrs]
  (let [v (literal-sexpr (get attrs :role))]
    (cond
      (= v ::absent) false
      (= v ::non-literal) true
      :else (not (contains? no-op-role-values v)))))

(defn- meaningful-handler?
  "True when value-loc holds a handler that could actually respond.
  Anything literally nil / false is treated as a no-op; non-literal values
  are optimistically accepted."
  [value-loc]
  (let [v (literal-sexpr value-loc)]
    (cond
      (= v ::absent) false
      (= v ::non-literal) true
      :else (not (or (nil? v) (false? v))))))

(defn- has-meaningful-handler? [attrs handler-keys]
  (some #(meaningful-handler? (get attrs %)) handler-keys))

(defn- on-click-on-non-interactive? [{:keys [kind attrs]} tag]
  (when (and (contains? non-interactive-tags tag)
             (= :map kind)
             (some? attrs))
    (and (has-meaningful-handler? attrs interaction-keys)
         (not (meaningful-role? attrs))
         (not (has-meaningful-handler? attrs keyboard-handler-keys)))))

(def ^:private empty-interactive-tags #{:a :button})

(def ^:private text-name-keys
  "Attribute keys that give a screen-reader-readable name to an element."
  #{:aria-label :aria-labelledby :title})

(defn- meaningful-text-name?
  "True when attrs supplies a non-empty accessible name via aria-label,
  aria-labelledby, or title. Non-literal values are optimistically accepted."
  [attrs]
  (some (fn [k]
          (let [v (literal-sexpr (get attrs k))]
            (cond
              (= v ::absent) false
              (= v ::non-literal) true
              (or (nil? v) (false? v)) false
              (and (string? v) (empty? v)) false
              :else true)))
        text-name-keys))

(defn- empty-interactive? [{:keys [kind attrs]} tag loc]
  (when (and (contains? empty-interactive-tags tag)
             (not (hiccup/has-body? loc)))
    (case kind
      :absent true
      :map (and (some? attrs) (not (meaningful-text-name? attrs)))
      false)))

(defn- handle-vector [loc _ns-name _aliases file]
  (let [first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (hiccup/inside-quoted-form? loc))
               (not (hiccup/inside-style-decl? loc)))
      (when-let [tag (hiccup/parse-tag (parser/raw first-child))]
        (let [info (hiccup/attrs-info loc)
              [row col] (try (z/position loc) (catch Exception _ [0 1]))
              base {:kw tag
                    :form (source-snippet loc)
                    :file file
                    :row row
                    :col col}
              usages (cond-> []
                       (img-alt-missing? info tag)
                       (conj (assoc base :type :img-alt-missing))

                       (invalid-tabindex? info)
                       (conj (assoc base :type :invalid-tabindex))

                       (on-click-on-non-interactive? info tag)
                       (conj (assoc base :type :on-click-on-non-interactive))

                       (empty-interactive? info tag loc)
                       (conj (assoc base :type :empty-interactive-element)))]
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
     :invalid-tabindex (vec (:invalid-tabindex by-type))
     :on-click-on-non-interactive (vec (:on-click-on-non-interactive by-type))
     :empty-interactive-element (vec (:empty-interactive-element by-type))}))

(defn- summary-lines* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive empty-interactive-element]}]
  [["Img missing alt:" (count img-alt-missing)]
   ["Invalid tabindex:" (count invalid-tabindex)]
   ["Onclick on non-interactive:" (count on-click-on-non-interactive)]
   ["Empty interactive element:" (count empty-interactive-element)]])

(defn- failed?* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive empty-interactive-element]}]
  (or (seq img-alt-missing)
      (seq invalid-tabindex)
      (seq on-click-on-non-interactive)
      (seq empty-interactive-element)))

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
          "https://www.w3.org/WAI/WCAG21/Understanding/focus-order")
     :on-click-on-non-interactive
     (str "A mouse / pointer / touch handler (:on-click, :on-mouse-down, :on-pointer-*, "
          ":on-touch-*, ...) is attached to a non-interactive tag (:div, :span, :li, :p, "
          ":section, ...) with no keyboard equivalent — mouse users can trigger it but "
          "keyboard users cannot. Either switch to a natively interactive tag (:button, "
          "or :a with :href), or add :role (\"button\", \"link\") or a keyboard handler "
          "(:on-key-down / :on-key-press / :on-key-up) — WCAG recommends both. Note: "
          ":role \"presentation\" / \"none\" / nil / \"\" don't count as valid roles. "
          "See: WCAG 2.1 SC 2.1.1 Keyboard — "
          "https://www.w3.org/WAI/WCAG21/Understanding/keyboard")
     :empty-interactive-element
     (str "A :button or :a element has no visible text and no :aria-label / "
          ":aria-labelledby / :title — screen readers announce nothing. Add text content, "
          "or provide an accessible name via :aria-label (e.g. for icon-only buttons). "
          "See: WCAG 2.1 SC 4.1.2 Name, Role, Value — "
          "https://www.w3.org/WAI/WCAG21/Understanding/name-role-value")})
  (rule->tier [_]
    {:img-alt-missing :bugs
     :invalid-tabindex :bugs
     :on-click-on-non-interactive :bugs
     :empty-interactive-element :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->A11yGroup))
