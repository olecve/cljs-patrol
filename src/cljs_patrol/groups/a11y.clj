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

(def ^:private snippet-edn-max
  ;; Chars available for `(pr-str snippet)` — the double-quoted, escaped form
  ;; that ends up in `.cljs-patrol/baseline.edn`. Sized so the surrounding
  ;; `   :form "…"}]}` (last-issue tail case) never exceeds 120 columns.
  107)

(defn- source-snippet
  "Return a display-friendly snippet of loc's source form.

  Whitespace is collapsed to single spaces, then truncated with an ellipsis
  so that `(pr-str snippet)` fits in `snippet-edn-max` chars — i.e. the
  finding's baseline line stays under 120 columns even after EDN escapes
  `\"` → `\\\"` and similar."
  [loc]
  (let [raw (try (z/string loc) (catch Exception _ ""))
        collapsed (str/replace raw #"\s+" " ")]
    (if (<= (count (pr-str collapsed)) snippet-edn-max)
      collapsed
      (loop [n (max 3 (dec (count collapsed)))]
        (let [candidate (str (subs collapsed 0 n) "...")]
          (if (<= (count (pr-str candidate)) snippet-edn-max)
            candidate
            (recur (dec n))))))))

(defn- img-alt-missing? [{:keys [kind attrs]} tag]
  (when (= :img tag)
    (case kind
      :absent true
      :non-map true
      :map (and (some? attrs) (not (contains? attrs :alt)))
      (:dynamic :dynamic-map) false)))

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
  #{:div :span :li :p :section :article :header :footer :main :aside :svg})

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

(def ^:private attrs-readable-kinds
  "Classifications whose `:attrs` can answer whether a key is present.
  `:dynamic-map` is a floor rather than the whole map, so it is enough to spot an
  interaction key and never enough to call one missing."
  #{:map :dynamic-map})

(defn- on-click-on-non-interactive? [{:keys [kind attrs]} tag]
  (when (and (contains? non-interactive-tags tag)
             (contains? attrs-readable-kinds kind)
             (some? attrs))
    (and (has-meaningful-handler? attrs interaction-keys)
         (not (meaningful-role? attrs))
         (not (has-meaningful-handler? attrs keyboard-handler-keys)))))

(def ^:private empty-interactive-tags #{:a :button})

(def ^:private empty-interactive-roles
  "ARIA role values that make an element behave as a button or link.
  Both string and keyword literals are accepted — Reagent stringifies
  keyword attribute values, so `:role :button` is equivalent to
  `:role \"button\"` at runtime."
  #{"button" "link" :button :link})

(def ^:private text-name-keys
  "Attribute keys that give a screen-reader-readable name to an element."
  #{:aria-label :aria-labelledby :title})

(defn- meaningful-text-name?
  "True when attrs supplies a non-empty accessible name.

  Checks :aria-label, :aria-labelledby, and :title. Non-literal values are optimistically accepted."
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

(defn- interactive-via-role? [attrs]
  (contains? empty-interactive-roles (literal-sexpr (get attrs :role))))

(defn- literal-string-loc? [loc]
  (and loc
       (contains? #{:token :multi-line} (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))))

(def ^:private branching-forms
  "Forms whose branches each render, so a body built only of them is still icon-only."
  #{"cond" "if" "if-not" "when" "when-not" "when-let" "when-some"})

(defn- right-locs [loc]
  (when loc (cons loc (lazy-seq (right-locs (z/right loc))))))

(defn- branch-result-locs
  "Locs a branching form can render, with tests and bindings dropped.
  `cond` alternates test and result, so only odd positions render; the rest put
  every argument after the first in the body."
  [head-str first-arg]
  (let [args (right-locs first-arg)]
    (if (= "cond" head-str)
      (keep-indexed (fn [i loc] (when (odd? i) loc)) args)
      (rest args))))

(declare icon-only-branches? visible-from?)

(defn- image-alt-name?
  "True when loc is an `[:img …]` carrying non-empty `:alt` text.
  Alt text is announced, so an image holding it names whatever control it sits in."
  [loc attrs]
  (and (some? attrs)
       (when-let [head (z/down loc)]
         (= :img (hiccup/parse-tag (parser/raw head))))
       (let [alt (literal-sexpr (get attrs :alt))]
         (and (string? alt) (seq alt)))))

(defn- visible-content?
  "True when loc carries visible text or dynamically-computed content.
  A nested Hiccup vector is treated as opaque icon markup unless it
  contains a string literal, a dynamic form, or an aria-name attribute
  on itself or a descendant."
  [loc]
  (cond
    (literal-string-loc? loc) true

    (= :vector (z/tag loc))
    (let [{:keys [slot attrs]} (hiccup/attrs-info loc)
          body-start (if slot (z/right slot) (some-> loc z/down z/right))]
      (or (when slot
            (or (meaningful-text-name? attrs)
                (image-alt-name? loc attrs)))
          (visible-from? body-start)))

    (= :list (z/tag loc))
    (not (icon-only-branches? loc))

    :else true))

(defn- visible-from?
  "True when `body-start` or any sibling to its right carries visible content."
  [body-start]
  (boolean (some visible-content? (take-while some? (iterate #(some-> % z/right) body-start)))))

(defn- icon-only-branches?
  "True when every branch a form could render is icon markup.

  A `(cond … [icons/a] … [icons/b])` in a button body reads as content to a
  reader and as nothing at all to a screen reader, so it must not count as a
  visible name. Anything else in a branch — a string, a symbol, a call that
  might produce text — makes the whole form opaque again."
  [loc]
  (and (= :list (z/tag loc))
       (let [head (z/down loc)
             head-str (when head (parser/raw head))]
         (and (contains? branching-forms head-str)
              (let [results (branch-result-locs head-str (z/right head))]
                (and (seq results)
                     (every? (fn [result]
                               (case (z/tag result)
                                 :vector (not (visible-content? result))
                                 :list (icon-only-branches? result)
                                 false))
                             results)))))))

(defn- has-visible-body?
  "True when the vector has body content producing visible text or a labelled child.
  False for structurally empty vectors, and for icon-only markup like
  `[:button [icons/x]]`."
  [vec-loc]
  (let [attrs-loc (hiccup/attrs-slot vec-loc)
        body-start (if attrs-loc
                     (z/right attrs-loc)
                     (some-> vec-loc z/down z/right))]
    (visible-from? body-start)))

(def ^:private widget-state-attrs
  "Attributes marking a control as a stateful widget rather than a plain button.
  Present, the element is left alone: it shows deliberate a11y work, and the name
  it may still lack is a different finding from an unlabelled icon button."
  #{:aria-pressed :aria-checked})

(def ^:private checkbox-role-values #{"checkbox" :checkbox})

(defn- stateful-widget? [attrs]
  (or (some (fn [k] (not= ::absent (literal-sexpr (get attrs k)))) widget-state-attrs)
      (contains? checkbox-role-values (literal-sexpr (get attrs :role)))))

(defn- empty-interactive?
  "True when an interactive element announces nothing at all.
  The body scan walks the whole subtree, so what the element is gets settled first:
  a `:div` that is not a control can never be flagged, whatever it contains."
  [{:keys [kind attrs]} tag loc]
  (let [interactive-tag? (contains? empty-interactive-tags tag)
        interactive-role? (and (= :map kind) (some? attrs) (interactive-via-role? attrs))]
    (when (and (or interactive-tag? interactive-role?)
               (not (has-visible-body? loc)))
      (if interactive-tag?
        (case kind
          :absent true
          :map (and (some? attrs)
                    (not (meaningful-text-name? attrs))
                    (not (stateful-widget? attrs)))
          false)
        (and (not (meaningful-text-name? attrs))
             (not (stateful-widget? attrs)))))))

(def ^:private aria-hidden-true-values
  "Values that take an element out of the accessibility tree.
  Reagent stringifies keyword attribute values, so all three spellings reach the DOM
  as `aria-hidden=\"true\"`."
  #{true "true" :true})

(defn- aria-hidden? [attrs]
  (contains? aria-hidden-true-values (literal-sexpr (get attrs :aria-hidden))))

(def ^:private focusable-tags
  "Tags the browser puts in the tab order with no authoring at all.

  `:a` is absent because a link earns its place there only once it has an `:href`,
  which is checked separately. `:details` is present for the `:summary` it always
  renders, which is the focusable part of a disclosure."
  #{:button :input :textarea :select :details :summary})

(def ^:private focusable-roles
  "Widget roles claiming an element the user can reach and operate.

  A role does not by itself put an element in the tab order, but claiming one and then
  hiding the element from assistive technology is the same contradiction either way:
  the role is an announcement nobody can hear. Non-widget roles are left out — `img`
  especially, since a decorative icon written `[:svg {:aria-hidden true}]` is correct
  markup and the existing missing-accessible-name rule already asks for the role to go."
  (into #{}
        (mapcat (juxt identity keyword))
        ["button" "link" "checkbox" "radio" "switch" "tab" "option" "menuitem"
         "menuitemcheckbox" "menuitemradio" "textbox" "searchbox" "combobox"
         "slider" "spinbutton" "treeitem"]))

(defn- written-value
  "Return the value attrs give `k` when it reaches the DOM, or `::absent`.

  Reagent omits an attribute whose value is nil or false, so both answer `::absent`:
  `[:a {:href nil}]` renders an `<a>` with no href. A value that cannot be read is
  written — the question is whether the attribute is there, not what it says."
  [attrs k]
  (let [v (literal-sexpr (get attrs k))]
    (if (or (= ::absent v) (nil? v) (false? v)) ::absent v)))

(defn- attr-written? [attrs k]
  (not= ::absent (written-value attrs k)))

(defn- tabindex-reading
  "Return how attrs place the element in the tab order.

  `:none` when no tabindex is written, `:removed` for a negative literal, `:tab-stop`
  for a non-negative one, and `:unknown` when one is written but cannot be read. The
  roving-tabindex idiom `:tab-index (if active? 0 -1)` is exactly that last case, and
  it is the escape hatch as often as it is not — so it is answered for neither way."
  [attrs]
  (let [written (remove #{::absent} (map #(written-value attrs %) [:tab-index :tabIndex]))]
    (cond
      (empty? written) :none
      (not (every? integer? written)) :unknown
      (some neg? written) :removed
      :else :tab-stop)))

(def ^:private disableable-tags
  "Tags whose elements a `disabled` attribute really takes out of the tab order.
  It is a form-control attribute: on a `:div` or an `:a` React renders it as a no-op and
  the element keeps its tab stop, so reading it as a way out there would switch the rule
  off on something that stays focusable."
  #{:button :input :select :textarea :fieldset :optgroup :option})

(defn- out-of-tab-order-when-disabled?
  "True when a `disabled` attribute takes this element out of the tab order.
  A value that cannot be read counts as disabled: that is the answer reporting less,
  the same posture the tabindex reading takes."
  [attrs tag]
  (and (contains? disableable-tags tag) (attr-written? attrs :disabled)))

(defn- hidden-input?
  "True when the element is an `<input type=\"hidden\">`, which renders nothing at all."
  [attrs tag]
  (and (= :input tag) (= "hidden" (literal-sexpr (get attrs :type)))))

(defn- focusable?
  "True when the element can take focus.
  Answered only from attrs that are absent or readable whole: a partial or computed map
  may hold the negative tabindex or the `disabled` that is the way out, and calling one
  missing from a partial view is what produces false reports."
  [{:keys [kind attrs]} tag]
  (let [readable (case kind
                   :map attrs
                   (:absent :non-map) {}
                   nil)]
    (boolean
     (when (some? readable)
       (let [tabindex (tabindex-reading readable)]
         (and (not (out-of-tab-order-when-disabled? readable tag))
              (not (hidden-input? readable tag))
              (contains? #{:none :tab-stop} tabindex)
              (or (contains? focusable-tags tag)
                  (and (= :a tag) (attr-written? readable :href))
                  (contains? focusable-roles (literal-sexpr (get readable :role)))
                  (= :tab-stop tabindex))))))))

(defn- aria-hidden-focusable?
  "True when an element hidden from assistive tech can still take focus itself."
  [{:keys [kind attrs]
    :as info} tag]
  (and (= :map kind) (some? attrs) (aria-hidden? attrs) (focusable? info tag)))

(def ^:private accessible-name-required-tags
  "Native tags whose element has no intrinsic accessible name.

  A visible `<label>` associated by id, `:aria-label`, or `:aria-labelledby` is
  required — `:placeholder` is a hint, not a name. `:dialog` matches native
  `<dialog>` and any wrapper aliased to `:dialog` via `:component-aliases`."
  #{:textarea :dialog})

(def ^:private accessible-name-attrs
  "Attributes that give an element a programmatic name.
  `:title` is a last-resort source but a real one, so it counts; `:placeholder`
  is a hint that disappears on typing and does not."
  #{:aria-label :aria-labelledby :title})

(def ^:private name-required-roles
  "Roles this rule requires to carry an accessible name of their own.
  Most are what WAI-ARIA marks \"Accessible Name Required: True\": `img` opts an element
  into the accessibility tree with nothing to announce; `dialog` and `alertdialog` need a
  name to stay identifiable once focus moves into them; `listbox`, `grid` and `tree` are
  containers a screen reader announces on entry, with nothing but the name to say which
  one the user has landed in.

  `tablist`, `menu` and `menubar` are stricter than the spec, which marks all three
  name-not-required. They are announced on entry exactly as the other containers are,
  and a page carrying two of them — a document's tabs beside a panel's — gives a screen
  reader user no way to tell one from the other. The spec permits the omission; this
  rule does not, and the report says so rather than citing a requirement.

  The name usually has to sit on the element itself; the one exception the check reads
  is a native `<caption>`, which names the `<table>` a `grid` role is most often written
  on. Reagent stringifies keyword attribute values at runtime, so both spellings count."
  (into #{}
        (mapcat (juxt identity keyword))
        ["dialog" "alertdialog" "img" "listbox" "grid" "tree" "tablist" "menu" "menubar"]))

(defn- has-accessible-name? [attrs]
  (some (fn [k]
          (let [v (literal-sexpr (get attrs k))]
            (cond
              (= v ::absent) false
              (= v ::non-literal) true
              (or (nil? v) (false? v)) false
              (and (string? v) (empty? v)) false
              :else true)))
        accessible-name-attrs))

(defn- name-required-attrs?
  "True when attrs claim a role that must carry its own accessible name.

  Either a literal `:role` in [[name-required-roles]], or `:aria-modal true`, which
  makes an element a dialog whatever its role says. Non-literal values are treated as
  unknown and skipped, matching the check's false-positive-free posture."
  [attrs]
  (or (contains? name-required-roles (literal-sexpr (get attrs :role)))
      (true? (literal-sexpr (get attrs :aria-modal)))))

(defn- body-locs
  "Return the element's body children, the props slot dropped.
  [[hiccup/props-slot]] rather than `attrs-slot`, so a props map built by a call is
  dropped too: markup handed to an element is rendered wherever that element puts it."
  [vec-loc]
  (let [slot (hiccup/props-slot vec-loc)
        body-start (if slot (z/right slot) (some-> vec-loc z/down z/right))]
    (hiccup/right-siblings (some-> body-start z/left))))

(defn- announces-something?
  "True when a `[:caption …]` carries anything to announce.
  An empty caption names no more than `:aria-label \"\"` does, which `has-accessible-name?`
  already rejects — the same emptiness has to be judged the same way in both places."
  [caption-loc]
  (boolean (some (fn [child]
                   (not (and (literal-string-loc? child) (empty? (z/sexpr child)))))
                 (body-locs caption-loc))))

(defn- named-by-caption?
  "True when a `<table>` holds a `[:caption …]` child that announces something.
  HTML-AAM names a `<table>` from its caption element, so `[:table {:role \"grid\"}
  [:caption \"Quarterly stats\"] …]` is named without an aria attribute. Only a table:
  nothing else HTML defines is named by a caption, so a `[:caption …]` under a `:role
  \"dialog\"` div names nothing and must not end the check. Only a direct child, since a
  caption belongs to the table it opens and one further down names a nested table. A
  child with no head at all — an empty vector, a string, a call — reads as not a caption
  rather than ending the run, and Reagent metadata on the caption is read through."
  [loc tag]
  (and (= :table tag)
       (boolean (some (fn [child]
                        (let [value (hiccup/unwrap-meta child)]
                          (and (= :caption (some-> value z/down parser/raw hiccup/parse-tag))
                               (announces-something? value))))
                      (body-locs loc)))))

(defn- missing-accessible-name? [{:keys [kind attrs]} tag loc]
  (cond
    (contains? accessible-name-required-tags tag)
    (case kind
      :absent true
      :non-map true
      :map (or (nil? attrs) (not (has-accessible-name? attrs)))
      (:dynamic :dynamic-map) false)

    (and (= kind :map) attrs (name-required-attrs? attrs))
    (and (not (has-accessible-name? attrs))
         (not (named-by-caption? loc tag)))

    :else nil))

(def ^:private role->implicit-aria-live
  "Live-region roles mapped to the `aria-live` politeness each implies.

  Both spellings count, since Reagent stringifies keyword values. `timer` and
  `marquee` are absent on purpose: they imply \"off\"."
  {"status" "polite"
   :status "polite"
   "log" "polite"
   :log "polite"
   "alert" "assertive"
   :alert "assertive"})

(def ^:private announcing-aria-live-values
  "Politeness values that ask for an announcement.
  \"off\" is excluded: silencing a live region is a deliberate choice."
  #{"polite" "assertive"})

(defn- declared-aria-live
  "Return the literal `:aria-live` spelling in attrs, or nil when absent or not literal.
  `literal-sexpr` hands a bare symbol straight back, so a computed value would
  otherwise read as a contradicting literal."
  [attrs]
  (let [v (literal-sexpr (get attrs :aria-live))]
    (cond
      (string? v) v
      (and (keyword? v) (not= v ::absent) (not= v ::non-literal)) (name v)
      :else nil)))

(defn- contradicting-aria-live
  "Return the role, the politeness it implies, and the declared one, when they conflict.

  The attribute wins: browsers read `aria-live` first and consult the role only when
  it is absent. An absent `:aria-live` is conformant and not reported, `\"off\"` is a
  deliberate opt-out, and non-literal values are skipped."
  [{:keys [kind attrs]}]
  (when (and (= :map kind) (some? attrs))
    (let [role (literal-sexpr (get attrs :role))
          implied (get role->implicit-aria-live role)
          declared (declared-aria-live attrs)]
      (when (and implied
                 declared
                 (contains? announcing-aria-live-values declared)
                 (not= implied declared))
        {:role role
         :implied implied
         :declared declared}))))

(defn- aria-live-hint [{:keys [role implied declared]}]
  (format ":role %s implies \"%s\", not \"%s\" — drop :aria-live, or set it to \"%s\"."
          (pr-str role) implied declared implied))

(defn- resolve-full-symbol [head-str {:keys [aliases refers]}]
  (cond
    (str/includes? head-str "/")
    (let [slash (str/index-of head-str "/")
          alias-part (subs head-str 0 slash)
          name-part (subs head-str (inc slash))]
      (when-let [full-ns (get aliases alias-part)]
        (symbol full-ns name-part)))

    :else
    (when-let [full-ns (get refers head-str)]
      (symbol full-ns head-str))))

(defn- resolve-component-tag
  "Return the native tag mapped to a resolved component symbol, or nil.

  A `some.ns/*` key maps every var in that namespace, so a large icon or widget
  namespace costs one entry rather than one per var. An exact symbol wins over it."
  [head-str ns-info component-aliases]
  (when (seq component-aliases)
    (when-let [full-sym (resolve-full-symbol head-str ns-info)]
      (or (get component-aliases full-sym)
          (when-let [component-ns (namespace full-sym)]
            (get component-aliases (symbol component-ns "*")))))))

(def ^:private repeating-forms
  "Forms that render their body once per item of a collection."
  #{"for" "map" "mapv" "mapcat" "map-indexed" "keep" "keep-indexed"})

(defn- descendant-of? [ancestor loc]
  (loop [cur loc]
    (cond
      (nil? cur) false
      (identical? (z/node cur) (z/node ancestor)) true
      :else (recur (z/up cur)))))

(defn- for-once-loc
  "The single expression a `for` evaluates once: its first binding's collection.
  Everything else in the bindings vector, including `:let` and any later
  collection expression, is re-evaluated on every iteration."
  [form-loc]
  (some-> form-loc z/down z/right z/down z/right))

(defn- repeats-child?
  "True when `loc` sits in the part of `form-loc` that runs once per item.

  `for` repeats everything except its first binding's collection expression. The
  map family takes the repeating function as its first argument, so its remaining
  arguments are collections that are evaluated once."
  [form-loc child loc]
  (if (= "for" (some-> form-loc z/down parser/raw))
    (let [once (for-once-loc form-loc)]
      (not (and once (descendant-of? once loc))))
    (let [repeating-arg (some-> form-loc z/down z/right)]
      (boolean (and repeating-arg (identical? (z/node repeating-arg) (z/node child)))))))

(defn- literal-name-value
  "Return the literal non-empty string attrs give to `k`, or nil.
  A computed value like `(str \"Figure \" (inc i))` already varies per item, and
  `:alt \"\"` marks a decorative image that is correct however often it repeats."
  [attrs k]
  (let [v (literal-sexpr (get attrs k))]
    (when (and (string? v) (seq v)) v)))

(defn- inside-repeating-form? [loc]
  (loop [child loc
         parent (z/up loc)]
    (cond
      (nil? parent) false

      (and (= :list (z/tag parent))
           (contains? repeating-forms (some-> parent z/down parser/raw))
           (repeats-child? parent child loc))
      true

      :else (recur parent (z/up parent)))))

(defn- vector-tag
  "Return the native tag a Hiccup vector renders, or nil when it is not Hiccup.
  Resolves wrapper components through `:component-aliases` the same way the
  per-vector checks do, so `[my.ui/button …]` reports as `:button`."
  [loc ns-info component-aliases]
  (let [head (some-> loc z/down)]
    (when (and head (= :token (z/tag head)))
      (let [head-str (parser/raw head)]
        (or (hiccup/parse-tag head-str)
            (resolve-component-tag head-str ns-info component-aliases))))))

(def ^:private interactive-content-tags
  "Tags HTML counts as interactive content.

  This is the category the content model of `<button>` and `<a href>` bans inside them.
  `:a` is absent because a link joins it only with an `:href`, and `:audio` / `:video`
  because they join it only with `controls` — both are asked separately. `:img` with a
  `usemap` is left out: the attribute is vanishingly rare and reading it wrong would
  make every image inside a link a finding."
  #{:button :input :select :textarea :details :embed :iframe :label})

(defn- interactive-content?
  "True when the element is interactive content HTML forbids inside a control.
  An `:input` of type hidden renders nothing and is the one member of the tag set that
  can opt out, so its type is read where it is written out."
  [{:keys [kind attrs]} tag]
  (let [readable? (and (contains? attrs-readable-kinds kind) (some? attrs))]
    (or (and (contains? interactive-content-tags tag)
             (not (and (= :input tag) readable? (= "hidden" (literal-sexpr (get attrs :type))))))
        (and (= :a tag) readable? (attr-written? attrs :href))
        (and readable? (interactive-via-role? attrs)))))

(defn- interactive-container?
  "True when the element is one that may not contain interactive content.
  Only `<button>` and `<a href>` carry the restriction in HTML; ARIA extends it in
  effect to anything claiming their roles, since assistive technology then has two
  overlapping controls to announce and keyboard activation is ambiguous."
  [{:keys [kind attrs]} tag]
  (let [readable? (and (contains? attrs-readable-kinds kind) (some? attrs))]
    (or (= :button tag)
        (and (= :a tag) readable? (attr-written? attrs :href))
        (and readable? (interactive-via-role? attrs)))))

(defn- child-locs [loc]
  (when-let [first-child (z/down loc)]
    (take-while some? (iterate #(some-> % z/right) first-child))))

(defn- content-loc? [loc ns-info component-aliases]
  (boolean (when-let [tag (vector-tag loc ns-info component-aliases)]
             (interactive-content? (hiccup/attrs-info loc) tag))))

(defn- names-unreadable-props?
  "True when the element's second child may name props this cannot read.
  `attrs-info` answers `:non-map` both for `[:button \"Save\"]`, which really carries no
  attrs, and for `[:button props \"Save\"]`, where a symbol nothing in the file defines
  may name a map holding the very way out being called missing."
  [loc]
  (let [second-child (some-> loc z/down z/right)]
    (and (some? second-child)
         (= :token (z/tag second-child))
         (some? (parser/sym-name second-child))
         (not= :map (:kind (hiccup/attrs-info loc))))))

(defn- focusable-loc? [loc ns-info component-aliases]
  (boolean (when-let [tag (vector-tag loc ns-info component-aliases)]
             (and (not (names-unreadable-props? loc))
                  (focusable? (hiccup/attrs-info loc) tag)))))

(defn- descend-locs
  "Return what to search inside `loc`.

  A Hiccup vector contributes its body, for the reason [[hiccup/props-slot]] gives, and a
  descendant is no different from the element being reported in that. Anything else
  contributes its children."
  [loc]
  (if (= :vector (z/tag loc))
    (body-locs loc)
    (child-locs loc)))

(defn- first-matching-descendant
  "Return the first vector among `locs` and their descendants satisfying `match?`, or nil.

  A plain descent: a quoting or discarding node is not entered at all, nor is a branch
  `prune?` rejects, so nothing has to be re-climbed at each node to ask whether it still
  counts. `prune?` is asked before `match?`, since a branch that should answer for itself
  should not answer here first."
  [{:keys [match? prune?]
    :as search} locs]
  (some (fn [child]
          (when-not (or (hiccup/unrendered-form? child)
                        (and prune? (prune? child)))
            (if (and (= :vector (z/tag child)) (match? child))
              child
              (first-matching-descendant search (descend-locs child)))))
        locs))

(defn- readable-attrs
  "Return the element's attrs when they can be read whole, or nil."
  [loc]
  (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
    (when (= :map kind) attrs)))

(defn- hidden-subtree?
  "True when the element carries `:aria-hidden true` of its own.
  The search stops there: that element runs the same search over its own subtree, and
  reporting both would count nesting depth rather than defects."
  [loc]
  (boolean (some-> (readable-attrs loc) aria-hidden?)))

(defn- disabled-fieldset?
  "True when the element empties the tab order beneath it.
  Only a `<fieldset disabled>` does: it takes every form control under it out of the tab order, so
  nothing in that branch is focusable and hiding it from assistive technology breaks nothing."
  [loc]
  (boolean (when-let [attrs (readable-attrs loc)]
             (and (= :fieldset (hiccup/parse-tag (parser/raw (z/down loc))))
                  (attr-written? attrs :disabled)))))

(defn- hidden-focusable-descendant
  "Return the first focusable element under an `:aria-hidden true` wrapper, or nil.

  `aria-hidden` applies to the whole subtree, so a control inside a hidden wrapper is
  hidden from assistive technology while keeping its place in the tab order — the same
  defect as on the wrapper itself, and the more common way to write it."
  [{:keys [kind attrs]} loc ns-info component-aliases]
  (when (and (= :map kind) (some? attrs) (aria-hidden? attrs))
    (first-matching-descendant {:match? #(focusable-loc? % ns-info component-aliases)
                                :prune? #(or (hidden-subtree? %) (disabled-fieldset? %))}
                               (body-locs loc))))

(def ^:private aria-label-reference-key
  "The ARIA attribute that makes a label a label.
  `aria-describedby` is a description rather than a name, so an element pointed at by one
  is still labelling nothing."
  :aria-labelledby)

(def ^:private aria-reference-index
  "What `aria-labelledby` in the file points at, held one file at a time.

  Two sets, because an id is written both ways: `:ids` holds the literal ones, and
  `:sources` the source text of computed ones. A field component builds the same id on
  both sides — `(str id \"-label\")` on the label and on the control — and identical source
  in one file is the strongest evidence available that the two agree.

  Every label in a file asks the same question, so this turns a scan per label into a scan
  per file. Keyed by identity of the file's root node, the way hiccup.clj keys its own
  index, so a file this never saw rebuilds it."
  (atom {:cached-root nil
         :references {:ids #{}
                      :sources #{}}}))

(defn- file-root [loc]
  (loop [current loc]
    (if-let [parent (z/up current)] (recur parent) current)))

(defn- normalized-source [loc]
  (parser/normalize-form (parser/raw loc)))

(defn- scan-aria-references [root]
  (loop [current (z/subzip root)
         references {:ids #{}
                     :sources #{}}]
    (cond
      (z/end? current) references

      (and (parser/kw-node? current)
           (= aria-label-reference-key (literal-sexpr current)))
      (let [value-loc (z/right current)
            value (literal-sexpr value-loc)]
        (recur (z/next current)
               (cond
                 (string? value) (update references :ids into (remove str/blank?)
                                         (str/split (str/trim value) #"\s+"))
                 (some? value-loc) (update references :sources conj (normalized-source value-loc))
                 :else references)))

      :else (recur (z/next current) references))))

(defn- aria-references [loc]
  (let [node (z/node (file-root loc))
        {:keys [cached-root references]} @aria-reference-index]
    (if (and node (identical? node cached-root))
      references
      (let [scanned (scan-aria-references (file-root loc))]
        (reset! aria-reference-index {:cached-root node
                                      :references scanned})
        scanned))))

(defn- shorthand-id
  "Return the id a Hiccup tag shorthand carries, as in `:label#notes-label`.
  [[hiccup/parse-tag]] strips it to read the tag, and a label written that way is named
  the same as one carrying `:id`."
  [loc]
  (let [raw (parser/raw (z/down loc))
        hash (str/index-of raw "#")]
    (when hash
      (let [after (subs raw (inc hash))
            dot (str/index-of after ".")]
        (not-empty (if dot (subs after 0 dot) after))))))

(defn- named-through-aria-reference?
  "True when an `aria-labelledby` in the file points at this element.

  A `<label>` referenced that way is a name source rather than a `for`-style label, which
  is conformant markup: the control carries the reference, so the association runs the
  other way and nothing is missing on the label. The id is matched literally where both
  sides write one, and by source text where both compute one."
  [attrs loc]
  (let [{:keys [ids sources]} (aria-references loc)
        id-loc (get attrs :id)
        literal (literal-sexpr id-loc)]
    (boolean
     (or (and (string? literal) (seq literal) (contains? ids literal))
         (and (some? id-loc) (not (string? literal)) (contains? sources (normalized-source id-loc)))
         (when-let [shorthand (shorthand-id loc)] (contains? ids shorthand))))))

(def ^:private labelable-tags
  "Tags a `<label>` can label.

  HTML calls these labelable elements, and a label's control is the first of them among
  its descendants when no `for` attribute names one instead."
  #{:input :select :textarea :button :meter :output :progress})

(def ^:private label-for-keys
  "Spellings of the `for` attribute Reagent sends to the DOM as `for`."
  [:for :html-for :htmlFor])

(def ^:private dom-attr-prefixes ["aria-" "data-" "on-" "on"])

(def ^:private dom-attr-keys
  #{:class :className :id :style :for :html-for :htmlFor :title :role :hidden :key :ref :tab-index :tabIndex})

(defn- names-a-dom-attr?
  "True when a literal props map carries something only markup would carry.
  A Malli entry's properties — `{:optional true}` — carry none of it."
  [attrs]
  (boolean (some (fn [k]
                   (or (contains? dom-attr-keys k)
                       (some #(str/starts-with? (name k) %) dom-attr-prefixes)))
                 (keys attrs))))

(defn- renders-something?
  "True when the label's body holds anything only markup holds.
  A string written out or a nested vector is markup; a lone symbol is what a Malli entry
  puts there — `[:label string?]` — and what a caption is called, so it settles nothing."
  [loc]
  (boolean (some #(or (literal-string-loc? %) (= :vector (z/tag %))) (body-locs loc))))

(defn- hiccup-label?
  "True when a `[:label …]` vector is markup rather than a Malli entry.

  Both are spelled the same way, so the tag cannot settle it and neither can props alone:
  `[:label {:optional true} string?]` is a schema entry carrying properties. What only
  markup has is a DOM attribute, or a body holding a string or a nested vector. A props
  call counts too, but only over a non-empty body — `[:label (my-schema)]` is an entry
  whose schema is computed, and it renders nothing."
  [{:keys [kind attrs]} loc]
  (or (and (= :map kind) (some? attrs) (names-a-dom-attr? attrs))
      (and (contains? #{:dynamic :dynamic-map} kind) (seq (body-locs loc)))
      (renders-something? loc)))

(defn- label-props
  "Return the label's props when `for` can be answered for, or `::unknown`.

  A props map read whole answers both ways. A built map answers only that `for` is there,
  never that it is missing. A symbol names a map this cannot see, and per call site it may
  hold anything — unlike a props *call*, which is shared between call sites and so could
  only return a constant `for`, pointing every label it renders at one element. That last
  is the one case answered without proof, and deliberately."
  [{:keys [kind attrs]} loc]
  (cond
    (= :dynamic kind) (if (names-unreadable-props? loc) ::unknown {})
    (and (= :map kind) (some? attrs)) attrs
    (= :dynamic-map kind) (if (some #(attr-written? (or attrs {}) %) label-for-keys) attrs ::unknown)
    (contains? #{:absent :non-map} kind) (if (names-unreadable-props? loc) ::unknown {})
    :else ::unknown))

(defn- label-body-verdict
  "Return :labels, :opaque or :orphan for what the label's body renders.

  :opaque for anything that might render a control and cannot be read — a component
  vector, or a call. A control written out inside either is associated exactly as one
  written directly in the label would be, so neither can be read as its absence."
  [loc ns-info component-aliases]
  (letfn [(verdict [locs]
            (reduce (fn [acc child]
                      (cond
                        (hiccup/unrendered-form? child) acc
                        (= :list (z/tag child)) (reduced :opaque)
                        (= :vector (z/tag child))
                        (let [tag (vector-tag child ns-info component-aliases)]
                          (cond
                            (nil? tag) (reduced :opaque)
                            (and (contains? labelable-tags tag)
                                 (not (hidden-input? (:attrs (hiccup/attrs-info child)) tag)))
                            (reduced :labels)
                            :else (let [inner (verdict (body-locs child))]
                                    (if (= :orphan inner) acc (reduced inner)))))
                        :else (let [inner (verdict (child-locs child))]
                                (if (= :orphan inner) acc (reduced inner)))))
                    :orphan
                    locs))]
    (verdict (body-locs loc))))

(defn- label-not-associated?
  "True when a `<label>` labels nothing that renders.

  HTML gives a label its control two ways: the `for` attribute, or the first labelable
  element among its descendants. ARIA gives it a third, running the other way: a control
  pointing `aria-labelledby` at the label's id. With none of them the element is text that
  happens to be a `<label>` — clicking it focuses nothing, and it contributes no accessible
  name, however much a sighted reader takes it for the field's label."
  [info tag loc ns-info component-aliases]
  (and (= :label tag)
       (hiccup-label? info loc)
       (let [props (label-props info loc)]
         (and (not= ::unknown props)
              (not (some #(attr-written? props %) label-for-keys))
              (not (named-through-aria-reference? props loc))
              (= :orphan (label-body-verdict loc ns-info component-aliases))))))

(defn- nested-interactive-loc
  "Return the first interactive element inside a control's body, or nil.

  The body, not the whole vector: see [[hiccup/props-slot]] for why props are skipped.
  Below the body the whole subtree counts, since a control wrapped in positioning `:div`s
  or produced by a `for` is nested just the same. The walk stays on the file's own zipper,
  because `z/subzip` restarts position tracking and the hint names a line."
  [info loc tag ns-info component-aliases]
  (when (interactive-container? info tag)
    (first-matching-descendant {:match? #(content-loc? % ns-info component-aliases)}
                               (body-locs loc))))

(defn- inner-element-marks
  "Return the display hint and the identity snippet for an element found inside another.

  A reader wants the line; an identity must never hold one, since a line moves whenever
  anything above it does. So the hint names the line and the snippet does not — and the
  snippet is what tells two findings in one file apart."
  [inner-loc ns-info component-aliases suffix]
  {:hint (format "%s on line %d %s"
                 (pr-str (vector-tag inner-loc ns-info component-aliases))
                 (parser/position-row inner-loc)
                 suffix)
   :inner-form (source-snippet inner-loc)})

(defn- control? [loc ns-info component-aliases]
  (and (= :vector (z/tag loc))
       (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
         (or (contains? empty-interactive-tags (vector-tag loc ns-info component-aliases))
             (and (contains? attrs-readable-kinds kind)
                  (some? attrs)
                  (interactive-via-role? attrs))))))

(defn- known-unnamed-control?
  "True when `loc` is a control that demonstrably has no name of its own.

  An absent or non-map attrs slot holds nothing, so it names nothing. A literal map
  answers for itself. A built, opaque or unclassifiable map is unknown rather than
  unnamed, and counts as named: claiming a key is absent from a partial view is what
  produces false positives."
  [loc ns-info component-aliases]
  (and (control? loc ns-info component-aliases)
       (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
         (case kind
           (:absent :non-map) true
           :map (and (some? attrs) (not (meaningful-text-name? attrs)))
           false))))

(defn- names-an-unnamed-control?
  "True when the nearest enclosing control demonstrably has no name of its own.

  Alt text is only a control's name when the image sits inside a link or button
  supplying none itself. An image in a row names nothing, so a badge repeating the
  same alt across items is correct markup."
  [loc ns-info component-aliases]
  (loop [parent (z/up loc)]
    (cond
      (nil? parent) false
      (control? parent ns-info component-aliases)
      (known-unnamed-control? parent ns-info component-aliases)
      :else (recur (z/up parent)))))

(defn- name-overridden?
  "True when something in attrs means the literal name is not what gets announced.
  `aria-labelledby` wins over `aria-label` in the accessible-name computation, so a
  per-item id there names the item however stale the `aria-label` beside it is, and
  `aria-hidden` removes the element from the tree altogether."
  [attrs]
  (or (attr-written? attrs :aria-labelledby)
      (aria-hidden? attrs)))

(defn- repeated-accessible-name
  "Return the constant name every iteration of a repeated element announces, or nil.
  Only an explicit name attribute counts, and only from a map that can be read whole:
  a built map such as `(merge {:aria-label \"Remove\"} props)` can still be overridden
  per item by what `props` holds. Visible body text is left alone, since WCAG allows a
  link or button to take its purpose from its surroundings where an authored
  `:aria-label` is not."
  [{:keys [kind attrs]} tag loc ns-info component-aliases]
  (when (and (= :map kind)
             (some? attrs)
             (not (name-overridden? attrs))
             (inside-repeating-form? loc))
    (cond
      (or (contains? empty-interactive-tags tag)
          (interactive-via-role? attrs))
      (literal-name-value attrs :aria-label)

      (= :img tag)
      (when (names-an-unnamed-control? loc ns-info component-aliases)
        (literal-name-value attrs :alt)))))

(defn- repeated-name-hint [name-value]
  (format "Every item announces %s. Fold the item into the name." (pr-str name-value)))

(defn- handle-vector* [loc ns-info file component-aliases]
  (let [first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (hiccup/inside-unrendered-form? loc))
               (not (hiccup/inside-style-decl? loc))
               (not (hiccup/inside-ns-form? loc)))
      (when-let [tag (vector-tag loc ns-info component-aliases)]
        (let [info (hiccup/attrs-info loc)
              aria-live-conflict (contradicting-aria-live info)
              repeated-name (repeated-accessible-name info tag loc ns-info component-aliases)
              nested-control (nested-interactive-loc info loc tag ns-info component-aliases)
              hidden-self? (aria-hidden-focusable? info tag)
              hidden-descendant (when-not hidden-self?
                                  (hidden-focusable-descendant info loc ns-info component-aliases))
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
                       (conj (assoc base :type :empty-interactive-element))

                       (missing-accessible-name? info tag loc)
                       (conj (assoc base :type :missing-accessible-name))

                       (label-not-associated? info tag loc ns-info component-aliases)
                       (conj (assoc base :type :label-not-associated))

                       (or hidden-self? hidden-descendant)
                       (conj (cond-> (assoc base :type :aria-hidden-focusable)
                               hidden-descendant
                               (merge (inner-element-marks hidden-descendant ns-info component-aliases
                                                           "is hidden with it and still takes focus."))))

                       nested-control
                       (conj (merge (assoc base :type :nested-interactive-element)
                                    (inner-element-marks nested-control ns-info component-aliases
                                                         "is nested inside it.")))

                       repeated-name
                       (conj (assoc base
                                    :type :repeated-accessible-name
                                    :hint (repeated-name-hint repeated-name)))

                       aria-live-conflict
                       (conj (assoc base
                                    :type :aria-live-contradicts-role
                                    :hint (aria-live-hint aria-live-conflict))))]
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
     :empty-interactive-element (vec (:empty-interactive-element by-type))
     :missing-accessible-name (vec (:missing-accessible-name by-type))
     :label-not-associated (vec (:label-not-associated by-type))
     :aria-hidden-focusable (vec (:aria-hidden-focusable by-type))
     :nested-interactive-element (vec (:nested-interactive-element by-type))
     :repeated-accessible-name (vec (:repeated-accessible-name by-type))
     :aria-live-contradicts-role (vec (:aria-live-contradicts-role by-type))}))

(defn- summary-lines* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive
                               empty-interactive-element missing-accessible-name
                               repeated-accessible-name aria-live-contradicts-role
                               aria-hidden-focusable nested-interactive-element
                               label-not-associated]}]
  [["Img missing alt:" (count img-alt-missing)]
   ["Invalid tabindex:" (count invalid-tabindex)]
   ["Onclick on non-interactive:" (count on-click-on-non-interactive)]
   ["Empty interactive element:" (count empty-interactive-element)]
   ["Missing accessible name:" (count missing-accessible-name)]
   ["Repeated accessible name:" (count repeated-accessible-name)]
   ["Aria-live contradicts role:" (count aria-live-contradicts-role)]
   ["Aria-hidden focusable:" (count aria-hidden-focusable)]
   ["Nested interactive element:" (count nested-interactive-element)]
   ["Label not associated:" (count label-not-associated)]])

(defn- failed?* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive
                         empty-interactive-element missing-accessible-name
                         repeated-accessible-name aria-live-contradicts-role
                         aria-hidden-focusable nested-interactive-element
                         label-not-associated]}]
  (or (seq img-alt-missing)
      (seq invalid-tabindex)
      (seq on-click-on-non-interactive)
      (seq empty-interactive-element)
      (seq missing-accessible-name)
      (seq repeated-accessible-name)
      (seq aria-live-contradicts-role)
      (seq aria-hidden-focusable)
      (seq nested-interactive-element)
      (seq label-not-associated)))

(defrecord A11yGroup [component-aliases]
  group/RuleGroup
  (group-id [_] :a11y)
  (group-name [_] "A11y")
  (parse-handlers [_]
    {:handle-vector (fn [loc ns-info file]
                      (handle-vector* loc ns-info file component-aliases))})
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
          ":on-touch-*, ...) is attached to a non-interactive tag (:div, :span, :svg, "
          ":li, :p, :section, ...) with no keyboard equivalent — mouse users can trigger it but "
          "keyboard users cannot. Either switch to a natively interactive tag (:button, "
          "or :a with :href), or add :role (\"button\", \"link\") or a keyboard handler "
          "(:on-key-down / :on-key-press / :on-key-up) — WCAG recommends both. Note: "
          ":role \"presentation\" / \"none\" / nil / \"\" don't count as valid roles. "
          "See: WCAG 2.1 SC 2.1.1 Keyboard — "
          "https://www.w3.org/WAI/WCAG21/Understanding/keyboard")
     :empty-interactive-element
     (str "A :button, :a, or element with :role \"button\" / \"link\" has no visible text "
          "and no :aria-label / :aria-labelledby / :title — screen readers announce "
          "nothing. Add text content, or provide an accessible name via :aria-label "
          "(e.g. for icon-only buttons). "
          "See: WCAG 2.1 SC 4.1.2 Name, Role, Value — "
          "https://www.w3.org/WAI/WCAG21/Understanding/name-role-value")
     :missing-accessible-name
     (str "An element renders without a programmatic name. Add :aria-label "
          "\"…\" or :aria-labelledby \"<id of visible label>\" (screen readers "
          "announce these as the element's name). :placeholder is a hint, not "
          "a name — it disappears when the user types and is not universally "
          "announced. Triggered by native form controls (`[:textarea …]`), "
          "modal-dialog shapes (`[:div {:role \"dialog\"}]`, `:role \"alertdialog\"`, "
          "`:aria-modal true`, or `[:dialog …]`), the container roles WAI-ARIA marks "
          "name-required — `:role \"listbox\"`, `\"grid\"`, `\"tree\"`, which a screen "
          "reader announces on entry with nothing but the name to say which container "
          "the user has landed in, plus `\"tablist\"`, `\"menu\"` and `\"menubar\"`, which "
          "this rule asks a name of although the spec permits none: they are announced "
          "on entry the same way, and two on a page are indistinguishable without one, "
          "`:role \"img\"` — which opts an element into the "
          "accessibility tree and then leaves a screen reader nothing to announce, "
          "so a decorative icon wants `:aria-hidden true` instead of a role — "
          "and any wrapper listed under `:a11y "
          ":component-aliases` in `.cljs-patrol/config.edn` — e.g. "
          "`{my.ui/drawer :dialog, my.ui/textarea :textarea}`. "
          "See: WCAG 2.1 SC 4.1.2 Name, Role, Value — "
          "https://www.w3.org/WAI/WCAG21/Understanding/name-role-value")
     :repeated-accessible-name
     (str "A control rendered once per collection item names every item with the same "
          "literal string, so a screen-reader user listing the controls on the page hears "
          "\"Remove, Remove, Remove\" with nothing to tell them apart. Build the item into "
          "the name, as in (str \"Remove \" (:title item)), or point :aria-labelledby at the id "
          "of the row's own visible label. Flagged on :button / :a / :role \"button\" / "
          ":role \"link\" carrying a literal :aria-label, and on :img whose :alt names "
          "an enclosing control that has none of its own, inside for / map / mapv / "
          "mapcat / map-indexed / keep / keep-indexed. A computed name is assumed to "
          "vary and is never flagged, nor is :alt \"\" on a decorative image, nor an "
          "image that names nothing, such as a status badge in a row, nor a built attrs "
          "map like (assoc base :aria-label \"…\") whose base cannot be read, since it "
          "may still supply a per-item name — one built entirely from maps this can "
          "read holds no such surprise and is flagged. "
          "A control in a branch is flagged on the name it carries, so "
          "arms that each name differently, as in a tab list, are reported even though "
          "only one renders per item. Visible body "
          "text is also left alone: WCAG lets a control take its purpose from its "
          "surroundings, which an authored :aria-label overrides. "
          "See: WCAG 2.1 SC 2.4.6 Headings and Labels — "
          "https://www.w3.org/WAI/WCAG21/Understanding/headings-and-labels")
     :aria-hidden-focusable
     (str "An element carries :aria-hidden true while keyboard or mouse focus can still "
          "land on it — on the element itself, or on something inside it, since "
          ":aria-hidden applies to the whole subtree and a hidden wrapper keeps every tab "
          "stop under it. Assistive technology is told the element is not there, so a "
          "screen reader announces nothing when focus arrives — the user lands on "
          "something silent. Flagged on natively focusable tags (:button, :input, :textarea, "
          ":select, :details, :summary, and :a carrying an :href), on a widget :role "
          "(\"button\", \"link\", \"checkbox\", \"tab\", \"option\", \"menuitem\", "
          "\"switch\", ...), and on any element given a non-negative :tabIndex / "
          ":tab-index. If the element exists purely as a mouse affordance, take it out of "
          "the tab order with :tabIndex -1 and block mouse focus with an :on-mouse-down "
          "that calls .preventDefault — a negative literal tabindex is what stops this "
          "being reported, on the element itself or on the one inside it that the report "
          "names. Otherwise drop the :aria-hidden and give the element a name. "
          "Note an attrs map that cannot be read whole is skipped — a computed map, or a "
          "built one such as (assoc props :aria-hidden true) whose base is opaque, since "
          "the escape hatch may sit in the part that cannot be read. A built map every "
          "part of which is readable states its whole key set and is treated as written "
          "out. A tabindex that cannot be read, such as the roving (if active? 0 -1), a "
          ":disabled that cannot be read, and a disabled form control all end the question "
          "the same way — `disabled` only where it governs, since on a :div or an :a it "
          "is inert and the tab stop survives it. A child whose own props cannot be read "
          "is passed over for the same reason the element's would be. "
          "See: WCAG 2.1 SC 4.1.2 Name, Role, Value — "
          "https://www.w3.org/WAI/WCAG21/Understanding/name-role-value")
     :label-not-associated
     (str "A <label> labels nothing that renders. HTML gives a label its control two ways: "
          "the `for` attribute naming one by id, or the first labelable element among its "
          "descendants (:input that is not hidden, :select, :textarea, :button, :meter, "
          ":output, :progress). ARIA adds a third running the other way: a control "
          "pointing :aria-labelledby at the label's id, matched literally where both sides "
          "write one and by source text where both compute one. :aria-describedby is a "
          "description rather than a name and does not count. With none of them, the "
          "element is text that happens to be a "
          "<label> — clicking it focuses nothing and it contributes no accessible name, "
          "however much a sighted reader takes it for the field's label. Give it :for with "
          "the control's :id, or wrap the control in it. Note that naming the field with "
          ":aria-label instead leaves the visible text and the announced name saying "
          "different things, which speech-input users cannot bridge (WCAG 2.5.3). A "
          "component or a call among the label's children ends the check rather than "
          "failing it, since either may render the control, as does a props map named by a "
          "symbol or built with a computed key; a props call that cannot be read does "
          "not, because `for` holds one control's id and a shared call could only return a "
          "constant one. A Malli entry is spelled the same way and is skipped: a DOM "
          "attribute in a literal props map, or a body holding a string or a nested "
          "vector, is what tells markup apart. "
          "See: WCAG 2.1 SC 1.3.1 Info and Relationships — "
          "https://www.w3.org/WAI/WCAG21/Understanding/info-and-relationships")
     :nested-interactive-element
     (str "An interactive element contains another one. The HTML content model bans "
          "interactive content inside <button> and inside <a href>, React logs a "
          "validateDOMNesting warning for it, and browsers recover by restructuring the "
          "markup in ways that differ between them. The ARIA form of the same mistake — "
          ":role \"button\" or :role \"link\" on a wrapper holding real controls — gives "
          "assistive technology two overlapping controls to announce and leaves keyboard "
          "activation ambiguous. Restructure so the wrapper is a plain :div doing the "
          "positioning and the two controls are siblings, or drop the outer one. The "
          "element's body is searched, not the props it is handed: `[:button {:tooltip "
          "[:a …]} …]` passes that markup on rather than rendering it there, and a props "
          "map built by a call is read the same way. Below the body the whole subtree "
          "counts, so a control under layout :divs or produced by a `for` is found; "
          "quoted or discarded markup is not, and markup living in another component "
          "stays invisible. Interactive content is the HTML category — :button, :input "
          "that is not hidden, :select, :textarea, :label, :details, :embed, :iframe, "
          "and :a carrying an :href — plus anything claiming a button or link role. "
          "See: HTML content model of <button> — "
          "https://html.spec.whatwg.org/multipage/form-elements.html#the-button-element")
     :aria-live-contradicts-role
     (str "An element sets :aria-live to a different politeness than its :role "
          "implies, and the attribute wins: browsers read :aria-live first and fall "
          "back to the role only when it is absent. :role \"alert\" with :aria-live "
          "\"polite\" is therefore an alert demoted to polite, and :role \"status\" "
          "with :aria-live \"assertive\" interrupts the user where the role asked not "
          "to. Either drop the attribute and let the role speak — \"status\" and "
          "\"log\" imply \"polite\", \"alert\" implies \"assertive\" — or correct it to "
          "match. Note :aria-live \"off\" is not flagged: silencing a live region is a "
          "deliberate choice. "
          "See: WAI-ARIA 1.2, Implicit Value for Role — "
          "https://www.w3.org/TR/wai-aria-1.2/#implictValueForRole")})
  (rule->tier [_]
    {:img-alt-missing :bugs
     :invalid-tabindex :bugs
     :on-click-on-non-interactive :bugs
     :empty-interactive-element :bugs
     :missing-accessible-name :bugs
     :repeated-accessible-name :bugs
     :aria-live-contradicts-role :bugs
     :aria-hidden-focusable :bugs
     :nested-interactive-element :bugs
     :label-not-associated :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(defn make-group
  "Return an a11y RuleGroup configured with the given map (see `A11yGroup`).
  Supported keys:
    :component-aliases {full-qualified-symbol native-tag-keyword} — treat
      calls to the named symbol as the given native tag for a11y checks."
  ([] (make-group nil))
  ([{:keys [component-aliases]}]
   (->A11yGroup (or component-aliases {}))))

(def group (make-group))
