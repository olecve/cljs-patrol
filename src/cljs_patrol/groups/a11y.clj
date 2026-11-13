(ns cljs-patrol.groups.a11y
  "Accessibility rule group: static checks on literal Hiccup vectors.

  Rules are conservative — we only flag when the Hiccup tag and its attribute
  map are both literals. Vectors with dynamically computed tags or attrs are
  skipped to keep the false-positive rate low."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.heads :as heads]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   (java.util
    IdentityHashMap)))

(def ^:private snippet-edn-max
  ;; Chars available for `(pr-str snippet)` — the double-quoted, escaped form
  ;; that ends up in `.cljs-patrol/baseline.edn`. Sized so the surrounding
  ;; `   :form "…"}]}` (last-issue tail case) never exceeds 120 columns.
  107)

(defn- interop-index
  "Find the file being read as rewritten for interop: its root, and identity maps between its vectors."
  []
  (parser/file-state ::interop-index {:written-root nil
                                      :unwrapped-locs nil
                                      :written-nodes nil}))

(defn- written-string
  "Return loc's source as written, `:>` included."
  [loc]
  (if-let [written (some-> ^IdentityHashMap (:written-nodes @(interop-index)) (.get (z/node loc)))]
    (n/string written)
    (z/string loc)))

(defn- truncate-to-fit
  "Return the longest prefix of `collapsed`, with an ellipsis, whose printed form fits `snippet-edn-max`.
  A printed candidate adds the ellipsis and two quotes to its prefix, so the search starts at the
  longest prefix that could fit rather than at the whole string."
  [collapsed]
  (loop [n (-> collapsed count dec (min (- snippet-edn-max (count (pr-str "...")))) (max 3))]
    (let [candidate (str (subs collapsed 0 n) "...")]
      (if (<= (count (pr-str candidate)) snippet-edn-max)
        candidate
        (recur (dec n))))))

(defn- source-snippet
  "Return a display-friendly snippet of loc's source form.

  Whitespace is collapsed to single spaces, then truncated with an ellipsis
  so that `(pr-str snippet)` fits in `snippet-edn-max` chars — i.e. the
  finding's baseline line stays under 120 columns even after EDN escapes
  `\"` → `\\\"` and similar."
  [loc]
  (let [raw (try (written-string loc) (catch Exception _ ""))
        collapsed (str/replace raw #"\s+" " ")]
    (if (<= (count (pr-str collapsed)) snippet-edn-max)
      collapsed
      (truncate-to-fit collapsed))))

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

(defn literal-sexpr
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

(defn meaningful-name-value?
  "True when a name attribute's value names something.
  A non-empty literal does, and so does a value that cannot be read."
  [value-loc]
  (let [v (literal-sexpr value-loc)]
    (cond
      (= v ::absent) false
      (= v ::non-literal) true
      (or (nil? v) (false? v)) false
      (and (string? v) (empty? v)) false
      :else true)))

(defn- meaningful-text-name?
  "True when attrs supplies a non-empty accessible name.

  Checks :aria-label, :aria-labelledby, and :title. Non-literal values are optimistically accepted."
  [attrs]
  (some #(meaningful-name-value? (get attrs %)) text-name-keys))

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
  (boolean (some visible-content? (when body-start (cons body-start (parser/right-siblings body-start))))))

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

(def ^:private widget-role-tags
  "Widget roles taken as tags, for a wrapper that sets its role inside itself.

  No HTML element is called `<switch>`, so these count only when `:component-aliases`
  maps a wrapper onto one: `{my.ui/switch :switch}` says the wrapper renders an element
  carrying that role. WAI-ARIA marks all five Accessible Name Required."
  #{:combobox :searchbox :slider :spinbutton :switch})

(def ^:private accessible-name-required-tags
  "Tags whose element has no intrinsic accessible name.

  A visible `<label>` associated by id, `:aria-label`, or `:aria-labelledby` is
  required — `:placeholder` is a hint, not a name. `:dialog` matches native
  `<dialog>` and any wrapper aliased to `:dialog` via `:component-aliases`."
  (into #{:textarea :dialog} widget-role-tags))

(def ^:private label-named-tags
  "Tags of the name-required elements a `<label>` may name.
  A label names a wrapper aliased to a widget role when the wrapper renders a labelable
  control: a Radix switch or select trigger is a `<button>`, a combobox an `<input>`.
  A slider thumb written as a `<span>` is not one, but the wrapper cannot be seen into."
  (conj widget-role-tags :textarea))

(defn- widget-role-vector?
  "True when a vector stands for a widget role, which only an aliased component does.
  Written out, `[:switch …]` renders an unknown element with no role at all."
  [tag loc]
  (and (contains? widget-role-tags tag)
       (nil? (some-> loc z/down parser/raw hiccup/parse-tag))))

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
  (some #(meaningful-name-value? (get attrs %)) accessible-name-attrs))

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
    (parser/right-siblings (some-> body-start z/left))))

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
      (and (keyword? v) (not= v ::absent) (not= v ::non-literal)) (name v))))

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

(defn- resolve-component-alias
  "Return the value a resolved component symbol is mapped to in `component-aliases`, or nil.

  Given the aliases, the value is a `{:tag … :label-props …}` alias; given the label
  components, a set of label props. A `some.ns/*` key maps every var in that namespace, so a large icon or widget
  namespace costs one entry rather than one per var. An exact symbol wins over it."
  [head-str ns-info component-aliases]
  (when (seq component-aliases)
    (when-let [full-sym (parser/resolve-full-symbol head-str ns-info)]
      (or (get component-aliases full-sym)
          (when-let [component-ns (namespace full-sym)]
            (get component-aliases (symbol component-ns "*")))))))

(defn- resolve-component-tag
  "Return the native tag mapped to a resolved component symbol, or nil."
  [head-str ns-info component-aliases]
  (:tag (resolve-component-alias head-str ns-info component-aliases)))

(defn- label-prop-value
  "Return the value of the first of `label-props` that names something, or nil."
  [attrs label-props]
  (->> label-props
       (map #(get attrs %))
       (filter meaningful-name-value?)
       first))

(defn- element-info
  "Classify the element's props, reading a wrapper's label props as its accessible name.

  An alias may list the props its wrapper wires to an accessible name of its own, such
  as a `:label` rendered as a visible label. The first that names something names the
  element, read as its name text. Beside a written `:aria-label` there is no telling
  which of the two the wrapper keeps, so it is read as a reference instead: the element
  is named, and neither is taken for the one name every item announces. A written
  `:aria-labelledby` is a reference already, and every rule asking for a constant
  name passes over it."
  [loc ns-info component-aliases]
  (let [{:keys [attrs]
         :as info} (hiccup/attrs-info loc)
        label-props (some-> loc z/down parser/raw
                            (resolve-component-alias ns-info component-aliases)
                            :label-props)
        label-value (label-prop-value attrs label-props)]
    (cond
      (nil? label-value) info
      (attr-written? attrs :aria-label) (assoc-in info [:attrs :aria-labelledby] label-value)
      :else (assoc-in info [:attrs :aria-label] label-value))))

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
    (parser/child-locs loc)))

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

(defn- aria-reference-index
  "Find what `aria-labelledby` in the file points at, kept in [[parser/file-state]] for that file alone.

  Two sets, because an id is written both ways: `:ids` holds the literal ones, and
  `:sources` the source text of computed ones. A field component builds the same id on
  both sides — `(str id \"-label\")` on the label and on the control — and identical source
  in one file is the strongest evidence available that the two agree.

  Every label in a file asks the same question, so this turns a scan per label into a scan
  per file. Keyed by identity of the file's root node, the way hiccup.clj keys its own
  index, so a tree this never saw rebuilds it."
  []
  (parser/file-state ::aria-reference-index {:cached-root nil
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
        state (aria-reference-index)
        {:keys [cached-root references]} @state]
    (if (and node (identical? node cached-root))
      references
      (let [scanned (scan-aria-references (file-root loc))]
        (reset! state {:cached-root node
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

(defn- label-vector?
  "True when a vector renders a `<label>`, as a Hiccup tag or through React interop."
  [loc]
  (and (= :vector (z/tag loc))
       (let [head (some-> loc z/down parser/raw)]
         (or (= :label (hiccup/parse-tag head))
             (and (= ":>" head) (= "\"label\"" (some-> loc z/down z/right parser/raw)))))))

(defn- cannot-be-props?
  "True when a native element's second child is one Reagent never takes as props.
  Reagent reads a map there as props at runtime, so a symbol, a deref or a call may
  hold one; a string, a number, a keyword or a vector cannot."
  [child]
  (case (z/tag child)
    (:vector :multi-line) true
    :token (not (symbol? (z/sexpr child)))
    false))

(defn- native-vector? [loc]
  (and (= :vector (some-> loc z/tag))
       (some? (some-> loc z/down parser/raw hiccup/parse-tag))))

(defn- argument-locs
  "Return the locs of a vector's children, the `#_` forms dropped."
  [loc]
  (remove #(= :uneval (z/tag %)) (parser/child-locs loc)))

(defn- label-component-props
  "Return the configured label props of the label component a vector renders, or nil."
  [loc {:keys [ns-info label-components]}]
  (when (and (seq label-components) (= :vector (z/tag loc)))
    (some-> loc z/down parser/raw (resolve-component-alias ns-info label-components))))

(defn- map-draws-label?
  "True when a literal props map may set one of the label props: it sets one, or its keys cannot be read."
  [attrs label-props]
  (or (nil? attrs) (some? (label-prop-value attrs label-props))))

(defn- draws-label?
  "True when a label component may draw its label.

  It draws one only when one of its label props is set. Props the rule cannot read may
  set one. A string, a keyword, a number or a vector in their place, written there or
  named by a symbol, is no props map, so it sets none. A `#_` form is no argument, so a
  props map after one is read as the props."
  [loc label-props]
  (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
    (case kind
      :map (map-draws-label? attrs label-props)
      :non-map (let [argument (second (argument-locs loc))
                     value (or (hiccup/resolved-value argument) argument)]
                 (if (= :map (z/tag value))
                   (map-draws-label? (hiccup/literal-map value) label-props)
                   (not (cannot-be-props? value))))
      true)))

(def ^:private interop-heads
  "Heads of a vector that renders a React component, which may pass props on as a component does."
  #{":>" ":f>" ":r>"})

(defn- id-taker?
  "True when the vector at loc is a native element, which keeps an id it is given rather than passing it on."
  [loc]
  (and (native-vector? loc)
       (not (contains? interop-heads (some-> loc z/down parser/raw)))))

(defn- control-of-label-component?
  "True when the label component at `wrapper` may give its label's id to the element at loc.

  The component writes the id into the props of its first child after its props, so
  that child is its one control. A form there gives it to what the form yields. A native
  element given the id keeps it, so a control inside it is not named, where a component
  may pass it on. `child` is the wrapper's child on the way up from loc."
  [wrapper child loc {:keys [ns-info]}]
  (and (identical? (z/node child) (some-> (nth (argument-locs wrapper) 2 nil) z/node))
       (let [result (->> (hiccup/result-locs (hiccup/unwrap-meta child) ns-info)
                         (map hiccup/unwrap-meta)
                         (filter #(descendant-of? % loc))
                         first)]
         (or (nil? result)
             (identical? (z/node result) (z/node loc))
             (not (id-taker? result))))))

(defn- inside-label?
  "True when a `<label>` vector encloses the element, or a component that draws one gives it the label's id.

  A label component the element is not the control of gives it no id, and neither does
  any component above it, so from there only a `<label>` vector may still name it."
  [loc context]
  (loop [child loc
         ancestor (z/up loc)
         blocked? false]
    (cond
      (nil? ancestor) false
      (label-vector? ancestor) true
      :else (let [label-props (when-not blocked? (label-component-props ancestor context))]
              (cond
                (nil? label-props) (recur ancestor (z/up ancestor) blocked?)
                (not (control-of-label-component? ancestor child loc context)) (recur ancestor (z/up ancestor) true)
                (draws-label? ancestor label-props) true
                :else (recur ancestor (z/up ancestor) false))))))

(defn- named-by-label?
  "True when a `<label>` may name the element.
  One wrapping it may, and so may one a label component draws for it, as `inside-label`
  has it, and one anywhere on the page whose `for` points at an id the element carries,
  written out or in the tag shorthand. An id that cannot be read counts, since the label
  it is built for cannot be ruled out."
  [attrs loc inside-label]
  (or @inside-label
      (some? (shorthand-id loc))
      (attr-written? attrs :id)))

(defn- name-required-tag?
  "True when the element's tag alone requires an accessible name."
  [tag loc]
  (and (contains? accessible-name-required-tags tag)
       (or (not (contains? widget-role-tags tag)) (widget-role-vector? tag loc))))

(defn- props-name-nothing?
  "True when the element's props demonstrably carry no accessible name.
  A component's second child is an argument, which a wrapper may take as its name
  whatever its shape, so only a native element's can be read past a missing map."
  [{:keys [kind attrs]} loc]
  (case kind
    :absent true
    :map (and (some? attrs) (not (has-accessible-name? attrs)) (not (aria-hidden? attrs)))
    :non-map (and (some? (some-> loc z/down parser/raw hiccup/parse-tag))
                  (cannot-be-props? (z/right (z/down loc))))
    false))

(defn- missing-accessible-name? [{:keys [kind attrs]
                                  :as info} tag loc inside-label]
  (cond
    (name-required-tag? tag loc)
    (and (props-name-nothing? info loc)
         (not (and (contains? label-named-tags tag) (named-by-label? attrs loc inside-label)))
         (not (and (= :switch tag) (seq (body-locs loc)))))

    (and (= kind :map) attrs (name-required-attrs? attrs))
    (and (not (has-accessible-name? attrs))
         (not (aria-hidden? attrs))
         (not (named-by-caption? loc tag)))))

(defn- unreferenced-id-candidate
  "Return the literal id an element relies on for its name, else nil.
  Only an element that [[missing-accessible-name?]] lets go on its id alone qualifies,
  and only an id written out as one whitespace-free string, in the props or the tag
  shorthand. Either way the element writes one literal containing it."
  [{:keys [attrs]
    :as info} tag loc inside-label]
  (when (and (name-required-tag? tag loc)
             (contains? label-named-tags tag)
             (props-name-nothing? info loc)
             (not @inside-label)
             (not (and (= :switch tag) (seq (body-locs loc)))))
    (let [shorthand (shorthand-id loc)
          id-written? (attr-written? attrs :id)
          literal (literal-sexpr (get attrs :id))]
      (cond
        (and shorthand (not id-written?)) shorthand
        (and (not shorthand) (string? literal) (re-matches #"\S+" literal)) literal))))

(def ^:private separator-roles
  "The `:role` spellings of a separator."
  #{"separator" :separator})

(defn- inside-comment-form?
  "True when a `(comment …)` encloses loc: code kept for the REPL, which no page renders."
  [loc]
  (boolean (some #(and (= :list (z/tag %)) (= "comment" (some-> % z/down parser/sym-name)))
                 (take-while some? (rest (iterate z/up loc))))))

(def ^:private shell-consumers
  "Core calls that take a Hiccup vector as data to add children to, rather than render it."
  #{'cljs.core/into 'cljs.core/conj})

(def ^:private destructuring-heads
  "Forms whose binding vector can destructure a shell into its tag and props."
  #{"let" "when-let" "if-let" "when-some" "if-some" "with-let"})

(defn- private-defn?
  "True when the list at loc is a `defn-`, or a `defn` whose name carries `:private` metadata."
  [loc]
  (case (some-> loc z/down parser/sym-name)
    "defn-" true
    "defn" (true? (:private (meta (try (z/sexpr (some-> loc z/down z/right)) (catch Exception _ nil)))))
    false))

(defn- returning-defn-name
  "Return the name of the single-arity private `defn` whose result can be loc, or nil.

  Only a private function is read, since no other file can call it."
  [loc]
  (let [top (parser/top-level-loc loc)]
    (when (and (= :list (z/tag top))
               (private-defn? top))
      (let [body-end (z/rightmost (z/down top))]
        (when (some #(identical? (z/node %) (z/node loc)) (hiccup/result-locs body-end))
          (some-> (parser/declared-name-loc top) parser/sym-name))))))

(defn- code-child-locs
  "Return the locs of loc's children, the `#_` forms dropped."
  [loc]
  (remove #(= :uneval (z/tag %)) (parser/child-locs loc)))

(defn- subtree-locs
  "Return loc and every loc inside it, the `#_` forms dropped."
  [loc]
  (->> loc code-child-locs (mapcat subtree-locs) (cons loc)))

(defn- may-hold-text?
  "True when a child added to a shell may hold text: anything but a vector, which may be icon markup."
  [loc]
  (not= :vector (some-> loc hiccup/unwrap-meta z/tag)))

(defn- filled-by-consumer?
  "True when `call` is the shell that an `into` or `conj` call adds a child to that may hold text.

  The children `into` adds are the items of its next argument, or anything when that is no
  vector written out."
  [call ns-info]
  (let [consumer (z/up call)
        [_ target & added] (code-child-locs consumer)]
    (and (identical? (z/node target) (z/node call))
         (condp = (heads/resolve-head consumer ns-info shell-consumers)
           'cljs.core/into (boolean (some #(or (not= :vector (z/tag %)) (some may-hold-text? (code-child-locs %)))
                                          added))
           'cljs.core/conj (boolean (some may-hold-text? added))
           false))))

(defn- destructured-and-filled?
  "True when `call` is the value of a vector binding in a `let`-like form, and its body adds a child to the shell.

  `(let [[tag attrs] (chip-element props)] [tag attrs label])` binds the shell's tag and
  props, and renders them with a child of their own. Every vector in the body headed by the
  tag must add a child that may hold text, so no branch renders the shell empty or with an
  icon only."
  [call]
  (let [bindings (z/up call)
        binding-form (some-> bindings z/up)
        elements (some-> bindings code-child-locs vec)
        index (first (keep-indexed #(when (identical? (z/node %2) (z/node call)) %1) elements))
        target (some->> index dec (get elements))
        tag-name (some-> target code-child-locs first parser/raw)
        rebuilds (when tag-name
                   (->> binding-form
                        code-child-locs
                        (drop 2)
                        (mapcat subtree-locs)
                        (filter #(and (= :vector (z/tag %)) (= tag-name (some-> % z/down parser/raw))))))]
    (boolean (and (= :vector (some-> bindings z/tag))
                  (= :list (some-> binding-form z/tag))
                  (contains? destructuring-heads (some-> binding-form z/down parser/sym-name))
                  (identical? (z/node bindings) (some-> binding-form code-child-locs second z/node))
                  (seq rebuilds)
                  (every? #(some may-hold-text? (drop 2 (code-child-locs %))) rebuilds)))))

(defn- live-use?
  "True when the token at loc names `fn-name` in code a page can run, as this file's function.

  A use in a `(comment …)`, a `#_` form or a quote never runs. The name the `defn` declares,
  and a local that shadows it, are no use of the function."
  [loc fn-name]
  (and (= :token (z/tag loc))
       (= fn-name (parser/raw loc))
       (not (identical? (z/node loc) (some-> loc parser/top-level-loc parser/declared-name-loc z/node)))
       (not (hiccup/inside-unrendered-form? loc))
       (not (inside-comment-form? loc))
       (not (heads/locally-bound? loc fn-name))))

(defn- filled-shell-name?
  "True when every live use of `fn-name` in the file is a call whose shell the caller fills.

  A use counts when it heads a call that a `let`-like form destructures and renders with a
  child, or a call that an `into` or `conj` call adds a child to that may hold text. Any
  other use, a rendered call or the name passed as a value, may render the shell empty."
  [root fn-name ns-info]
  (let [uses (filter #(live-use? % fn-name) (take-while (complement z/end?) (iterate z/next root)))]
    (boolean (and (seq uses)
                  (every? (fn [use]
                            (let [call (z/up use)]
                              (and (= :list (some-> call z/tag))
                                   (nil? (z/left use))
                                   (or (destructured-and-filled? call) (filled-by-consumer? call ns-info)))))
                          uses)))))

(defn- element-shell?
  "True when an element is a shell a function returns for its callers to fill, not markup it renders.

  Each live call of the function must fill it: `(let [[tag attrs] (chip-element props)]
  [tag attrs label])`, `(into (link-shell href) [label])` or `(conj (link-shell href) label)`.
  The answer for each function is kept for the file."
  [loc ns-info]
  (boolean (when-let [fn-name (returning-defn-name loc)]
             (let [state (parser/file-state ::filled-shell-names {})]
               (if (contains? @state fn-name)
                 (get @state fn-name)
                 (let [answer (filled-shell-name? (file-root loc) fn-name ns-info)]
                   (swap! state assoc fn-name answer)
                   answer))))))

(defn- focusable-separator
  "Return {:unnamed? …} for a separator a user can focus, or nil.

  WAI-ARIA gives `separator` two modes: a static rule, which is structure, and a focusable
  splitter, which is a widget. Only the splitter is asked for a name, so only one counts: a
  wrapper aliased to `:separator`, which renders the splitter since a static rule is
  written as an `<hr>` rather than wrapped, or an element whose `:role` is separator, or an
  `[:hr]`, that carries a tabindex. One hidden with `:aria-hidden`, or whose `:aria-hidden`
  cannot be read, may be out of the tree, and one in a `(comment …)` is never rendered.
  A wrapper given children may make them its name, as a switch wrapper may."
  [{:keys [kind attrs]
    :as info} tag loc]
  (let [aliased? (and (= :separator tag) (nil? (some-> loc z/down parser/raw hiccup/parse-tag)))
        native? (and (= :map kind)
                     (some? attrs)
                     (or (contains? separator-roles (literal-sexpr (get attrs :role)))
                         (and (= :hr tag) (not (attr-written? attrs :role))))
                     (or (attr-written? attrs :tab-index) (attr-written? attrs :tabIndex)))]
    (when (and (or aliased? native?)
               (contains? #{::absent false "false"} (literal-sexpr (get attrs :aria-hidden)))
               (not (inside-comment-form? loc)))
      {:unnamed? (and (props-name-nothing? info loc)
                      (not (and aliased? (seq (body-locs loc)))))})))

(defn- unnamed-separator-hint [separator-count]
  (str "One of " separator-count " focusable separators in the run, and WAI-ARIA asks each to be named. "
       "Add :aria-labelledby with the id of the resized pane's heading, or :aria-label."))

(defn- unreferenced-id-hint [id]
  (format (str "No other literal in the run contains the id %s, so no label points at it. "
               "Point a [:label {:for %s}] at it, or pass :aria-label.")
          (pr-str id) (pr-str id)))

(def ^:private id-reference-names
  "Names an attribute pointing at an element by its id goes by.
  It is spelled as a Reagent key, as a DOM property, or as the string a `#js` object,
  `js-obj` or `.setAttribute` takes."
  #{"for" "html-for" "htmlFor" "aria-labelledby" ".-htmlFor" "-htmlFor"})

(defn- literal-value
  "Return the sexpr of a token or string node the source writes out, else ::non-literal."
  [node]
  (if (contains? #{:token :multi-line} (n/tag node))
    (try (n/sexpr node) (catch Exception _ ::non-literal))
    ::non-literal))

(defn- literal-text
  "Return the text a literal value could spell an id with, or nil for a symbol left unquoted.
  A keyword gives its name and a number or character its printed form, since `str`,
  `name` and Reagent all render them that way."
  [value quoted?]
  (cond
    (= ::non-literal value) nil
    (string? value) value
    (keyword? value) (name value)
    (or (number? value) (char? value)) (str value)
    (symbol? value) (when quoted? (name value))))

(defn- computed-reference?
  "True when the value names an id-reference attribute and the value after it is not written out."
  [value next-value]
  (and (or (string? value) (symbol? value) (keyword? value))
       (contains? id-reference-names (name value))
       (not (or (string? next-value)
                (number? next-value)
                (and (keyword? next-value) (not= ::non-literal next-value))))))

(defn- quoting?
  "True when the node quotes its children, so a symbol inside is written out as a name."
  [node]
  (or (contains? #{:quote :syntax-quote} (n/tag node))
      (and (= :list (n/tag node))
           (= 'quote (some-> (first (remove n/whitespace-or-comment? (n/children node))) literal-value)))))

(defn- scan-file-literals [root-node]
  (let [literals (volatile! (transient {}))
        computed? (volatile! false)
        visit (fn visit [parent]
                (let [quoted? (quoting? parent)]
                  (loop [children (seq (n/children parent))
                         previous ::none]
                    (if-let [child (first children)]
                      (cond
                        (n/whitespace-or-comment? child) (recur (next children) previous)
                        (n/inner? child) (do (visit child)
                                             (when (computed-reference? previous ::non-literal)
                                               (vreset! computed? true))
                                             (recur (next children) ::non-literal))
                        :else (let [value (literal-value child)
                                    text (literal-text value quoted?)]
                                (when text
                                  (vswap! literals assoc! text (inc (get @literals text 0))))
                                (when (computed-reference? previous value)
                                  (vreset! computed? true))
                                (recur (next children) value)))
                      (when (computed-reference? previous nil)
                        (vreset! computed? true))))))]
    (visit root-node)
    {:literals (persistent! @literals)
     :computed-reference? @computed?}))

(defn- file-literals-root
  "Find the root node whose literals were last collected, kept in [[parser/file-state]] for the file being read.
  Keyed by identity, as [[aria-reference-index]] is, so a tree parsed again is collected again."
  []
  (parser/file-state ::file-literals-root nil))

(defn- file-literals
  "Collect what can point an id at an element in the file of `loc`, once per file.
  An id is referenced when another literal in the run contains it: a `:for`, an
  `aria-labelledby` list, a markup string, or a constant either is built from. Lists are
  where a file is met, so a file holding none, not even an `ns` form, is not read."
  [loc]
  (let [root (file-root loc)
        state (file-literals-root)]
    (when-not (identical? (z/node root) @state)
      (reset! state (z/node root))
      {:decls []
       :dynamics []
       :usages [(assoc (scan-file-literals (z/node root)) :type ::file-literals)]})))

(defn- unreferenced-id?
  "True when nothing in the run can point at the candidate's id.
  Another literal containing the id refers to it. Once any reference in the run is
  computed, so does any other literal the id contains, since it may be one of the pieces
  the reference is built from."
  [literals computed-reference? {:keys [id]}]
  (and (= 1 (->> literals (filter #(str/includes? (key %) id)) (map val) (reduce + 0)))
       (not (and computed-reference?
                 (some #(and (not= id %) (str/includes? id %)) (keys literals))))))

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
                            (or (widget-role-vector? tag child)
                                (and (contains? labelable-tags tag)
                                     (not (hidden-input? (:attrs (hiccup/attrs-info child)) tag))))
                            (reduced :labels)
                            :else (let [inner (verdict (body-locs child))]
                                    (if (= :orphan inner) acc (reduced inner)))))
                        :else (let [inner (verdict (parser/child-locs child))]
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

(def ^:private self-named-input-types
  "Input types named by their own value, rendered with their label after them, or not shown."
  #{"button" "checkbox" "hidden" "image" "radio" "reset" "submit"})

(def ^:private field-roles
  "Roles of the fields a visible label written before them is taken to name."
  #{"combobox" "listbox" "searchbox" "slider" "spinbutton" "textbox"})

(defn- labelled-field?
  "True when the element is a field that a visible label written before it names.

  Buttons and links are left out: an icon button beside a caption acts on the value the
  caption names, and its own `:aria-label` says what it does. An input counts only when
  its type is missing or written as one that shows a value, since a computed type may be
  a checkbox, whose label follows it."
  [attrs tag loc]
  (cond
    (= :input tag) (let [input-type (literal-sexpr (get attrs :type))]
                     (or (= ::absent input-type)
                         (and (string? input-type) (not (contains? self-named-input-types input-type)))))
    (contains? #{:select :textarea} tag) true
    (contains? widget-role-tags tag) (and (not= :switch tag) (widget-role-vector? tag loc))
    :else (contains? field-roles (literal-sexpr (get attrs :role)))))

(defn- label-style-call?
  "True when `loc` calls one of the configured label styles.
  An unqualified name nothing refers is read as a var of the file's own namespace."
  [loc {:keys [ns-info label-styles]}]
  (and (= :list (some-> loc z/tag))
       (let [head (z/down loc)]
         (and (some? head)
              (let [head-str (parser/raw head)]
                (contains? label-styles (or (parser/resolve-full-symbol head-str ns-info)
                                            (symbol (:ns-name ns-info) head-str))))))))

(defn- visible-label?
  "True when the native element at `loc` is styled as a label and shows only written text.

  The style is the one thing that says the text is a label, so it must be a configured
  label style, called as the props or as the `:class` of a literal props map. The body
  must be literal strings with some text in them: a symbol or a call may render nothing.
  A `<label>` is left to `label-not-associated`, which has a fix of its own for it."
  [loc context]
  (let [head (z/down loc)
        slot (z/right head)
        body (parser/right-siblings slot)]
    (and (not= :label (hiccup/parse-tag (parser/raw head)))
         (or (label-style-call? slot context)
             (label-style-call? (-> loc hiccup/attrs-info :attrs :class) context))
         (every? literal-string-loc? body)
         (some #(not (str/blank? (z/sexpr %))) body))))

(defn- first-body-child? [loc]
  (let [parent (z/up loc)]
    (and (native-vector? parent)
         (= (z/node loc) (some-> (body-locs parent) first z/node)))))

(def ^:private label-wrapper-depth
  "How many wrapping elements may stand between a label or field and the shared container."
  2)

(defn- leading-locs
  "Return `loc` and the native elements it opens, as far as `label-wrapper-depth`."
  [loc]
  (->> (iterate #(some-> % body-locs first) loc)
       (take-while native-vector?)
       (take (inc label-wrapper-depth))))

(defn- yielding-parent
  "Return the form `loc` stands in for in its parent's place, or nil.

  Metadata wraps it, and a `let`, `when` or `if` yields it, so either sits where the
  element renders. Neither counts as a wrapper: no element of its own comes between."
  [loc ns-info]
  (let [parent (z/up loc)]
    (case (some-> parent z/tag)
      :meta parent
      :list (when (some #(= (z/node loc) (z/node %)) (hiccup/result-locs parent ns-info)) parent)
      nil)))

(defn- enclosing-locs
  "Return `loc` and the native elements it opens from outside, as far as `label-wrapper-depth`.
  The forms that yield it count too, as [[yielding-parent]] says."
  [loc ns-info]
  (loop [current loc
         depth 0
         found [loc]]
    (if-let [parent (yielding-parent current ns-info)]
      (recur parent depth (conj found parent))
      (if (and (< depth label-wrapper-depth) (first-body-child? current))
        (let [parent (z/up current)]
          (recur parent (inc depth) (conj found parent)))
        found))))

(def ^:private field-props
  "Props a component that renders a field is named or driven through."
  #{:aria-label :aria-labelledby :default-value :label :on-change :placeholder :value})

(defn- possible-field?
  "True when the vector at `loc` is a field, or a component that may render one.

  Unlike [[labelled-field?]], an input whose type is computed counts: it may show a value.
  A component no alias maps cannot be seen into, so it counts when its props name or drive
  it the way a field's are, or when they cannot be read at all: a symbol or a call.
  Interop, `[:> Comp …]`, counts always, its props sitting past the component."
  [loc {:keys [ns-info component-aliases]}]
  (let [head (z/down loc)
        {:keys [kind attrs]} (hiccup/attrs-info loc)]
    (cond
      (= ":>" (some-> head parser/raw)) true

      :else
      (if-let [tag (vector-tag loc ns-info component-aliases)]
        (if (= :input tag)
          (not (contains? self-named-input-types (literal-sexpr (get attrs :type))))
          (labelled-field? attrs tag loc))
        (or (contains? #{:dynamic :dynamic-map} kind)
            (some? (parser/sym-name (z/right head)))
            (some #(contains? attrs %) field-props))))))

(declare holds-field?)

(defn- for-form? [loc]
  (= "for" (some-> loc z/down parser/sym-name)))

(defn- renders-field?
  "True when the form at `loc`, written where its value renders, may render a field.

  What a `let`, `when`, `if`, `cond` or `case` yields and a `for` or `fn` body are read.
  A call, a symbol, a deref or a reader conditional cannot be read, so it may render one,
  unless it is `plain?`: taken as text or data. A map is read for the vectors it holds."
  [loc except context plain?]
  (case (z/tag loc)
    (:meta :vector) (holds-field? loc except context)
    :list (let [results (if (contains? #{"fn" "for"} (some-> loc z/down parser/sym-name))
                          (take-last 1 (parser/right-siblings (z/down loc)))
                          (hiccup/result-locs loc))]
            (if (identical? (z/node loc) (some-> results first z/node))
              (not plain?)
              (some #(renders-field? % except context plain?) results)))
    :map (some #(renders-field? % except context true) (parser/child-locs loc))
    :token (and (not plain?) (some? (parser/sym-name loc)))
    (:deref :reader-macro) (not plain?)
    false))

(def ^:private text-tags
  "Elements whose symbols and calls are taken as text, not as fields they may render."
  #{:b :code :em :h1 :h2 :h3 :h4 :h5 :h6 :i :p :small :span :strong})

(defn- holds-field?
  "True when `loc` or anything it renders may be a field, `except` that one left out.

  A component's arguments and a text element's body are plain: their calls and symbols
  are data or text. A `for` in an element's props slot is read as its first child."
  [loc except context]
  (boolean
   (when loc
     (case (z/tag loc)
       :meta (holds-field? (hiccup/unwrap-meta loc) except context)
       :vector (and (not (identical? (z/node loc) (z/node except)))
                    (or (possible-field? loc context)
                        (if (native-vector? loc)
                          (let [slot (hiccup/props-slot loc)
                                plain? (contains? text-tags (hiccup/parse-tag (parser/raw (z/down loc))))]
                            (some #(renders-field? % except context plain?)
                                  (concat (when (for-form? slot) [slot]) (body-locs loc))))
                          (some #(renders-field? % except context true) (parser/child-locs loc)))))
       (renders-field? loc except context false)))))

(defn- labels?
  "True when `loc` is a visible label, or opens with one."
  [loc context]
  (boolean (some #(visible-label? % context) (leading-locs loc))))

(defn- field-before-next-label?
  "True when a field follows `loc` among its siblings before another visible label does.
  The label before `loc` then names a group of fields, not the one in `loc`."
  [loc field context]
  (loop [[sibling & more] (parser/right-siblings loc)]
    (cond
      (nil? sibling) false
      (labels? sibling context) false
      (holds-field? sibling field context) true
      :else (recur more))))

(defn- aria-label-beside-visible-label?
  "True when a field names itself with `:aria-label` right after a visible label.

  The label is text styled with a configured label style, written as the sibling just
  before the field inside a native container. Either side may sit in wrappers it opens,
  as a field in a `[:div (field-wrapper) …]`. Two strings then say what the field is, and
  nothing keeps them alike when one is reworded (WCAG 2.5.3 Label in Name).

  A label shared by several fields names the group, so a field beside another field, in
  its wrapper or after it before the next label, is left alone. So is a label whose
  wrapper holds a field of its own, which it names instead."
  [tag loc {:keys [label-styles ns-info]
            :as context}]
  (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
    (and (seq label-styles)
         (= :map kind)
         (attr-written? attrs :aria-label)
         (not (contains? attrs :aria-labelledby))
         (labelled-field? attrs tag loc)
         (boolean
          (some (fn [field-side]
                  (let [previous (z/left field-side)]
                    (and (native-vector? (z/up field-side))
                         (labels? previous context)
                         (not (holds-field? previous loc context))
                         (not (holds-field? field-side loc context))
                         (not (field-before-next-label? field-side loc context)))))
                (enclosing-locs loc ns-info))))))

(defn- nested-interactive-loc
  "Return the first interactive element inside a control's body, or nil.

  The body, not the whole vector: see [[hiccup/props-slot]] for why props are skipped.
  Below the body the whole subtree counts, since a control wrapped in positioning `:div`s
  or produced by a `for` is nested just the same."
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
       (let [{:keys [kind attrs]} (element-info loc ns-info component-aliases)]
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

(defn- code-children [node]
  (remove n/whitespace-or-comment? (n/children node)))

(defn- aliased-interop-node? [node ns-info component-aliases]
  (and (= :vector (n/tag node))
       (let [[head component] (code-children node)]
         (and (= ":>" (some-> head n/string))
              (some? (some-> component n/string (resolve-component-tag ns-info component-aliases)))))))

(defn- unwrap-interop
  "Rewrite each aliased `[:> Comp …]` as `[Comp …]`, `:>` turned to whitespace so positions hold."
  [node ns-info component-aliases]
  (if-not (n/inner? node)
    node
    (let [children (n/children node)
          rewritten (mapv #(unwrap-interop % ns-info component-aliases) children)
          node (if (every? true? (map identical? children rewritten)) node (n/replace-children node rewritten))]
      (if (aliased-interop-node? node ns-info component-aliases)
        (n/replace-children node (let [[before [_ & after]] (split-with #(not= ":>" (n/string %)) (n/children node))]
                                   (concat before [(n/whitespace-node "  ")] after)))
        node))))

(defn- index-unwrapped-vectors
  "Map each written vector node to its rewritten loc, and each rewritten vector node back."
  [written-root unwrapped-root]
  (let [unwrapped-locs (IdentityHashMap.)
        written-nodes (IdentityHashMap.)]
    (loop [written (z/of-node* written-root)
           unwrapped (z/of-node* unwrapped-root)]
      (when-not (z/end? written)
        (when (= :vector (z/tag written))
          (.put unwrapped-locs (z/node written) unwrapped)
          (.put written-nodes (z/node unwrapped) (z/node written)))
        (recur (z/next* written) (z/next* unwrapped))))
    {:unwrapped-locs unwrapped-locs
     :written-nodes written-nodes}))

(defn- unwrapped-loc
  "Return loc's place in the file with every aliased `[:> Comp …]` read as a call."
  [written-loc ns-info component-aliases]
  (let [root-node (z/node (file-root written-loc))
        state (interop-index)
        cached @state]
    (when-not (identical? root-node (:written-root cached))
      (let [unwrapped (if (seq component-aliases)
                        (unwrap-interop root-node ns-info component-aliases)
                        root-node)]
        (reset! state (merge {:written-root root-node}
                             (if (identical? unwrapped root-node)
                               {:unwrapped-locs nil
                                :written-nodes nil}
                               (index-unwrapped-vectors root-node unwrapped))))))
    (or (some-> ^IdentityHashMap (:unwrapped-locs @state) (.get (z/node written-loc)))
        written-loc)))

(defn- handle-vector* [written-loc ns-info file {:keys [component-aliases label-components label-styles]}]
  (let [loc (unwrapped-loc written-loc ns-info component-aliases)
        first-child (z/down loc)]
    (when (and first-child
               (= :token (z/tag first-child))
               (not (hiccup/inside-non-hiccup-form? loc)))
      (when-let [tag (vector-tag loc ns-info component-aliases)]
        (let [info (element-info loc ns-info component-aliases)
              inside-label (delay (inside-label? loc {:label-components label-components
                                                      :ns-info ns-info}))
              aria-live-conflict (contradicting-aria-live info)
              repeated-name (repeated-accessible-name info tag loc ns-info component-aliases)
              nested-control (nested-interactive-loc info loc tag ns-info component-aliases)
              unreferenced-id (unreferenced-id-candidate info tag loc inside-label)
              separator (focusable-separator info tag loc)
              hidden-self? (aria-hidden-focusable? info tag)
              hidden-descendant (when-not hidden-self?
                                  (hidden-focusable-descendant info loc ns-info component-aliases))
              [row col] (parser/position loc)
              base {:kw tag
                    :form (source-snippet written-loc)
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

                       (and (empty-interactive? info tag loc) (not (element-shell? loc ns-info)))
                       (conj (assoc base :type :empty-interactive-element))

                       (missing-accessible-name? info tag loc inside-label)
                       (conj (assoc base :type :missing-accessible-name))

                       separator
                       (conj (assoc base
                                    :type ::focusable-separator
                                    :kw :separator
                                    :unnamed? (:unnamed? separator)))

                       unreferenced-id
                       (conj (assoc base
                                    :type ::unreferenced-id-candidate
                                    :id unreferenced-id))

                       (label-not-associated? info tag loc ns-info component-aliases)
                       (conj (assoc base :type :label-not-associated))

                       (aria-label-beside-visible-label? tag loc {:component-aliases component-aliases
                                                                  :label-styles label-styles
                                                                  :ns-info ns-info})
                       (conj (assoc base :type :aria-label-beside-visible-label))

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

(defn- analyze* [{:keys [usages usages-in-run]}]
  ;; The parser pools :usages across ALL enabled groups into one seq before
  ;; each group's `analyze` is called (see parser/analyze-project + core/run).
  ;; This filter is REQUIRED to keep other groups' usages (:sub, :event,
  ;; :style-call, ...) out of the a11y result — not defensive code.
  (let [by-type (group-by :type usages)
        run-files (filter #(= ::file-literals (:type %)) (or usages-in-run usages))
        literals (apply merge-with + (map :literals run-files))
        computed-reference? (some :computed-reference? run-files)
        unreferenced-ids (->> (::unreferenced-id-candidate by-type)
                              (filter #(unreferenced-id? literals computed-reference? %))
                              (map #(-> %
                                        (assoc :type :missing-accessible-name
                                               :hint (unreferenced-id-hint (:id %)))
                                        (dissoc :id))))
        separators-in-run (count (filter #(= ::focusable-separator (:type %)) (or usages-in-run usages)))
        unnamed-separators (when (< 1 separators-in-run)
                             (->> (::focusable-separator by-type)
                                  (filter :unnamed?)
                                  (map #(-> %
                                            (assoc :type :missing-accessible-name
                                                   :hint (unnamed-separator-hint separators-in-run))
                                            (dissoc :unnamed?)))))]
    {:img-alt-missing (vec (:img-alt-missing by-type))
     :invalid-tabindex (vec (:invalid-tabindex by-type))
     :on-click-on-non-interactive (vec (:on-click-on-non-interactive by-type))
     :empty-interactive-element (vec (:empty-interactive-element by-type))
     :missing-accessible-name (->> (concat (:missing-accessible-name by-type) unreferenced-ids unnamed-separators)
                                   (sort-by (juxt :file :row :col))
                                   vec)
     :label-not-associated (vec (:label-not-associated by-type))
     :aria-label-beside-visible-label (vec (:aria-label-beside-visible-label by-type))
     :aria-hidden-focusable (vec (:aria-hidden-focusable by-type))
     :nested-interactive-element (vec (:nested-interactive-element by-type))
     :repeated-accessible-name (vec (:repeated-accessible-name by-type))
     :aria-live-contradicts-role (vec (:aria-live-contradicts-role by-type))}))

(defn- summary-lines* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive
                               empty-interactive-element missing-accessible-name
                               repeated-accessible-name aria-live-contradicts-role
                               aria-hidden-focusable nested-interactive-element
                               label-not-associated aria-label-beside-visible-label]}]
  [["Img missing alt:" (count img-alt-missing)]
   ["Invalid tabindex:" (count invalid-tabindex)]
   ["Onclick on non-interactive:" (count on-click-on-non-interactive)]
   ["Empty interactive element:" (count empty-interactive-element)]
   ["Missing accessible name:" (count missing-accessible-name)]
   ["Repeated accessible name:" (count repeated-accessible-name)]
   ["Aria-live contradicts role:" (count aria-live-contradicts-role)]
   ["Aria-hidden focusable:" (count aria-hidden-focusable)]
   ["Nested interactive element:" (count nested-interactive-element)]
   ["Label not associated:" (count label-not-associated)]
   ["Aria-label beside visible label:" (count aria-label-beside-visible-label)]])

(defn- failed?* [{:keys [img-alt-missing invalid-tabindex on-click-on-non-interactive
                         empty-interactive-element missing-accessible-name
                         repeated-accessible-name aria-live-contradicts-role
                         aria-hidden-focusable nested-interactive-element
                         label-not-associated aria-label-beside-visible-label]}]
  (or (seq img-alt-missing)
      (seq invalid-tabindex)
      (seq on-click-on-non-interactive)
      (seq empty-interactive-element)
      (seq missing-accessible-name)
      (seq repeated-accessible-name)
      (seq aria-live-contradicts-role)
      (seq aria-hidden-focusable)
      (seq nested-interactive-element)
      (seq label-not-associated)
      (seq aria-label-beside-visible-label)))

(defrecord A11yGroup [component-aliases label-components label-styles]
  group/RuleGroup
  (group-id [_] :a11y)
  (group-name [_] "A11y")
  (parse-handlers [_]
    {:handle-vector (fn [loc ns-info file]
                      (handle-vector* loc ns-info file {:component-aliases component-aliases
                                                        :label-components label-components
                                                        :label-styles label-styles}))
     :handle-list (fn [loc _ns-info _file]
                    (file-literals loc))})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:img-alt-missing
     (str "Every :img needs :alt (WCAG 1.1.1). Use :alt \"\" for a decorative image, otherwise "
          "text that says what the image shows.")
     :invalid-tabindex
     (str "tabindex must be 0 or a negative integer (WCAG 2.4.3). A positive value breaks the natural "
          "focus order and a non-integer may not make the element focusable; use 0, or -1 for focus "
          "from code only.")
     :on-click-on-non-interactive
     (str "A mouse, pointer or touch handler sits on a non-interactive tag, so keyboard users cannot "
          "trigger it (WCAG 2.1.1). Use :button or :a with :href, or add a :role such as \"button\" "
          "and a keyboard handler like :on-key-down.")
     :empty-interactive-element
     (str "A button or link has no text and no :aria-label, :aria-labelledby or :title, so screen "
          "readers announce nothing (WCAG 4.1.2). Add visible text, or an :aria-label for an "
          "icon-only control.")
     :missing-accessible-name
     (str "A form control, dialog or named container has no accessible name (WCAG 4.1.2). Add :aria-label "
          "or :aria-labelledby with a visible label's id; a textarea or widget may take a [:label {:for id}]. "
          "A decorative :role \"img\" wants :aria-hidden true.")
     :repeated-accessible-name
     (str "Every item of a collection names its control with the same literal string, so a screen "
          "reader hears \"Remove, Remove, Remove\" (WCAG 2.4.6). Build the item into the name, as in "
          "(str \"Remove \" (:title item)), or point :aria-labelledby at the row's own label.")
     :aria-hidden-focusable
     (str "An element with :aria-hidden true, or something inside it, can still take focus, so a "
          "screen reader lands on silence (WCAG 4.1.2). Drop :aria-hidden and name the element, or "
          "for a mouse-only affordance set :tabIndex -1 and prevent focus on mouse down.")
     :label-not-associated
     (str "A :label is tied to no control, so clicking it focuses nothing and it names nothing "
          "(WCAG 1.3.1). Give it :for with the control's :id, or wrap the control inside it.")
     :aria-label-beside-visible-label
     (str "A field names itself with :aria-label right after a visible label, so two strings say what it "
          "is and drift apart when one is reworded (WCAG 2.5.3 Label in Name). Give the label an :id and "
          "point the field's :aria-labelledby at it, or make the label a [:label {:for id}].")
     :nested-interactive-element
     (str "An interactive element contains another one, which the HTML content model bans and screen "
          "readers announce as overlapping controls. Make the wrapper a plain :div with the two "
          "controls as siblings, or drop the outer control.")
     :aria-live-contradicts-role
     (str "The :aria-live politeness differs from what the :role implies, and the attribute wins "
          "(WAI-ARIA Implicit Value for Role). Drop :aria-live and let the role speak, or change it to "
          "match: \"status\" and \"log\" imply polite, \"alert\" assertive.")})
  (docs-anchors [_]
    {:aria-hidden-focusable "aria-hidden-on-a-focusable-element"
     :aria-label-beside-visible-label "aria-label-beside-visible-label"
     :aria-live-contradicts-role "aria-live-contradicts-role"
     :empty-interactive-element "empty-interactive-element"
     :img-alt-missing "img-missing-alt"
     :invalid-tabindex "invalid-tabindex"
     :label-not-associated "label-not-associated"
     :missing-accessible-name "missing-accessible-name"
     :nested-interactive-element "nested-interactive-element"
     :on-click-on-non-interactive "on-click-on-non-interactive-tag"
     :repeated-accessible-name "repeated-accessible-name"})
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
     :label-not-associated :bugs
     :aria-label-beside-visible-label :bugs})
  (file-extensions [_] #{".cljs" ".cljc"}))

(defn- alias-key
  "Read an alias key as a symbol; a string splits at its last slash, as \"@scope/pkg/Comp\"."
  [k]
  (if-not (string? k)
    k
    (let [slash (str/last-index-of k "/")]
      (if (and slash (pos? slash) (< slash (dec (count k))))
        (symbol (subs k 0 slash) (subs k (inc slash)))
        (throw (ex-info (str ":a11y :component-aliases string key must name a package and a component, "
                             "as \"@scope/package/Comp\", got " (pr-str k))
                        {:key k}))))))

(defn- normalize-alias
  "Read an alias value as `{:tag … :label-props #{…}}`, from a bare tag or a map.
  `:label-props` may be one keyword or a collection of them; any other shape throws,
  since reading it as no props would report every call it was written to name."
  [alias-value]
  (if-not (map? alias-value)
    {:tag alias-value}
    (let [label-props (:label-props alias-value)]
      (assoc alias-value
             :label-props (cond
                            (nil? label-props) #{}
                            (keyword? label-props) #{label-props}
                            (and (coll? label-props) (every? keyword? label-props)) (set label-props)
                            :else (throw (ex-info (str ":a11y :component-aliases :label-props must be "
                                                       "a keyword or a collection of keywords, got "
                                                       (pr-str label-props))
                                                  {:label-props label-props})))))))

(defn- normalize-aliases
  "Read the alias config, refusing two keys for one component."
  [component-aliases]
  (reduce (fn [aliases [k v]]
            (let [key-symbol (alias-key k)]
              (when (contains? aliases key-symbol)
                (throw (ex-info (str ":a11y :component-aliases maps " key-symbol " twice")
                                {:key key-symbol})))
              (assoc aliases key-symbol (normalize-alias v))))
          {}
          component-aliases))

(defn- normalize-label-styles
  "Read the label styles as a set of fully qualified symbols, refusing any other shape."
  [label-styles]
  (if (and (or (nil? label-styles) (coll? label-styles))
           (every? qualified-symbol? label-styles))
    (set label-styles)
    (throw (ex-info (str ":a11y :label-styles must be a collection of fully qualified symbols, got "
                         (pr-str label-styles))
                    {:label-styles label-styles}))))

(defn- normalize-label-components
  "Read the label components as a map from a fully qualified symbol to a set of prop keywords, refusing any other shape."
  [label-components]
  (if-not (or (nil? label-components) (map? label-components))
    (throw (ex-info (str ":a11y :label-components must be a map from a fully qualified symbol to its label props, got "
                         (pr-str label-components))
                    {:label-components label-components}))
    (reduce-kv (fn [components component label-props]
                 (when-not (qualified-symbol? component)
                   (throw (ex-info (str ":a11y :label-components key must be a fully qualified symbol, got "
                                        (pr-str component))
                                   {:key component})))
                 (assoc components component (cond
                                               (keyword? label-props) #{label-props}
                                               (and (coll? label-props) (seq label-props) (every? keyword? label-props))
                                               (set label-props)
                                               :else (throw (ex-info (str ":a11y :label-components " component
                                                                          " must name a keyword or a collection of"
                                                                          " keywords, got " (pr-str label-props))
                                                                     {:key component
                                                                      :label-props label-props})))))
               {}
               label-components)))

(defn make-group
  "Return an a11y RuleGroup configured with the given map (see `A11yGroup`).
  Supported keys:
    :component-aliases {full-qualified-symbol native-tag-keyword} — treat
      calls to the named symbol as the given native tag for a11y checks, written
      as a call or through `[:> Comp …]` interop. A key may be a string,
      \"@scope/package/Comp\", for a name no symbol can spell. The value may
      instead be {:tag native-tag-keyword :label-props #{prop …}}, naming the
      props the wrapper wires to an accessible name of its own.
    :label-components {full-qualified-symbol prop-or-props} — field wrappers that
      draw a `<label>` and give its id to their first child after the props, their
      control, when one of the props is set. The props are one keyword or a
      non-empty collection of them. A `some.ns/*` key lists every var there.
    :label-styles #{full-qualified-symbol …} — style functions that mark
      text as a field's visible label. Off until set."
  ([] (make-group nil))
  ([{:keys [component-aliases label-components label-styles]}]
   (->A11yGroup (normalize-aliases component-aliases)
                (normalize-label-components label-components)
                (normalize-label-styles label-styles))))

(def group (make-group))
