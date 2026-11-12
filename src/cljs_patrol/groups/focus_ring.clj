(ns cljs-patrol.groups.focus-ring
  "Rule group: focus rings in Spade styles that lose the forced-colors outline or skip the design system's preset.

  Windows forced-colors mode drops `box-shadow` and keeps `outline`, so a ring painted with a
  shadow needs a transparent outline beside it, and a ring built by hand drifts from the preset
  the next time the ring changes. Which shadows are approved and which outline goes with them
  is a codebase's own choice, so the group does nothing until `:focus-ring` is configured."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z])
  (:import
   (java.util.regex
    Pattern)))

(def ^:private style-decl-fns #{"defclass" "defattrs" "defglobal"})

(def ^:private default-selectors #{":focus" ":focus-visible" ":focus-within"})

(def ^:private reset-values
  "Values of `:box-shadow` that paint no ring, so no outline needs to go with them."
  #{"none" "unset" "initial" "inherit" "revert" "revert-layer"})

(def ^:private custom-property-reference
  "A value that is only a CSS custom property, set somewhere this cannot see, so it is read like a local."
  #"\s*var\(--[\w-]+\)\s*(!important)?\s*")

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [collapsed (str/replace (try (z/string loc) (catch Exception _ "")) #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

(defn- literal [loc]
  (when (= :token (z/tag loc))
    (try (z/sexpr loc) (catch Exception _ nil))))

(defn- css-text
  "Return the text Garden writes for a literal value, or nil for a value that is not one."
  [value]
  (cond
    (string? value) (str/trim (str/replace value #"\s+" " "))
    (keyword? value) (name value)
    (number? value) (str value)))

(defn- without-negations
  "Remove every `:not(…)` from a selector, the parentheses balanced, so a state it excludes is not read as one it selects."
  [selector]
  (loop [text selector]
    (if-let [start (str/index-of text ":not(")]
      (let [end (loop [index (+ start 5)
                       depth 1]
                  (cond
                    (>= index (count text)) index
                    (zero? depth) index
                    :else (recur (inc index)
                                 (case (.charAt ^String text index)
                                   \( (inc depth)
                                   \) (dec depth)
                                   depth))))]
        (recur (str (subs text 0 start) (subs text end))))
      text)))

(defn- focus-states
  "Return the configured focus pseudo-classes `selector` selects, outside any `:not(…)`."
  [selector patterns]
  (let [text (without-negations selector)]
    (set (keep (fn [[state pattern]] (when (re-find pattern text) state)) patterns))))

(defn- selector-text
  "Return a selector token's text as Garden reads it, or nil for a selector that is not a literal."
  [loc]
  (let [value (literal loc)]
    (cond
      (string? value) value
      (keyword? value) (name value))))

(defn- resolve-symbol
  "Return the fully qualified symbol a token names, or nil for a local or a name no alias explains."
  [value ns-info]
  (when (symbol? value)
    (let [text (str value)]
      (or (parser/resolve-full-symbol text ns-info)
          (when (and (namespace value) (str/includes? (namespace value) "."))
            value)))))

(defn- map-entries
  "Return [key-text value-loc] for each entry of a literal map whose key is a keyword or a string."
  [map-loc]
  (->> (parser/child-locs map-loc)
       (remove #(= :uneval (z/tag %)))
       (partition 2)
       (keep (fn [[key-loc value-loc]]
               (let [k (literal key-loc)]
                 (when (or (keyword? k) (string? k))
                   [(name k) key-loc value-loc]))))))

(defn- block-parts
  "Split a selector vector into its selector locs, its literal maps, its nested vectors, and whether anything else sits in it.

  Only keywords and strings lead as selectors. A symbol or a call there may stand for a map,
  so it makes the block one this cannot read in full, the same as one after the selectors."
  [vector-loc]
  (let [children (remove #(= :uneval (z/tag %)) (parser/child-locs vector-loc))
        [head body] (split-with selector-text children)]
    {:head head
     :maps (filter #(= :map (z/tag %)) body)
     :vectors (filter #(= :vector (z/tag %)) body)
     :opaque? (boolean (some #(not (contains? #{:map :vector} (z/tag %))) body))}))

(defn- blocks
  "Return every selector block under `locs`, each with the focus states of its own selectors and those above it.

  Only vectors written in the declaration are read: a block a call produces is not something
  this can see. A block's `:outer-maps` are the literal maps of the focus blocks around it,
  where its outline may already be set."
  [locs patterns outer]
  (mapcat (fn [loc]
            (when (= :vector (z/tag loc))
              (let [{:keys [head maps vectors opaque?]} (block-parts loc)
                    texts (map selector-text head)
                    own-states (into #{} (mapcat #(when % (focus-states % patterns))) texts)
                    states (into (:states outer #{}) own-states)
                    block {:loc loc
                           :selector (str/join " " (concat (:selectors outer) (map parser/raw head)))
                           :states states
                           :maps maps
                           :opaque? (or opaque? (:opaque? outer false))
                           :outer-maps (:maps outer [])}
                    inner {:selectors (concat (:selectors outer) (map parser/raw head))
                           :states states
                           :opaque? (:opaque? block)
                           :maps (cond-> (:outer-maps block) (seq states) (into maps))}]
                (when (or (seq maps) (seq vectors))
                  (cons block (blocks vectors patterns inner))))))
          locs))

(defn- value-tokens
  "Return every token under `loc`, itself included."
  [loc]
  (if (= :token (z/tag loc))
    [loc]
    (mapcat value-tokens (parser/child-locs loc))))

(defn- shadow-value
  "Classify a `:box-shadow` value: a reset, a var, a string, a `str` or a Garden vector built in place.

  Nil for a value that paints no ring, a keyword such as `:none`, or that this cannot read."
  [value-loc ns-info]
  (case (z/tag value-loc)
    :token (let [value (literal value-loc)]
             (cond
               (and (string? value) (contains? reset-values (str/lower-case (str/trim value)))) {:kind :reset}
               (and (string? value) (re-matches custom-property-reference value)) {:kind :local}
               (string? value) {:kind :string
                                :tokens [value-loc]}
               (symbol? value) (if-let [resolved (resolve-symbol value ns-info)]
                                 {:kind :var
                                  :symbol resolved
                                  :tokens [value-loc]}
                                 {:kind :local})))
    :list (when (= "str" (parser/sym-name (z/down value-loc)))
            {:kind :str
             :tokens (->> value-loc parser/child-locs rest (mapcat value-tokens))})
    :vector {:kind :composed
             :tokens (value-tokens value-loc)}
    nil))

(defn- entry-value
  "Return the value loc of the last entry named `key-name` across `maps`, or nil."
  [maps key-name]
  (->> maps
       (mapcat map-entries)
       (filter #(= key-name (first %)))
       last))

(defn- finding [rule {:keys [loc selector]} key-loc decl-kw file hint]
  (let [[row col] (parser/position key-loc)]
    (cond-> {:kw decl-kw
             :type rule
             :selector selector
             :form (source-snippet loc)
             :file file
             :row row
             :col col}
      hint (assoc :hint hint))))

(defn- companion-finding
  "Return a missing-companion finding when the block sets a shadow without every companion property at its configured value."
  [block key-loc required decl-kw file]
  (when-not (:opaque? block)
    (let [maps (concat (:outer-maps block) (:maps block))
          problems (keep (fn [[property expected]]
                           (let [[_ _ value-loc] (entry-value maps (name property))
                                 actual (some-> value-loc literal css-text)]
                             (cond
                               (nil? value-loc) property
                               (and (= :token (z/tag value-loc)) (some? actual) (not= actual (css-text expected))) property)))
                         required)]
      (when (seq problems)
        (finding :focus-ring-missing-companion block key-loc decl-kw file
                 (str "Set " (str/join " and " (map (fn [property]
                                                      (str property " " (pr-str (get required property))))
                                                    problems))
                      " beside :box-shadow."))))))

(defn- source-findings
  "Return the findings about where a block's `:box-shadow` value comes from."
  [block key-loc shadow {:keys [approved-shadows allow-stacking-under forbidden-tokens forbidden-color-literals]} ns-info decl-kw
   file]
  (let [token-values (map literal (:tokens shadow))
        symbols (keep #(resolve-symbol % ns-info) token-values)
        strings (map str/lower-case (filter string? token-values))
        forbidden-token (first (filter #(contains? forbidden-tokens %) symbols))
        forbidden-color (first (filter (fn [color] (some #(str/includes? % color) strings)) forbidden-color-literals))
        approved? (case (:kind shadow)
                    :var (contains? approved-shadows (:symbol shadow))
                    :str (and (seq (filter #(contains? allow-stacking-under %) (:states block)))
                              (boolean (some #(contains? approved-shadows %) symbols)))
                    false)]
    (cond-> []
      forbidden-token (conj (finding :focus-ring-forbidden-token block key-loc decl-kw file
                                     (when-let [approved (get forbidden-tokens forbidden-token)]
                                       (str "Use " approved "."))))
      forbidden-color (conj (finding :focus-ring-forbidden-color block key-loc decl-kw file nil))
      (and (not forbidden-token) (not forbidden-color) (seq approved-shadows) (not approved?))
      (conj (finding :focus-ring-raw-shadow block key-loc decl-kw file nil)))))

(defn- block-findings [block config ns-info decl-kw file exempt?]
  (when (seq (:states block))
    (when-let [[_ key-loc value-loc] (entry-value (:maps block) "box-shadow")]
      (when-let [shadow (shadow-value value-loc ns-info)]
        (when-not (= :reset (:kind shadow))
          (let [companion (companion-finding block key-loc (:required-companion config) decl-kw file)]
            (cond-> (if companion [companion] [])
              (and (not exempt?) (not= :local (:kind shadow)))
              (into (source-findings block key-loc shadow config ns-info decl-kw file)))))))))

(defn- exempt-path?
  "True when `file` is one of the configured paths, written relative to the project."
  [file exempt-paths]
  (let [path (str/replace (str file) "\\" "/")]
    (boolean (some #(or (= path %) (str/ends-with? path (str "/" %))) exempt-paths))))

(defn- handle-list [config]
  (fn [loc {:keys [ns-name]
            :as ns-info} file]
    (when (contains? style-decl-fns (parser/sym-name (z/down loc)))
      (let [name-loc (parser/declared-name-loc loc)]
        (when-let [decl-name (parser/sym-name name-loc)]
          (let [decl-kw (keyword ns-name decl-name)
                exempt? (exempt-path? file (:exempt-paths config))
                findings (->> (blocks (parser/right-siblings name-loc) (:patterns config) {})
                              (mapcat #(block-findings % config ns-info decl-kw file exempt?))
                              vec)]
            (when (seq findings)
              {:decls findings
               :usages []
               :dynamics []})))))))

(def ^:private rules
  [:focus-ring-missing-companion :focus-ring-raw-shadow :focus-ring-forbidden-token :focus-ring-forbidden-color])

(defn- analyze* [{:keys [declarations]}]
  (let [by-type (group-by :type declarations)]
    (into {} (map (fn [rule] [rule (vec (get by-type rule))])) rules)))

(defn- summary-lines* [result]
  [["Focus ring without the forced-colors companion:" (count (:focus-ring-missing-companion result))]
   ["Focus ring not from the preset:" (count (:focus-ring-raw-shadow result))]
   ["Focus ring from a forbidden token:" (count (:focus-ring-forbidden-token result))]
   ["Focus ring with a forbidden color:" (count (:focus-ring-forbidden-color result))]])

(defrecord FocusRingGroup [config]
  group/RuleGroup
  (group-id [_] :focus-ring)
  (group-name [_] "Focus ring")
  (parse-handlers [_] (if config {:handle-list (handle-list config)} {}))
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (when config (summary-lines* result)))
  (failed? [_ result] (boolean (some #(seq (get result %)) rules)))
  (suggestions [_]
    {:focus-ring-missing-companion
     (str "A focus style paints a ring with box-shadow, which forced-colors mode drops. Set the configured "
          "companion, such as a transparent outline, in the same block so the ring still shows there.")
     :focus-ring-raw-shadow
     "A focus style builds its box-shadow by hand. Use one of the approved focus-ring shadows from the preset."
     :focus-ring-forbidden-token
     "A focus style paints its box-shadow from a raw theme token. Use the approved focus-ring shadow instead."
     :focus-ring-forbidden-color
     "A focus style paints its box-shadow with a raw brand color. Use one of the approved focus-ring shadows."})
  (docs-anchors [_]
    {:focus-ring-missing-companion "focus-ring-without-the-forced-colors-companion"
     :focus-ring-raw-shadow "focus-ring-not-from-the-preset"
     :focus-ring-forbidden-token "focus-ring-from-a-forbidden-token"
     :focus-ring-forbidden-color "focus-ring-with-a-forbidden-color"})
  (rule->tier [_]
    {:focus-ring-missing-companion :bugs
     :focus-ring-raw-shadow :deprecations
     :focus-ring-forbidden-token :deprecations
     :focus-ring-forbidden-color :deprecations})
  (file-extensions [_] group/cljs-extensions))

(defn- state-pattern
  "Match a pseudo-class not followed by more of a name, so `:focus` does not match inside `:focus-visible`."
  [state]
  (re-pattern (str (Pattern/quote state) "(?![\\w-])")))

(defn- normalize-config [config]
  (when (map? config)
    (let [selectors (set (or (seq (:selectors config)) default-selectors))
          forbidden-tokens (:forbidden-tokens config)]
      {:patterns (mapv (juxt identity state-pattern) selectors)
       :approved-shadows (set (:approved-shadows config))
       :allow-stacking-under (set (:allow-stacking-under config))
       :required-companion (into {} (:required-companion config))
       :forbidden-tokens (if (map? forbidden-tokens)
                           forbidden-tokens
                           (zipmap forbidden-tokens (repeat nil)))
       :forbidden-color-literals (->> config :forbidden-color-literals (map str/lower-case) set)
       :exempt-paths (set (:exempt-paths config))})))

(defn make-group
  "Return a focus-ring RuleGroup configured with the given map.

  Off until set. Supported keys:
    :selectors #{\":focus\" …} — focus pseudo-classes; a selector holding one, outside
      `:not(…)`, is a focus selector. Defaults to `:focus`, `:focus-visible`, `:focus-within`.
    :approved-shadows #{full-qualified-symbol …} — vars whose value is an approved ring.
    :allow-stacking-under #{\":focus-visible\" …} — focus states under which a `(str …)`
      holding an approved shadow is accepted.
    :required-companion {:outline \"2px solid transparent\"} — properties a focus block
      with a shadow must also set.
    :forbidden-tokens {full-qualified-symbol approved-symbol} — raw tokens, each mapped to
      the approved var to suggest, or a set when there is none to suggest.
    :forbidden-color-literals #{\"#516bff\" …} — colors no focus shadow may hold.
    :exempt-paths #{\"src/app/styles/focus_rings.cljs\" …} — files that may build a ring
      by hand; the companion is still required there."
  ([] (make-group nil))
  ([config] (->FocusRingGroup (normalize-config config))))

(def group (make-group))
