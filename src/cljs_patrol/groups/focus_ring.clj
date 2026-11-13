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

(defn- literal [loc]
  (when (= :token (z/tag loc))
    (try (z/sexpr loc) (catch Exception _ nil))))

(defn- without-important
  "Drop a trailing `!important`, which changes how a value cascades and not what it paints."
  [text]
  (str/trim (str/replace text #"(?i)\s*!important\s*$" "")))

(defn- css-text
  "Return the text Garden writes for a literal value, `!important` left out, or nil for a value that is not one."
  [value]
  (cond
    (string? value) (without-important (str/replace value #"\s+" " "))
    (keyword? value) (name value)
    (number? value) (str value)))

(defn- compact-color-text
  "Drop the spaces around parentheses, commas and slashes, so `rgb(81, 107, 255)` and `rgb(81,107,255)` compare equal."
  [text]
  (str/replace text #"\s*([(),/])\s*" "$1"))

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

(defn- selector-list
  "Split selector texts on commas, so `\"a::after, b::after\"` is read as the two selectors it holds."
  [texts]
  (mapcat #(str/split (str/trim %) #"\s*,\s*") texts))

(defn- blocks
  "Return every selector block under `locs`, each with the focus states of its own selectors and those above it.

  Only vectors written in the declaration are read: a block a call produces is not something
  this can see. A block's `:outer-maps` are the literal maps around it that apply to its element
  while its ring shows, where its outline may already be set. A block whose selectors all start
  with `&` is the element around it in a narrower state, or one of its pseudo-elements, so every
  map around it counts. A descendant, `[:input:focus …]`, is another element, and only the maps
  of the focus blocks around it count. A selector holding `:not(:focus-visible)` styles focus
  from a pointer, so that no ring shows, and takes `:focus` out of the states it inherits too."
  [locs patterns outer]
  (mapcat (fn [loc]
            (when (= :vector (z/tag loc))
              (let [{:keys [head maps vectors opaque?]} (block-parts loc)
                    texts (mapv selector-text head)
                    own-states (into #{} (mapcat #(focus-states % patterns)) texts)
                    pointer-only? (some #(str/includes? % ":not(:focus-visible)") texts)
                    states (cond-> (into (:states outer #{}) own-states)
                             pointer-only? (disj ":focus"))
                    path (conj (:path outer []) texts)
                    same-element? (and (seq texts) (every? #(str/starts-with? % "&") (selector-list texts)))
                    outer-maps (if same-element? (:element-maps outer []) (:focus-maps outer []))
                    block {:loc loc
                           :selector (str/join " " (concat (:selectors outer) (map parser/raw head)))
                           :path path
                           :states states
                           :maps maps
                           :opaque? (or opaque? (:opaque? outer false))
                           :outer-maps outer-maps}
                    inner {:selectors (concat (:selectors outer) (map parser/raw head))
                           :path path
                           :states states
                           :opaque? (:opaque? block)
                           :element-maps (concat outer-maps maps)
                           :focus-maps (if (seq states) (concat outer-maps maps) (:focus-maps outer []))}]
                (when (or (seq maps) (seq vectors) opaque?)
                  (cons block (blocks vectors patterns inner))))))
          locs))

(defn- value-tokens
  "Return every token under `loc`, itself included."
  [loc]
  (if (= :token (z/tag loc))
    [loc]
    (mapcat value-tokens (parser/child-locs loc))))

(defn- str-call? [loc]
  (and (= :list (z/tag loc))
       (= "str" (parser/sym-name (z/down loc)))))

(defn- unreadable-part?
  "True when a part of a composed value may hold anything: a call other than `str`, or a local."
  [loc ns-info]
  (case (z/tag loc)
    :token (let [value (literal loc)]
             (and (symbol? value) (nil? (resolve-symbol value ns-info))))
    :list (or (not (str-call? loc))
              (boolean (some #(unreadable-part? % ns-info) (rest (parser/child-locs loc)))))
    (boolean (some #(unreadable-part? % ns-info) (parser/child-locs loc)))))

(defn- composed-value
  "Classify a `str` or a Garden vector by its parts: the tokens to search, and whether a part may hold anything."
  [kind parts ns-info]
  {:kind kind
   :tokens (mapcat value-tokens parts)
   :unreadable? (boolean (some #(unreadable-part? % ns-info) parts))})

(defn- shadow-value
  "Classify a `:box-shadow` value: a reset, a var, a string, a `str` or a Garden vector built in place.

  Nil for a value that paints no ring, a keyword such as `:none`, or that this cannot read."
  [value-loc ns-info]
  (case (z/tag value-loc)
    :token (let [value (literal value-loc)]
             (cond
               (and (string? value) (contains? reset-values (str/lower-case (without-important value)))) {:kind :reset}
               (string? value) {:kind :string
                                :tokens [value-loc]}
               (symbol? value) (if-let [resolved (resolve-symbol value ns-info)]
                                 {:kind :var
                                  :symbol resolved
                                  :tokens [value-loc]}
                                 {:kind :local})))
    :list (when (str-call? value-loc)
            (composed-value :str (rest (parser/child-locs value-loc)) ns-info))
    :vector (composed-value :composed (parser/child-locs value-loc) ns-info)
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
             :form (parser/source-snippet loc)
             :file file
             :row row
             :col col}
      hint (assoc :hint hint))))

(def ^:private outline-longhands #{"outline-width" "outline-style" "outline-color"})

(defn- drawn-outline-text?
  "True when an `outline` value draws a line: no `none` or `hidden` style, and no zero width.

  What sits inside parentheses, the arguments of `rgb(0 0 0)` or a `calc(…)`, is no width."
  [text]
  (let [outside-parentheses (loop [current (str/lower-case text)]
                              (let [next-text (str/replace current #"\([^()]*\)" " ")]
                                (if (= next-text current) current (recur next-text))))
        parts (str/split (str/trim outside-parentheses) #"\s+")]
    (not-any? #(or (contains? #{"none" "hidden"} %)
                   (re-matches #"[+-]?0*\.?0+[a-z%]*" %))
              parts)))

(def ^:private default-outline "2px solid transparent")

(defn- outline-text
  "Normalize an outline for comparison: lower case, one space between parts, none inside a color, `!important` left out."
  [text]
  (-> text css-text str/lower-case compact-color-text))

(defn- outline-holds?
  "True when `maps`, read in the order the cascade applies them, end on the required outline, or one this cannot read.

  With no `expected` value, any outline that draws holds."
  [maps expected]
  (let [entries (mapcat map-entries maps)
        [_ _ value-loc] (last (filter #(= "outline" (first %)) entries))
        text (some-> value-loc literal css-text)]
    (cond
      (some #(contains? outline-longhands (first %)) entries) true
      (nil? value-loc) false
      (nil? text) true
      expected (= (outline-text text) (outline-text expected))
      :else (drawn-outline-text? text))))

(defn- outline-finding
  "Return a missing-outline finding when no block that may carry the ring's outline sets it.

  The block itself counts with the maps around it. A pseudo-element's ring also counts the
  element it decorates, whose outline forced-colors mode paints: it holds when every element
  the ring's selectors decorate has a block that sets the outline."
  [block key-loc expected decl-kw file]
  (let [holds? #(outline-holds? (concat (:outer-maps %) (:maps %)) expected)
        element-groups (:element-groups block)]
    (when-not (or (:opaque? block)
                  (some :opaque? (apply concat element-groups))
                  (holds? block)
                  (and (seq element-groups) (every? #(some holds? %) element-groups)))
      (finding :focus-ring-missing-outline block key-loc decl-kw file
               (str "Set :outline " (pr-str (or expected default-outline)) " beside :box-shadow.")))))

(defn- replacement
  "Return the approved shadows the raw tokens and colors in the value stand in for, sorted and each once, or nil."
  [symbols strings {:keys [replaced-symbols replaced-colors]}]
  (->> (concat (mapcat replaced-symbols symbols)
               (keep (fn [[pattern approved]] (when (some #(re-find pattern %) strings) approved)) replaced-colors))
       distinct
       (sort-by str)
       seq))

(defn- use-hint
  "Name the shadows to use: \"Use a.\", or \"Use one or more of: a, b.\" when the raw values belong to several.

  A raw value that several shadows list may stand for one of them or for each, so the hint
  lists them all and leaves the choice to the author."
  [shadows]
  (if (next shadows)
    (str "Use one or more of: " (str/join ", " shadows) ".")
    (str "Use " (first shadows) ".")))

(defn- raw-shadow-finding
  "Return a raw-shadow finding when a block's `:box-shadow` is not one of the approved shadows.

  A `str` or a Garden vector holding an approved shadow is a stacked ring and accepted, unless
  it also holds a raw value an approved shadow replaces. One that holds a local or a call may
  hold anything and is left alone."
  [block key-loc shadow {:keys [approved-shadows]
                         :as config} ns-info decl-kw file]
  (let [token-values (map literal (:tokens shadow))
        symbols (keep #(resolve-symbol % ns-info) token-values)
        strings (map compact-color-text (filter string? token-values))
        approved (replacement symbols strings config)
        accepted? (and (nil? approved)
                       (case (:kind shadow)
                         :var (contains? approved-shadows (:symbol shadow))
                         (:str :composed) (or (boolean (some approved-shadows symbols)) (:unreadable? shadow))
                         false))]
    (when-not accepted?
      (finding :focus-ring-raw-shadow block key-loc decl-kw file (some-> approved use-hint)))))

(defn- block-findings [block config ns-info decl-kw file preset?]
  (when (seq (:states block))
    (when-let [[_ key-loc value-loc] (entry-value (:maps block) "box-shadow")]
      (when-let [shadow (shadow-value value-loc ns-info)]
        (when-not (= :reset (:kind shadow))
          (->> [(when-not (false? (:outline config))
                  (outline-finding block key-loc (:outline config) decl-kw file))
                (when (and (seq (:approved-shadows config)) (not preset?) (not= :local (:kind shadow)))
                  (raw-shadow-finding block key-loc shadow config ns-info decl-kw file))]
               (remove nil?)))))))

(defn- element-groups
  "Return the blocks of the element each `::after` or `::before` selector of a block's last level decorates.

  A selector left with only `&`, as in a nested `[:&::after …]`, decorates the element of the
  level above. Any other is looked up among the blocks beside this one, in any block whose
  selectors hold it."
  [path by-path by-parent]
  (for [selector (selector-list (peek path))
        :let [base (str/replace selector #"(?i)::?(after|before)$" "")]
        :when (not= base selector)]
    (if (contains? #{"&" ""} base)
      (get by-path (pop path))
      (filter #(some #{base} (-> % :path peek selector-list)) (get by-parent (pop path))))))

(defn- base-maps
  "Return the maps a declaration's element always carries.

  Its own literal maps, and those of the blocks whose selectors include `&`."
  [body]
  (mapcat (fn [loc]
            (case (z/tag loc)
              :map [loc]
              :vector (let [{:keys [head maps]} (block-parts loc)
                            selectors (selector-list (map selector-text head))]
                        (when (some #{"&"} selectors)
                          maps))
              nil))
          body))

(defn- with-element-groups
  "Give each pseudo-element block the blocks of the elements it decorates, where its outline may be set.

  Forced-colors mode paints the element's outline, so a ring on `&:focus-visible::after` keeps
  showing when a sibling `&:focus-visible` block sets the outline."
  [all-blocks]
  (let [by-path (group-by :path all-blocks)
        by-parent (group-by (comp pop :path) all-blocks)]
    (map #(assoc % :element-groups (element-groups (:path %) by-path by-parent)) all-blocks)))

(defn- handle-list [config]
  (fn [loc {:keys [ns-name]
            :as ns-info} file]
    (when (contains? style-decl-fns (parser/sym-name (z/down loc)))
      (let [name-loc (parser/declared-name-loc loc)]
        (when-let [decl-name (parser/sym-name name-loc)]
          (let [decl-kw (keyword ns-name decl-name)
                preset? (contains? (:preset-namespaces config) ns-name)
                body (parser/right-siblings name-loc)
                findings (->> (blocks body (:patterns config) {:element-maps (base-maps body)})
                              with-element-groups
                              (mapcat #(block-findings % config ns-info decl-kw file preset?))
                              vec)]
            (when (seq findings)
              {:decls findings
               :usages []
               :dynamics []})))))))

(def ^:private rules
  [:focus-ring-missing-outline :focus-ring-raw-shadow])

(defn- analyze* [{:keys [declarations]}]
  (let [by-type (group-by :type declarations)]
    (into {} (map (fn [rule] [rule (vec (get by-type rule))])) rules)))

(defn- summary-lines* [result]
  [["Focus ring without an outline:" (count (:focus-ring-missing-outline result))]
   ["Focus ring not from the preset:" (count (:focus-ring-raw-shadow result))]])

(defrecord FocusRingGroup [config]
  group/RuleGroup
  (group-id [_] :focus-ring)
  (group-name [_] "Focus ring")
  (parse-handlers [_] (if config {:handle-list (handle-list config)} {}))
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (when config (summary-lines* result)))
  (failed? [_ result] (boolean (some #(seq (get result %)) rules)))
  (suggestions [_]
    {:focus-ring-missing-outline
     (str "A focus style paints a ring with box-shadow, which forced-colors mode drops. Set an outline that "
          "draws, such as 2px solid transparent, on this block or the focus block around it.")
     :focus-ring-raw-shadow
     "A focus style builds its box-shadow by hand. Use one or more of the approved focus-ring shadows from the preset."})
  (docs-anchors [_]
    {:focus-ring-missing-outline "focus-ring-without-an-outline"
     :focus-ring-raw-shadow "focus-ring-not-from-the-preset"})
  (rule->tier [_]
    {:focus-ring-missing-outline :bugs
     :focus-ring-raw-shadow :deprecations})
  (file-extensions [_] group/cljs-extensions))

(defn- state-pattern
  "Match a pseudo-class not followed by more of a name, so `:focus` does not match inside `:focus-visible`."
  [state]
  (re-pattern (str (Pattern/quote state) "(?![\\w-])")))

(defn- color-pattern
  "Match a color, in any case, as a whole: `#fff` does not match inside `#fffabc`, nor `red` inside `darkred`."
  [color]
  (let [text (compact-color-text color)]
    (re-pattern (str "(?i)(?<![\\w#-])" (Pattern/quote text) (if (str/starts-with? text "#") "(?![0-9a-f])" "(?![\\w-])")))))

(defn- invalid-config [message value]
  (throw (ex-info (str ":focus-ring :approved-shadows " message ", got "
                       (binding [*print-namespace-maps* false] (pr-str value)))
                  {:approved-shadows value})))

(defn- replaced-value?
  "True for a raw token, a fully qualified symbol, or a color, a string that does not read as a symbol.

  A color holds a `/` only between its arguments, inside parentheses: one with a `/` and no
  parenthesis is a symbol written as a string."
  [value]
  (or (qualified-symbol? value)
      (and (string? value) (not (and (str/includes? value "/") (not (str/includes? value "(")))))))

(defn- read-approved-shadows
  "Read `:approved-shadows` as [shadows replaced], refusing any other shape.

  Replaced pairs are [value shadow]."
  [approved]
  (let [shadows (if (map? approved) (keys approved) approved)]
    (when-not (and (or (nil? approved) (coll? approved)) (every? qualified-symbol? shadows))
      (invalid-config "must be a collection of fully qualified symbols, or a map from them" approved))
    (when (map? approved)
      (doseq [values (vals approved)]
        (when-not (or (nil? values) (and (coll? values) (every? replaced-value? values)))
          (invalid-config "must map each shadow to a collection of fully qualified symbols and colors" approved))))
    [(set shadows)
     (when (map? approved)
       (for [[shadow values] approved
             value values]
         [value shadow]))]))

(defn- normalize-config [config]
  (when (map? config)
    (let [selectors (set (or (seq (:selectors config)) default-selectors))
          [shadows replaced] (read-approved-shadows (:approved-shadows config))
          {color-pairs true
           symbol-pairs false} (group-by (comp string? first) replaced)]
      {:patterns (mapv (juxt identity state-pattern) selectors)
       :approved-shadows shadows
       :preset-namespaces (into #{} (map namespace) shadows)
       :replaced-symbols (reduce (fn [found [value shadow]] (update found value (fnil conj []) shadow)) {} symbol-pairs)
       :replaced-colors (mapv (fn [[value shadow]] [(color-pattern value) shadow]) color-pairs)
       :outline (:outline config)})))

(defn make-group
  "Return a focus-ring RuleGroup configured with the given map.

  Off until set. Supported keys:
    :approved-shadows — the vars whose value is an approved ring, fully qualified: a set, or a
      map from each to the raw tokens and colors it replaces, which a finding then names it for.
      Their namespaces, the preset, may build rings by hand.
    :outline \"2px solid transparent\" — the outline a focus block with a shadow must set.
      Unset, any outline that draws holds; false turns the check off.
    :selectors #{\":focus\" …} — focus pseudo-classes; a selector holding one, outside
      `:not(…)`, is a focus selector. Defaults to `:focus`, `:focus-visible`, `:focus-within`."
  ([] (make-group nil))
  ([config] (->FocusRingGroup (normalize-config config))))

(def group (make-group))
