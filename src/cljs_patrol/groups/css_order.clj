(ns cljs-patrol.groups.css-order
  "CSS property-order rule group: flags Spade style maps written out of outside-to-inside order."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.css-order.orders :as orders]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private style-decl-fns #{"defclass" "defattrs"})

(def ^:private min-properties
  "Smallest map worth ordering. Below this there is no meaningful outside-to-inside shape."
  4)

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [collapsed (str/replace (try (z/string loc) (catch Exception _ "")) #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

(defn- children [loc]
  (take-while some? (iterate z/right (z/down loc))))

(defn- selector-label [vector-loc]
  (->> (children vector-loc)
       (take-while #(not= :map (z/tag %)))
       (map parser/raw)
       (str/join " ")))

(defn- style-maps
  "Every map literal Spade reads as a style body, tagged with the selector path it sits under.
  Only literals nested directly in the declaration or in a selector vector are collected —
  a map a call produces, `(merge …)` or `(case …)`, is not something this can read."
  [loc selector]
  (mapcat (fn [child]
            (case (z/tag child)
              :map [{:map-loc child
                     :selector selector}]
              :vector (style-maps child (str/trim (str selector " " (selector-label child))))
              nil))
          (children loc)))

(defn- property-name
  "CSS property a map key names, or nil when the key is not one.
  Namespaced keywords are data, and an `&`-prefixed key is a selector that landed in the
  map — its own rule reports that, and ranking it here would report the same defect twice."
  [key-loc]
  (when (= :token (z/tag key-loc))
    (let [value (try (z/sexpr key-loc) (catch Exception _ nil))
          property (cond
                     (and (keyword? value) (nil? (namespace value))) (name value)
                     (string? value) value)]
      (when (and property (not (str/starts-with? property "&")))
        property))))

(defn- properties
  "The rankable keys of a style map, in source order."
  [order map-loc]
  (for [key-loc (take-nth 2 (children map-loc))
        :let [property (property-name key-loc)]
        :when property]
    {:property property
     :rank (orders/rank order property)
     :key (parser/raw key-loc)
     :row (parser/position-row key-loc)}))

(defn- first-violation
  "Return the first property ranked below one already seen, or nil when the block is in order.
  The finding carries the two properties that make the fix obvious: the earliest one it should
  have preceded, and the one it actually follows."
  [ordered]
  (loop [[property & remaining] (rest ordered)
         seen [(first ordered)]]
    (when property
      (if-let [outranked (first (filter #(> (:rank %) (:rank property)) seen))]
        {:property property
         :expected-before outranked
         :found-after (peek seen)}
        (recur remaining (conj seen property))))))

(def ^:private hint-order-limit
  "How many properties the hint spells out before trailing off.
  The full sequence stays on the finding for the EDN and HTML reports to use."
  10)

(defn- expected-order
  "The block's own keys in the order the table wants them.
  Sorting is stable, so properties the table does not rank keep their relative places at the end
  rather than being shuffled into an order nothing actually asked for."
  [ordered]
  (mapv :key (sort-by :rank ordered)))

(defn- order-hint [ordered]
  (let [keys (expected-order ordered)
        shown (take hint-order-limit keys)]
    (str "Order for this block: " (str/join " " shown)
         (when (> (count keys) hint-order-limit) " …"))))

(defn- block-finding [order {:keys [map-loc selector]} style-kw file]
  (let [ordered (properties order map-loc)]
    (when (>= (count ordered) min-properties)
      (when-let [{:keys [property expected-before found-after]} (first-violation ordered)]
        {:kw style-kw
         :type :css-property-order-outside-in
         :selector selector
         :property (:key property)
         :expected-before (:key expected-before)
         :found-after (:key found-after)
         :expected-order (expected-order ordered)
         :form (str style-kw " " (source-snippet map-loc))
         :hint (str "Move " (:key property) " before " (:key expected-before) ". "
                    (order-hint ordered))
         :file file
         :row (:row property)}))))

(defn- declared-name-loc
  "Return the symbol naming the declaration, reached through metadata when the name carries any.
  `(defclass ^:private foo …)` puts a meta node where the bare symbol would otherwise sit, and
  reading that slot directly would skip the whole declaration."
  [list-loc]
  (loop [loc (some-> list-loc z/down z/right)]
    (if (and loc (= :meta (z/tag loc)))
      (recur (last (children loc)))
      loc)))

(defn- handle-list [order loc {:keys [ns-name]} file]
  (when (contains? style-decl-fns (parser/sym-name (z/down loc)))
    (when-let [style-name (parser/sym-name (declared-name-loc loc))]
      (let [style-kw (keyword ns-name style-name)]
        {:decls (vec (keep #(block-finding order % style-kw file) (style-maps loc "")))
         :usages []
         :dynamics []}))))

(defn- analyze* [{:keys [declarations]}]
  {:css-property-order-outside-in
   (vec (filter #(= :css-property-order-outside-in (:type %)) declarations))})

(defn- summary-lines* [{:keys [css-property-order-outside-in]}]
  [["CSS property order:" (count css-property-order-outside-in)]])

(defn- suggestion [order]
  (let [{:keys [package url]} (get orders/sources order)]
    (str "Style map keys run out of the order " package " defines (" url "), embedded here verbatim "
         "and applied the way stylelint-order applies it to CSS. Each config draws the line between "
         "layout and looks differently — recess puts typography ahead of background and border, "
         "concentric puts border and background ahead of text — so read a finding against the table "
         "in use. Pick another with {:css-order {:order :smacss}} in .cljs-patrol/config.edn or "
         "--css-order smacss; available: " (str/join ", " (map name orders/names)) ". A property the "
         "table does not name — a custom property included — is left unordered and only reads as a "
         "problem when a ranked property follows it. Blocks under four properties are not judged, and "
         "only the first property out of place in each block is reported: resequencing the rest is a "
         "write-time call.")))

(defrecord CssOrderGroup [order]
  group/RuleGroup
  (group-id [_] :css-order)
  (group-name [_] (str "CSS order (" (name order) ")"))
  (parse-handlers [_] {:handle-list (partial handle-list order)})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ _] false)
  (suggestions [_] {:css-property-order-outside-in (suggestion order)})
  (rule->tier [_]
    {:css-property-order-outside-in :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(defn make-group
  "Return a css-order RuleGroup using the named property-order table.
  Supported keys:
    :order — one of cljs-patrol.groups.css-order.orders/names; defaults to :recess."
  ([] (make-group nil))
  ([{:keys [order]}]
   (->CssOrderGroup (orders/resolve-order order))))

(def group (make-group))
