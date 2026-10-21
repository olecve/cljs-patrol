(ns cljs-patrol.groups.css-order
  "CSS property-order rule group: flags Spade style maps written out of outside-to-inside order."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.css-order.orders :as orders]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.style-blocks :as blocks]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private min-properties
  "Smallest map worth ordering. Below this there is no meaningful outside-to-inside shape."
  4)

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [collapsed (str/replace (try (z/string loc) (catch Exception _ "")) #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

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
  (for [key-loc (blocks/map-key-locs map-loc)
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

(defn- handle-list [order loc {:keys [ns-name]} file]
  (when (blocks/style-decl? loc)
    (when-let [style-name (parser/sym-name (parser/declared-name-loc loc))]
      (let [style-kw (keyword ns-name style-name)]
        {:decls (->> loc blocks/style-maps (keep #(block-finding order % style-kw file)) vec)
         :usages []
         :dynamics []}))))

(defn- analyze* [{:keys [declarations]}]
  {:css-property-order-outside-in
   (vec (filter #(= :css-property-order-outside-in (:type %)) declarations))})

(defn- summary-lines* [{:keys [css-property-order-outside-in]}]
  [["CSS property order:" (count css-property-order-outside-in)]])

(defn- suggestion [order]
  (let [{:keys [package]} (get orders/sources order)]
    (str "Style map properties are out of the order " package " defines. Move the reported property "
         "as the hint says, or pick another table with {:css-order {:order :smacss}} in "
         ".cljs-patrol/config.edn or --css-order: " (str/join ", " (map name orders/names)) ".")))

(defrecord CssOrderGroup [order]
  group/RuleGroup
  (group-id [_] :css-order)
  (group-name [_] (str "CSS order (" (name order) ")"))
  (parse-handlers [_] {:handle-list (partial handle-list order)})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ _] false)
  (suggestions [_] {:css-property-order-outside-in (suggestion order)})
  (docs-anchors [_]
    {:css-property-order-outside-in "css-property-order-outside-in"})
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
