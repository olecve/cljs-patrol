(ns cljs-patrol.groups.layout
  "Rule group: sibling forms sharing a line with a map split over several lines.

  cljfmt's `:split-keypairs-over-multiple-lines?` puts every key-value pair of a
  multi-key map on its own line, but leaves the elements of a vector, list or call
  where they are. A map that shares a line with a sibling form then splits into a
  staircase that `cljfmt check` accepts and nobody can read. No cljfmt or clj-kondo
  option prevents it, so this group reports the two shapes it takes."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private map-tags #{:map :namespaced-map})

(defn- span
  "Return [start-row end-row] of loc, or nil when positions are not tracked."
  [loc]
  (try (let [[[start-row] [end-row]] (z/position-span loc)] [start-row end-row])
       (catch Exception _ nil)))

(defn- start-row [loc] (first (span loc)))

(defn- end-row [loc] (second (span loc)))

(defn- multi-line? [loc]
  (let [[start end] (span loc)]
    (boolean (and start end (< start end)))))

(defn- split-map? [loc]
  (and (contains? map-tags (z/tag loc)) (multi-line? loc)))

(defn- last-child [loc]
  (when (z/down loc) (z/rightmost (z/down loc))))

(defn- closes-split-map?
  "True when the last line of loc is the closing line of a split map.

  Either loc is one, or the form it ends with is, at any depth: a Hiccup child ending in
  a split props map closes on the same line its map does, so a form following it on that
  line sits after the map just the same."
  [loc]
  (loop [current loc]
    (cond
      (nil? current) false
      (split-map? current) true
      :else (let [child (when (z/down current) (last-child current))]
              (when (and child (= (end-row child) (end-row current)))
                (recur child))))))

(def ^:private binding-vector-heads
  "Forms whose first argument is a vector of binding pairs."
  #{"let" "let*" "loop" "loop*" "when-let" "if-let" "when-some" "if-some" "when-first"
    "binding" "with-redefs" "with-open" "with-let" "doseq" "for" "dotimes" "go-loop"
    "with-local-vars"})

(def ^:private pair-list-starts
  "Forms whose arguments run in pairs, mapped to the child index the first pair starts at.

  The head is index 0. A pair written on one line — a `cond` test and its branch, a `case`
  constant and its result — is the form's own layout, not one sibling crowding another."
  {"cond" 1
   "case" 2
   "cond->" 2
   "cond->>" 2
   "condp" 3})

(defn- head-name [list-loc]
  (some-> list-loc z/down parser/sym-name))

(defn- binding-vector?
  "True when vector-loc holds binding pairs.

  A `let`-like form's bindings, or a `:let` modifier inside a `for` or `doseq`."
  [vector-loc]
  (let [parent (z/up vector-loc)]
    (or (and (= :list (some-> parent z/tag))
             (contains? binding-vector-heads (head-name parent))
             (identical? (z/node vector-loc) (some-> parent z/down z/right z/node)))
        (and (= ":let" (some-> vector-loc z/left parser/raw))
             (= :vector (some-> parent z/tag))
             (binding-vector? parent)))))

(defn- pair-start
  "Return the child index binding pairs start at in container-loc, or nil."
  [container-loc]
  (case (z/tag container-loc)
    :vector (when (binding-vector? container-loc) 0)
    :list (if (= :reader-macro (some-> container-loc z/up z/tag))
            0
            (get pair-list-starts (head-name container-loc)))
    nil))

(defn- pair-partners?
  "True when the children at index i and i + 1 are the two halves of one binding pair."
  [start i]
  (boolean (and start (>= i start) (even? (- i start)))))

(def ^:private fn-heads #{"fn" "fn*" "defn" "defn-" "defmethod" "defmacro"})

(defn- parameter-vector?
  "True when vector-loc is the parameter list of a fn-like form, single- or multi-arity."
  [vector-loc]
  (let [parent (z/up vector-loc)
        in-fn? (fn [list-loc] (contains? fn-heads (head-name list-loc)))]
    (and (= :list (some-> parent z/tag))
         (or (and (in-fn? parent)
                  (= :vector (z/tag vector-loc))
                  (not (some #(= :vector (z/tag %))
                             (take-while #(not (identical? (z/node %) (z/node vector-loc)))
                                         (iterate z/right (z/down parent))))))
             (and (identical? (z/node vector-loc) (some-> parent z/down z/node))
                  (= :list (some-> parent z/up z/tag))
                  (in-fn? (z/up parent)))))))

(defn- destructuring-vector?
  "True when vector-loc binds names rather than building data or Hiccup.

  A parameter list, the binding side of a `let`-like pair, or a vector nested in either.
  Only there is a leading symbol a binding — `[_ {:keys [id]}]` — rather than the head of
  a Hiccup element, `[button {:size :s}]`."
  [vector-loc]
  (let [parent (z/up vector-loc)]
    (boolean
     (or (parameter-vector? vector-loc)
         (and (= :vector (some-> parent z/tag))
              (binding-vector? parent)
              (even? (count (take-while some? (iterate z/left (z/left vector-loc))))))
         (and (contains? #{:vector :map} (some-> parent z/tag))
              (or (and (= :vector (z/tag parent)) (destructuring-vector? parent))
                  (and (= :map (z/tag parent))
                       (= :vector (some-> parent z/up z/tag))
                       (destructuring-vector? (z/up parent)))))))))

(defn- symbol-token? [loc]
  (and (= :token (z/tag loc))
       (some? (parser/sym-name loc))))

(defn- single-line-string? [loc]
  (and (= :token (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))
       (not (multi-line? loc))))

(defn- head?
  "True when the child at index i is the head of container-loc rather than a sibling.

  A list's first child is always its head. A vector's is only when it names something —
  a Hiccup tag or component — and the vector is not binding names."
  [container-loc i child]
  (and (zero? i)
       (case (z/tag container-loc)
         (:list :fn) true
         :vector (and (or (symbol-token? child) (parser/kw-node? child))
                      (not (destructuring-vector? container-loc)))
         false)))

(defn- children [container-loc]
  (->> (iterate z/right (z/down container-loc))
       (take-while some?)
       (remove #(= :uneval (z/tag %)))
       vec))

(def ^:private part-max 60)

(defn- part [loc]
  (let [text (parser/normalize-form (parser/raw loc))]
    (if (<= (count text) part-max)
      text
      (str (subs text 0 (- part-max 3)) "..."))))

(defn- container-kind
  "Name what holds the two siblings: the call's head, or `vector`.

  A symbol that reads back as itself, since the baseline round-trips it through EDN —
  `(symbol \"[]\")` would come back as an empty vector and match nothing."
  [container-loc]
  (if (= :vector (z/tag container-loc))
    'vector
    (symbol (or (head-name container-loc) "list"))))

(def ^:private hints
  {:form-after-split-map "Start this form on its own line, aligned with the one before it."
   :split-map-after-sibling "Start this map on its own line, aligned with the form before it."})

(defn- top-level-name
  "Return what the top-level form holding loc defines, or nil.

  The var a `def`-like form names, or the keyword a registration such as
  `(reg-event-fx ::saved …)` is keyed by."
  [loc]
  (let [top (last (take-while #(and % (not= :forms (z/tag %))) (iterate z/up loc)))
        head (some-> top z/down parser/sym-name)]
    (when (and head (= :list (z/tag top)))
      (if (str/starts-with? head "def")
        (some-> (parser/declared-name-loc top) parser/sym-name)
        (let [id (some-> top z/down z/right)]
          (when (parser/kw-node? id) (parser/raw id)))))))

(defn- finding
  "Build the finding for sibling b crowding a.

  `:test` is the definition the two sit in, which the baseline keys on alongside the
  form. A layout finding's form is short and repeats — the same `{:run-op-id …} #{:loading}`
  call appears in several handlers of one file — so without it they would be one entry."
  [rule container-loc a b file]
  (let [[row col] (try (z/position b) (catch Exception _ [0 1]))]
    (cond-> {:kw (container-kind container-loc)
             :type rule
             :form (str (part a) " " (part b))
             :file file
             :row row
             :col col
             :hint (get hints rule)}
      (top-level-name a) (assoc :test (top-level-name a)))))

(defn- path-vector?
  "True when loc is a vector holding nothing but plain tokens.

  `[:drafting-editor :draft]` as `assoc-in` takes it, `[:current-route]` as a test seeds
  app-db with."
  [loc]
  (and (= :vector (z/tag loc))
       (every? #(= :token (z/tag %)) (children loc))))

(defn- destructuring-container? [container-loc]
  (and (= :vector (z/tag container-loc)) (destructuring-vector? container-loc)))

(defn- sibling-kind
  "Classify a as a sibling a split map may follow: `:counted`, `:opt-in` or nil.

  A map or a vector counts. A plain symbol in a parameter list or binding — the `_` of
  an event vector, `[_ {:keys [id] :as user}]` — counts only when configured to, since
  cljfmt leaves it before a split map all over a typical codebase and most read the
  result as fine. A symbol anywhere else, `(reset! state {…})`, and a path vector of
  plain tokens, `(assoc-in db [:a :b] {…})`, are the ordinary way to pass a target and
  its value, and never count. Inside a binding form a vector of symbols is a pattern,
  `[[named seen] {:keys [label] :as item}]`, and counts like any other vector."
  [container-loc a]
  (let [destructuring? (destructuring-container? container-loc)]
    (cond
      (symbol-token? a) (when destructuring? :opt-in)
      (contains? map-tags (z/tag a)) :counted
      (= :vector (z/tag a)) (when (or destructuring? (not (path-vector? a))) :counted))))

(defn- hiccup-like?
  "True when container-loc is a vector headed by a tag, component or event id.

  A vector of data or of bindings has no such head."
  [container-loc]
  (let [first-child (z/down container-loc)]
    (boolean (and first-child (head? container-loc 0 first-child)))))

(defn- lone-symbol?
  "True when loc is a symbol with nothing after it on its line."
  [loc]
  (and (symbol-token? loc)
       (let [next-sibling (z/right loc)]
         (or (nil? next-sibling) (> (start-row next-sibling) (end-row loc))))))

(defn- crowds-split-map?
  "True when b, on the closing line of the split map a, is a sibling it should not carry.

  Every argument of a call counts, and every child of a Hiccup element but a single-line
  string, which reads as the element's label, and a lone symbol ending the line, which
  reads the same way: `[button {…} items-per-page]`, or a Malli entry's `int?`. A symbol
  with more forms after it on that line counts. In a vector of data or of bindings only a
  map or a form spanning lines counts: a parameter list `[{:keys [db] :as cofx} event]`
  keeps its next parameter where cljfmt leaves it."
  [container-loc b]
  (case (z/tag container-loc)
    (:list :fn) true
    :vector (if (hiccup-like? container-loc)
              (not (or (single-line-string? b) (lone-symbol? b)))
              (or (contains? map-tags (z/tag b)) (multi-line? b)))
    false))

(defn- structural-vector?
  "True when a is a fn's parameter list or a `let`-like form's bindings.

  What follows one is the form's body, not a sibling crowding it, so a split map after
  `(fn [x]` is cljfmt's ordinary layout."
  [a]
  (and (= :vector (z/tag a))
       (or (parameter-vector? a) (binding-vector? a))))

(defn- shape
  "Return the rule the sibling b breaks by sharing a line with a, or nil.

  `symbol-sibling?` widens the second rule to a split map after a plain symbol in a
  parameter list or binding, `(fn [_ {:keys [id] :as user}])` once cljfmt splits the
  map, and is off unless configured."
  [container-loc start i a b symbol-sibling?]
  (when (and (= (end-row a) (start-row b))
             (not (pair-partners? start i)))
    (cond
      (if (split-map? a)
        (crowds-split-map? container-loc b)
        (and (closes-split-map? a) (multi-line? b)))
      :form-after-split-map

      (and (split-map? b)
           (not (head? container-loc i a))
           (not (structural-vector? a))
           (case (sibling-kind container-loc a)
             :counted true
             :opt-in symbol-sibling?
             false))
      :split-map-after-sibling)))

(defn- check-container [symbol-sibling? container-loc file]
  (let [cs (children container-loc)
        start (pair-start container-loc)]
    (->> (range (dec (count cs)))
         (keep (fn [i]
                 (let [a (nth cs i)
                       b (nth cs (inc i))]
                   (when-let [rule (shape container-loc start i a b symbol-sibling?)]
                     (finding rule container-loc a b file)))))
         vec)))

(defn- handle [symbol-sibling? loc _ns-info file]
  (let [found (check-container symbol-sibling? loc file)]
    (when (seq found)
      {:decls found
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:form-after-split-map (vec (filter #(= :form-after-split-map (:type %)) declarations))
   :split-map-after-sibling (vec (filter #(= :split-map-after-sibling (:type %)) declarations))})

(defn- summary-lines* [{:keys [form-after-split-map split-map-after-sibling]}]
  [["Form after split map:" (count form-after-split-map)]
   ["Split map after sibling:" (count split-map-after-sibling)]])

(defrecord LayoutGroup [symbol-sibling?]
  group/RuleGroup
  (group-id [_] :layout)
  (group-name [_] "Layout")
  (parse-handlers [_] {:handle-list (fn [loc ns-info file] (handle symbol-sibling? loc ns-info file))
                       :handle-vector (fn [loc ns-info file] (handle symbol-sibling? loc ns-info file))})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  ;; Layout has no runtime effect. A project that wants it enforced opts in with --fail-on.
  (failed? [_ _] false)
  (suggestions [_]
    {:form-after-split-map
     (str "A form starts on the line where a map split over several lines closes: the next "
          "map of a vector, the next argument of a call, the next child of a Hiccup element. "
          "cljfmt's :split-keypairs-over-multiple-lines? splits every map onto one line per "
          "pair but leaves the forms around it where they are, so the result passes "
          "`cljfmt check` and reads as a staircase. Put each sibling on its own line, aligned "
          "with the first. A short string child on the closing line of a Hiccup element's "
          "props, [button {...} \"Edit\"], is left alone, as is the second half of a binding "
          "pair, a cond clause or a case clause.")
     :split-map-after-sibling
     (str "A map split over several lines starts after a whole sibling form on the same "
          "line, as the second map of [{:keys [db]} {:keys [id] :as profile}] or of "
          "(f {:db db} {:id id :source s}) does once cljfmt splits it. Its pairs then hang "
          "off a column far to the right of the form they belong to. Start the map on its own "
          "line, aligned with the sibling before it. Only a map or vector sibling counts by "
          "default, and not a path vector of plain tokens, a fn's parameters or a let's "
          "bindings; a map after the head of a call or Hiccup element is never one. Set "
          ":layout {:split-map-after-symbol true} to also report a map after a plain symbol "
          "in a parameter list or binding, as in (fn [_ {:keys [id] :as user}]).")})
  (rule->tier [_]
    {:form-after-split-map :cleanup
     :split-map-after-sibling :cleanup})
  (file-extensions [_] group/cljs-extensions))

(defn make-group
  "Return a layout RuleGroup configured with the given map.

  Supported keys:
    :split-map-after-symbol true — also report a split map that follows a plain symbol,
      `_` included, in a parameter list or binding. Off by default: cljfmt leaves
      `[_ {:keys [id] :as user}]`, split, all over a typical codebase, and most read the
      result as fine."
  ([] (make-group nil))
  ([config]
   (->LayoutGroup (boolean (when (map? config) (:split-map-after-symbol config))))))

(def group (make-group))
