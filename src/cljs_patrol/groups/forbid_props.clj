(ns cljs-patrol.groups.forbid-props
  "Rule group: props a codebase forbids on some elements or components, each with the reason.

  Modelled on ESLint's `react/forbid-dom-props` and `react/forbid-component-props`, as one
  rule, since Hiccup writes a native tag and a component the same way. Which props are
  forbidden is a codebase's own choice, so the group does nothing until `:forbid-props` is
  configured."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.heads :as heads]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(defn- literal [loc]
  (when (= :token (z/tag loc))
    (try (z/sexpr loc) (catch Exception _ ::unreadable))))

(defn- element
  "Return a recognized tag keyword, a resolved component symbol, or nil.

  Only keyword heads use the shared HTML and SVG tag reader. It reads up to the first
  class or id marker, including in tag chains. String heads and unknown tags stay unknown.
  A symbol must resolve through aliases or refers, or have a fully qualified namespace.
  A local that shadows a referred component stays unknown."
  [loc ns-info]
  (let [head (z/down loc)]
    (when (= :token (some-> head z/tag))
      (let [raw (parser/raw head)
            value (literal head)]
        (cond
          (keyword? value) (let [tag (hiccup/parse-tag raw)]
                             (when (hiccup/html-tag? tag) tag))
          (symbol? value) (when-not (and (nil? (namespace value)) (heads/locally-bound? loc (name value)))
                            (or (parser/resolve-full-symbol raw ns-info)
                                (when (some-> (namespace value) (str/includes? ".")) value))))))))

(defn- forbidden-for?
  "True when an entry forbids its prop on `element`."
  [{:keys [allowed-for disallowed-for]} element]
  (cond
    (= :native disallowed-for) (and (keyword? element) (not= :<> element))
    disallowed-for (contains? disallowed-for element)
    allowed-for (not (contains? allowed-for element))
    :else true))

(defn- dropped-literal? [loc]
  (and (= :token (z/tag loc)) (contains? #{false nil} (literal loc))))

(defn- written?
  "True when a prop reaches the element: Reagent drops one whose value is false or nil.

  A reader conditional whose every branch is such a literal is dropped on every platform."
  [value-loc]
  (not (if (= :reader-macro (z/tag value-loc))
         (let [branches (some-> value-loc z/down z/right parser/child-locs)]
           (and (seq branches) (every? dropped-literal? (take-nth 2 (rest branches)))))
         (dropped-literal? value-loc))))

(defn- finding [loc prop element message file]
  (let [[row col] (parser/position loc)]
    {:kw prop
     :type :forbidden-prop
     :form (parser/source-snippet loc)
     :file file
     :row row
     :col col
     :hint (or message (str prop " is forbidden on " element "."))}))

(defn- handle-vector [entries]
  (fn [loc ns-info file]
    (when-not (or (hiccup/inside-non-hiccup-form? loc) (hiccup/re-frame-vector? loc ns-info))
      (when-let [element (element loc ns-info)]
        (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
          (when (and (= :map kind) (seq attrs))
            (let [findings (for [{:keys [prop message]
                                  :as entry} entries
                                 :let [value-loc (get attrs prop)]
                                 :when (and value-loc (written? value-loc) (forbidden-for? entry element))]
                             (finding loc prop element message file))]
              (when (seq findings)
                (let [note (hiccup/mixed-schema-note loc)]
                  {:decls []
                   :dynamics []
                   :usages (mapv #(hiccup/add-note % note) findings)})))))))))

(defn- analyze* [{:keys [usages]}]
  {:forbidden-prop (filterv #(= :forbidden-prop (:type %)) usages)})

(defrecord ForbidPropsGroup [entries]
  group/RuleGroup
  (group-id [_] :forbid-props)
  (group-name [_] "Forbid props")
  (parse-handlers [_] (if (seq entries) {:handle-vector (handle-vector entries)} {}))
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (when (seq entries) [["Forbidden props:" (count (:forbidden-prop result))]]))
  (failed? [_ result] (boolean (seq (:forbidden-prop result))))
  (suggestions [_]
    {:forbidden-prop
     "A prop this codebase forbids on this element or component. The finding says why and what to use instead."})
  (docs-anchors [_]
    {:forbidden-prop "forbidden-prop"})
  (rule->tier [_]
    {:forbidden-prop :deprecations})
  (file-extensions [_] group/cljs-extensions))

(defn- invalid-config [message value]
  (throw (ex-info (str ":forbid-props " message ", got " (binding [*print-namespace-maps* false] (pr-str value)))
                  {:forbid-props value})))

(defn- element-name?
  "True for an HTML or SVG tag keyword, plain, or a fully qualified component symbol.

  Any other keyword names nothing the rule reads, a typo such as `:buton` or a custom element,
  so an entry naming it would never match."
  [value]
  (or (and (simple-keyword? value) (hiccup/html-tag? value))
      (qualified-symbol? value)))

(defn- read-elements [entry k]
  (when-some [elements (get entry k)]
    (if (and (= :disallowed-for k) (= :native elements))
      :native
      (do
        (when-not (and (coll? elements) (not (map? elements)) (every? element-name? elements))
          (invalid-config (str k " must be " (when (= :disallowed-for k) ":native or ")
                               "a collection of HTML tag keywords and fully qualified component symbols") entry))
        (set elements)))))

(defn- read-entry
  "Read one entry, a bare prop keyword or a map, refusing any other shape."
  [entry]
  (cond
    (keyword? entry) {:prop entry}

    (map? entry)
    (let [{:keys [prop message]} entry
          unknown (remove #{:prop :allowed-for :disallowed-for :message} (keys entry))]
      (when-not (keyword? prop)
        (invalid-config "entry must name its :prop as a keyword" entry))
      (when (seq unknown)
        (invalid-config (str "entry has unknown keys " (str/join ", " unknown)) entry))
      (when (and (contains? entry :allowed-for) (contains? entry :disallowed-for))
        (invalid-config "entry takes :allowed-for or :disallowed-for, not both" entry))
      (when-not (or (nil? message) (string? message))
        (invalid-config ":message must be a string" entry))
      (cond-> {:prop prop}
        message (assoc :message message)
        (contains? entry :allowed-for) (assoc :allowed-for (read-elements entry :allowed-for))
        (contains? entry :disallowed-for) (assoc :disallowed-for (read-elements entry :disallowed-for))))

    :else (invalid-config "entry must be a prop keyword or a map" entry)))

(defn make-group
  "Return a forbid-props RuleGroup configured with the given entries.

  Off until set. Each entry is a prop keyword, forbidden on every element, or a map:
    :prop :aria-disabled — the prop.
    :disallowed-for #{:button my.ui/button} — forbidden only on these tags and components.
    :disallowed-for :native — forbidden on native HTML and SVG tags only.
    :allowed-for #{:div} — forbidden everywhere but on these; one of the two at most.
    :message \"Use :disabled.\" — what a finding says instead of naming the prop."
  ([] (make-group nil))
  ([entries]
   (when-not (or (nil? entries) (sequential? entries))
     (invalid-config "must be a vector of entries" entries))
   (let [read (mapv read-entry entries)]
     (when-let [repeated (seq (for [[prop n] (frequencies (map :prop read)) :when (< 1 n)] prop))]
       (invalid-config (str "names " (str/join ", " repeated) " in more than one entry") entries))
     (->ForbidPropsGroup read))))

(def group (make-group))
