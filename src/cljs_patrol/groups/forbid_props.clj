(ns cljs-patrol.groups.forbid-props
  "Rule group: props a codebase forbids on some elements or components, each with the reason.

  Modelled on ESLint's `react/forbid-dom-props` and `react/forbid-component-props`, as one
  rule, since Hiccup writes a native tag and a component the same way. Which props are
  forbidden is a codebase's own choice, so the group does nothing until `:forbid-props` is
  configured."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [collapsed (str/replace (try (z/string loc) (catch Exception _ "")) #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

(defn- literal [loc]
  (when (= :token (z/tag loc))
    (try (z/sexpr loc) (catch Exception _ ::unreadable))))

(defn- element
  "Return what a Hiccup vector renders: an HTML tag keyword, a component's fully qualified symbol, or nil.

  A keyword counts only when HTML has the element, so a re-frame event vector or a Malli
  schema is never read as one. A symbol counts when the file's aliases or refers resolve it,
  or when it is written fully qualified. A local stays unknown."
  [loc ns-info]
  (let [head (z/down loc)]
    (when (= :token (some-> head z/tag))
      (let [raw (parser/raw head)
            value (literal head)]
        (cond
          (keyword? value) (let [tag (hiccup/parse-tag raw)]
                             (when (hiccup/html-tag? tag) tag))
          (symbol? value) (or (parser/resolve-full-symbol raw ns-info)
                              (when (some-> (namespace value) (str/includes? ".")) value)))))))

(defn- forbidden-for?
  "True when an entry forbids its prop on `element`."
  [{:keys [allowed-for disallowed-for]} element]
  (cond
    disallowed-for (contains? disallowed-for element)
    allowed-for (not (contains? allowed-for element))
    :else true))

(defn- written?
  "True when a prop reaches the element: Reagent drops one whose value is false or nil."
  [value-loc]
  (not (and (= :token (z/tag value-loc)) (contains? #{false nil} (literal value-loc)))))

(defn- finding [loc prop element message file]
  (let [[row col] (parser/position loc)]
    {:kw prop
     :type :forbidden-prop
     :form (source-snippet loc)
     :file file
     :row row
     :col col
     :hint (or message (str prop " is forbidden on " element "."))}))

(defn- handle-vector [entries]
  (fn [loc ns-info file]
    (when-not (hiccup/inside-non-hiccup-form? loc)
      (when-let [element (element loc ns-info)]
        (let [{:keys [kind attrs]} (hiccup/attrs-info loc)]
          (when (and (= :map kind) (seq attrs))
            (let [findings (for [{:keys [prop message]
                                  :as entry} entries
                                 :let [value-loc (get attrs prop)]
                                 :when (and value-loc (written? value-loc) (forbidden-for? entry element))]
                             (finding loc prop element message file))]
              (when (seq findings)
                {:decls []
                 :dynamics []
                 :usages (vec findings)}))))))))

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

(defn- element-name? [value]
  (or (keyword? value) (qualified-symbol? value)))

(defn- read-elements [entry k]
  (when-some [elements (get entry k)]
    (when-not (and (coll? elements) (not (map? elements)) (every? element-name? elements))
      (invalid-config (str k " must be a collection of tag keywords and fully qualified component symbols") entry))
    (set elements)))

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
    :allowed-for #{:div} — forbidden everywhere but on these; one of the two at most.
    :message \"Use :disabled.\" — what a finding says instead of naming the prop."
  ([] (make-group nil))
  ([entries]
   (when-not (or (nil? entries) (sequential? entries))
     (invalid-config "must be a vector of entries" entries))
   (->ForbidPropsGroup (mapv read-entry entries))))

(def group (make-group))
