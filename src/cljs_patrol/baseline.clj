(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.parser :as parser]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.io
    Writer)
   (java.time
    Instant)))

(def ^:private name-keyed-rules
  "Rules identified by the var they name plus the file it is in.
  A test var's name is what the finding is about, so it is the identity; the row moves
  whenever anything above it does."
  #{:deftest-leading-article})

(def ^:private form-keyed-rules
  "Rules identified by the form at fault plus the file it is in.

  For a rule whose findings share one `:kw` — every assertion-message finding names `is`,
  every conditional names the head it branches on — the keyword is a kind rather than an
  identity, so the source text of the form is what tells two findings in a file apart. It
  moves with the code it describes, where a row moves whenever anything above it does.

  Two identical forms in one file collapse to one identity, and for these rules that is
  worth a number: on a 483-file suite, 5 of 213 assertion-message findings collapse, a
  little over 2%. Each is the same assertion written twice — `(is (nil? (palette)) \"and
  the palette goes away\")` appearing in two tests of one file — which no key that
  survives reformatting can tell apart. The cost is that fixing one leaves the other
  suppressed; the alternative, keying on the row, would churn the whole baseline every
  time a line moves above a finding."
  #{:assertion-message-inline :conditional-assertion :private-var-deref})

(def ^:private keyword-keyed-rules
  "Rules where the issue is uniquely identified by its keyword."
  #{:unused-subs :unused-events :phantom-subs :phantom-events
    :duplicate-subs :duplicate-events
    :reg-sub-=>-1-arity
    :reg-event-fx-db-only :reg-event-fx-empty
    :reg-event-db-empty :reg-event-db-returning-effects})

(def ^:private var-keyed-rules
  "Rules where the issue is identified by namespace + var name (from a namespaced keyword)."
  #{:unused-styles :defattrs-in-merge :defclass-as-sole-attr :mixed-token-groups
    :docstring-summary :docstring-indentation :docstring-leading-trailing-whitespace})

(def ^:private hiccup-site-rules
  "Hiccup-vector rules whose baseline identity is (rule + tag + file + form).

  Line and column are recorded on the finding for the report but are not part
  of the identity — reformatting a file must not turn every baselined Hiccup
  finding into a new one. Two identical Hiccup vectors in the same file
  collapse to a single identity, which is rare for an element written out with its
  attributes. Do not carry that over to shorter forms: the same collapse runs at about
  2% for assertion messages (see [[form-keyed-rules]]).

  A rule reporting a container adds `:inner-form`, the snippet of the element found
  inside it, because the collapse above is rare only for element-level rules: a wrapper's
  own snippet runs past the truncation nearly every time, so two differing only past the
  cut would be one identity."
  #{:aria-live-contradicts-role
    :empty-interactive-element
    :repeated-accessible-name
    :img-alt-missing
    :invalid-tabindex
    :missing-accessible-name
    :on-click-on-non-interactive
    :aria-hidden-focusable
    :nested-interactive-element
    :label-not-associated
    :redundant-into-hiccup})

(def ^:private selector-keyed-rules
  "Spade rules identified by the declaration plus the one selector at fault.
  A finding in a nested block adds `:block`, the selector path above it; one in the base map
  carries no such path, so identities recorded before these rules read nested blocks still match."
  #{:pseudo-in-main-map :spade-ampersand-not-at-start})

(def ^:private selector-list-keyed-rules
  "Spade rules identified by the declaration plus the whole selector head at fault.
  `:block` distinguishes nesting levels, as for `selector-keyed-rules`."
  #{:consecutive-self-selectors :spade-keyword-combinator-selector})

(def ^:private style-block-rules
  "Rules reported once per style block, identified by the declaration plus the selector it sits under."
  #{:css-property-order-outside-in})

(defn- relativize-path
  "Strip source-dir prefix from path to produce a portable relative path.
  Falls back to the original path if source-dir is nil or not a prefix."
  [source-dir path]
  (if source-dir (fs/relativize source-dir path) path))

(defn issue->identity
  "Extract the stable identity of an issue for baseline comparison.

  Returns a map with :rule and the minimum fields needed to uniquely identify
  the issue without depending on line numbers where possible.
  When source-dir is provided, file paths are made relative to it."
  ([rule issue] (issue->identity rule issue nil))
  ([rule issue source-dir]
   (let [rel #(relativize-path source-dir %)]
     (cond
       (contains? keyword-keyed-rules rule)
       {:rule rule
        :key (:kw issue)}

       (contains? var-keyed-rules rule)
       (let [kw (or (:kw issue) (:decl-kw issue))]
         {:rule rule
          :ns (namespace kw)
          :var (name kw)})

       (= :deprecated-effects rule)
       {:rule rule
        :effect (:effect issue)
        :file (rel (:file issue))
        :line (:row issue)}

       (contains? selector-keyed-rules rule)
       (cond-> {:rule rule
                :ns (namespace (:kw issue))
                :var (name (:kw issue))
                :selector (:selector issue)}
         (seq (:block issue)) (assoc :block (:block issue)))

       (contains? selector-list-keyed-rules rule)
       (cond-> {:rule rule
                :ns (namespace (:kw issue))
                :var (name (:kw issue))
                :selectors (str/join "," (:selectors issue))}
         (seq (:block issue)) (assoc :block (:block issue)))

       (contains? style-block-rules rule)
       {:rule rule
        :ns (namespace (:kw issue))
        :var (name (:kw issue))
        :selector (:selector issue)}

       (contains? name-keyed-rules rule)
       {:rule rule
        :name (str (:kw issue))
        :file (rel (:file issue))}

       (contains? form-keyed-rules rule)
       {:rule rule
        :kind (:kw issue)
        :file (rel (:file issue))
        :form (parser/normalize-form (:form issue))}

       (contains? hiccup-site-rules rule)
       (let [normalize parser/normalize-form]
         (cond-> {:rule rule
                  :tag (:kw issue)
                  :file (rel (:file issue))
                  :form (normalize (:form issue))}
           (:inner-form issue) (assoc :inner-form (normalize (:inner-form issue)))))

       (= :dynamic-sites rule)
       {:rule rule
        :form (parser/normalize-form (:form issue))
        :file (rel (:file issue))
        :line (:row issue)}

       :else
       (throw (ex-info (str "Unknown rule for identity extraction: " rule)
                       {:rule rule
                        :issue issue}))))))

(defn result->identities
  ([result] (result->identities result nil))
  ([result source-dir]
   (into #{}
         (mapcat (fn [[rule-key items]]
                   (when (sequential? items)
                     (map #(issue->identity rule-key % source-dir) items))))
         result)))

(def baseline-version 4)

(def default-baseline-path ".cljs-patrol/baseline.edn")

(defn resolve-baseline-path
  "Return `configured-path` when one is set, else the default under the first source directory.
  Which directory that is matters: a run over several source roots keeps its baseline beside the
  first, so the same invocation always reads and writes the same file."
  [configured-path source-dirs]
  (or configured-path
      (fs/join-path (first source-dirs) default-baseline-path)))

(defn- sort-key
  "Vector of stringified identity fields used to sort baseline entries deterministically.
  Order chosen so entries cluster by rule, then by file, then by whatever
  field distinguishes findings within a file — keeping each file's a11y
  findings contiguous for readable diffs."
  [identity]
  (mapv #(str (get identity % ""))
        [:rule :file :ns :key :var :effect :tag :form :selector :selectors :block :line]))

(defn- sort-issues [issues]
  (vec (sort-by sort-key issues)))

(def ^:private tool-version
  (or (some-> (io/resource "cljs_patrol/VERSION") slurp str/trim not-empty)
      "dev"))

(def ^:private tier-order
  "Canonical order for `:tier->total`.
  `:bugs` first: it's the reviewer's load-bearing question."
  [:bugs :deprecations :cleanup])

(defn- tier-totals [rule-counts rule->tier]
  (reduce
   (fn [acc tier]
     (assoc acc tier
            (->> rule-counts
                 (filter (fn [[rule _]] (= tier (get rule->tier rule))))
                 (map val)
                 (reduce + 0))))
   (array-map)
   tier-order))

(defn- summarize [issues rule->tier]
  (let [rule-counts (into (sorted-map) (frequencies (map :rule issues)))]
    {:total (count issues)
     :tier->total (tier-totals rule-counts rule->tier)
     :rule->total rule-counts}))

(defn- write-summary [^Writer w {:keys [total tier->total rule->total]}]
  (.write w (str " :summary\n {:total " total "\n"))
  (.write w "  :tier->total {")
  (doseq [[i [tier n]] (map-indexed vector tier->total)]
    (when (pos? i) (.write w " "))
    (.write w (str (pr-str tier) " " n)))
  (.write w "}\n")
  (.write w "  :rule->total")
  (if (empty? rule->total)
    (.write w " {}}\n")
    (do
      (.write w "\n  {")
      (doseq [[i [rule n]] (map-indexed vector rule->total)]
        (when (pos? i) (.write w "\n   "))
        (.write w (str (pr-str rule) " " n)))
      (.write w "}}\n"))))

(defn- write-baseline-file [^String path sorted rule->tier]
  (with-open [w (io/writer path)]
    (.write w (str "{:version " baseline-version "\n"))
    (.write w (str " :generated-at \"" (Instant/now) "\"\n"))
    (.write w (str " :tool-version \"" tool-version "\"\n"))
    (write-summary w (summarize sorted rule->tier))
    (.write w " :issues\n [")
    (doseq [[i issue] (map-indexed vector sorted)]
      (when (pos? i) (.write w "\n\n  "))
      (.write w "{")
      (doseq [[j [k v]] (map-indexed vector issue)]
        (when (pos? j) (.write w "\n   "))
        (.write w (str (pr-str k) " " (pr-str v))))
      (.write w "}"))
    (.write w "]}\n")))

(defn write-baseline
  "Write a baseline file at `path` with the given set of identity maps.

  `rule->tier` is used to compute `:summary :tier->total`; when absent, all
  tier counts are zero (tier info isn't part of the identity itself).
  The new content goes to a sibling temp file that is moved into place only
  once it is complete, so a failure partway through leaves an existing
  baseline untouched rather than truncated."
  ([path issues] (write-baseline path issues {}))
  ([path issues rule->tier]
   (let [sorted (sort-issues issues)
         parent (fs/parent-dir path)
         tmp-path (str path "." (fs/nano-time) ".tmp")]
     (when parent (fs/mkdirs parent))
     (try
       (write-baseline-file tmp-path sorted rule->tier)
       (fs/move-replace! tmp-path path)
       nil
       (finally
         (fs/delete-file! tmp-path))))))

(defn read-baseline
  "Read and validate a baseline file at `path`.
  Returns {:ok issues} on success, {:error message} on failure."
  [path]
  (if-not (fs/file-exists? path)
    {:error (str "Baseline file not found: " path
                 "\nRun --baseline-write first to create one.")}
    (try
      (let [data (edn/read-string (slurp path))]
        (cond
          (not (map? data))
          {:error (str "Malformed baseline file: " path " (expected a map)")}

          (not= baseline-version (:version data))
          {:error (str "Baseline version mismatch in " path ": found version "
                       (:version data) ", expected " baseline-version "."
                       "\nRe-run --baseline-write to regenerate.")}

          :else
          {:ok (set (:issues data))}))
      (catch Exception e
        {:error (str "Failed to parse baseline file: " path "\n" (.getMessage e))}))))

(defn diff-baseline
  "Compare found issues against a baseline.

  `baseline` and `found` are both sets of identity maps.
  Returns {:new #{...} :present #{...} :fixed #{...}}."
  [baseline found]
  {:new (into #{} (remove baseline) found)
   :present (into #{} (filter baseline) found)
   :fixed (into #{} (remove found) baseline)})

(defn collect-identities
  "Collect all issue identities from run-results across all groups.

  `run-results` is a seq of {:source-dir ... :group-results [...]}."
  [run-results]
  (into #{}
        (mapcat (fn [{:keys [source-dir group-results]}]
                  (mapcat #(result->identities % source-dir) group-results)))
        run-results))

(def default-config-path ".cljs-patrol/config.edn")

(defn read-config
  "Read `.cljs-patrol/config.edn` and return the full map.

  Returns {} if the file is missing. On a malformed file, prints a
  warning to stderr and returns {} so the tool still runs against
  defaults — a broken config shouldn't crash CI, but silently
  ignoring one that never took effect is worse."
  []
  (if (fs/file-exists? default-config-path)
    (try
      (edn/read-string (slurp default-config-path))
      (catch Exception e
        (binding [*out* *err*]
          (println (str "WARN: could not parse " default-config-path ": "
                        (.getMessage e))))
        {}))
    {}))

(defn merge-config
  "Apply :baseline config-file settings to CLI opts, with CLI flags taking precedence.

  Always returns all baseline keys (:baseline-path, :strict-baseline, :quiet-baseline)
  so callers don't have to handle missing keys.
  Recognized :baseline keys: :path, :strict, :quiet."
  [config cli-opts]
  (let [baseline-config (or (:baseline config) {})]
    (merge {:baseline-path (:path baseline-config)
            :strict-baseline (boolean (:strict baseline-config))
            :quiet-baseline (boolean (:quiet baseline-config))}
           cli-opts)))
