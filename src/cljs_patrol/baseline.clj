(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.edn :as edn]
   [clojure.string :as str])
  (:import
   (java.io
    IOException
    Writer)
   (java.nio.file
    Path)
   (java.time
    Instant)))

(def ^:private name-keyed-rules
  "Rules identified by the var they name plus the file it is in.
  A test var's name is what the finding is about, so it is the identity; the row moves
  whenever anything above it does."
  #{:deftest-leading-article :lookup-helper-name})

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
  #{:assertion-message-inline :conditional-assertion :var-deref-in-test :js-hint-in-test
    :form-after-split-map :split-map-after-sibling :nested-call-chain
    :cond-else-nil :negated-test :into-empty :plus-minus-one :negated-equality
    :get-by-presence-check :literal-constant-assertion :unconditional-cond-threading
    :test-id-query-on-role :inline-component :commented-out-code :restating-comment :vec-of-map-filter
    :forbidden-prop})

(def ^:private keyword-keyed-rules
  "Rules where the issue is uniquely identified by its keyword."
  #{:unused-subs :unused-events :phantom-subs :phantom-events
    :duplicate-subs :duplicate-events
    :reg-sub-=>-1-arity
    :reg-event-fx-db-only :reg-event-fx-empty
    :reg-event-db-empty :reg-event-db-returning-effects
    :reg-fx-public-fn})

(def ^:private var-keyed-rules
  "Rules where the issue is identified by namespace + var name (from a namespaced keyword)."
  #{:unused-styles :defattrs-as-data :defclass-as-sole-attr :mixed-token-groups
    :docstring-summary :docstring-indentation :docstring-leading-trailing-whitespace
    :docstring-repeats-name})

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
  cut would be one identity. `button-missing-type` adds its label's snippet the same way,
  since a button's props often run past the cut too."
  #{:aria-live-contradicts-role
    :empty-interactive-element
    :repeated-accessible-name
    :img-alt-missing
    :invalid-tabindex
    :invalid-role
    :role-value-spelling
    :button-missing-type
    :missing-accessible-name
    :on-click-on-non-interactive
    :aria-hidden-focusable
    :nested-interactive-element
    :non-focusable-tooltip-trigger
    :label-not-associated
    :aria-label-beside-visible-label
    :redundant-into-hiccup
    :empty-fragment-props
    :redundant-fragment
    :discarded-hiccup
    :remounting-branch
    :controlled-input-dispatch})

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
  #{:css-property-order-outside-in
    :focus-ring-missing-outline :focus-ring-raw-shadow})

(defn- relativize-path
  "Give `path` relative to the `Path` `source-dir`, so a key does not hold the folder the run started in.

  Give `path` unchanged when `source-dir` is nil or is not above it, and the empty string when `path` is `source-dir`,
  which is so when the run reads one file. A baseline written before keeps its keys only with these results.
  A relative `source-dir` or `path` is read from the working directory of the file system of `source-dir`."
  [^Path source-dir ^String path]
  (if source-dir
    (let [base (fs/absolute source-dir)
          target (fs/absolute (fs/path (fs/file-system-of base) path))]
      (cond
        (= base target) ""
        (.startsWith target base) (str (fs/relative-path base target))
        :else path))
    path))

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

       (= :color-literal rule)
       {:rule rule
        :file (rel (:file issue))
        :name (:top-level issue)
        :literal (:literal issue)}

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
       (cond-> {:rule rule
                :kind (:kw issue)
                :file (rel (:file issue))
                :form (parser/normalize-form (:form issue))}
         ;; A finding whose form is short — a var name and nothing else — needs the test
         ;; it sits in to tell two of them in a file apart. The test name moves with the
         ;; code, where a row does not.
         (:test issue) (assoc :test (str (:test issue))))

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
  "Give the baseline file as {:path its `Path`, :text the text to print}: the configured one when set, else the default.

  The default is under the first source directory. Which directory that is matters: a run over several source roots
  keeps its baseline beside the first, so the same invocation always reads and writes the same file. The `Path`
  values of `source-dirs` give the file system. A configured path prints as typed, and its `:path` is nil when it
  names no file, such as the empty string."
  [^String configured-path source-dirs]
  (let [^Path first-dir (first source-dirs)]
    (if configured-path
      {:path (fs/parse-path (fs/file-system-of first-dir) configured-path)
       :text configured-path}
      (let [path (fs/resolve-path first-dir default-baseline-path)]
        {:path path
         :text (str path)}))))

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
  (let [rule-counts (->> issues (map :rule) frequencies (into (sorted-map)))]
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

(defn- write-baseline-file [^Path path sorted rule->tier]
  (with-open [w (fs/text-writer path)]
    (.write w (str "{:version " baseline-version "\n"))
    (.write w (str " :generated-at \"" (Instant/now) "\"\n"))
    (.write w (str " :tool-version \"" group/tool-version "\"\n"))
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

(defn- delete-temp-file!
  "Delete the temporary file `tmp-path` when it is there.

  A failed delete prints a warning on stderr and does not throw, because it runs in a `finally`, and a throw there
  hides the error that stopped the write."
  [^Path tmp-path]
  (try
    (fs/delete-if-exists! tmp-path)
    (catch IOException e
      (binding [*out* *err*]
        (println (str "WARN: could not delete " tmp-path ": " (fs/failure-reason e)))))))

(defn write-baseline
  "Write a baseline file at the `Path` `path` with the given set of identity maps.

  `rule->tier` is used to compute `:summary :tier->total`; when absent, all
  tier counts are zero (tier info isn't part of the identity itself).
  The new content goes to a sibling temp file that is moved into place only
  once it is complete, so a failure partway through leaves an existing
  baseline untouched rather than truncated."
  ([path issues] (write-baseline path issues {}))
  ([^Path path issues rule->tier]
   (let [sorted (sort-issues issues)
         parent (fs/parent path)
         tmp-path (fs/path (fs/file-system-of path) (str path "." (fs/nano-time) ".tmp"))]
     (when parent (fs/create-dirs! parent))
     (try
       (write-baseline-file tmp-path sorted rule->tier)
       (fs/move-over! tmp-path path)
       nil
       (finally
         (delete-temp-file! tmp-path))))))

(defn not-found-message [text]
  (str "Baseline file not found: " text
       "\nRun --baseline-write first to create one."))

(defn read-baseline
  "Read and validate the baseline file `baseline`, a {:path :text} map as [[resolve-baseline-path]] gives.

  Returns {:ok issues} on success, {:error message} on failure. The messages print `:text`."
  [{:keys [path text]}]
  (if-not (some-> path fs/exists?)
    {:error (not-found-message text)}
    (try
      (let [data (edn/read-string (fs/read-text path))]
        (cond
          (not (map? data))
          {:error (str "Malformed baseline file: " text " (expected a map)"
                       "\nRe-run --baseline-write --allow-new to regenerate.")}

          (not= baseline-version (:version data))
          {:error (str "Baseline version mismatch in " text ": found version "
                       (:version data) ", expected " baseline-version "."
                       "\nRe-run --baseline-write --allow-new to regenerate.")}

          :else
          {:ok (set (:issues data))}))
      (catch IOException e
        {:error (str "Cannot read baseline file " text ": " (fs/failure-reason e)
                     "\nGive the tool read access to the file, then run it again.")})
      (catch Exception e
        {:error (str "Failed to parse baseline file: " text "\n" (.getMessage e)
                     "\nRe-run --baseline-write --allow-new to regenerate.")}))))

(defn diff-baseline
  "Compare found issues against a baseline.

  `baseline` and `found` are both sets of identity maps.
  Returns {:new #{...} :present #{...} :fixed #{...}}."
  [baseline found]
  {:new (into #{} (remove baseline) found)
   :present (into #{} (filter baseline) found)
   :fixed (into #{} (remove found) baseline)})

(defn find-added-identities
  "Compare the identities a write would record against the baseline file `baseline`, a {:path :text} map.
  Returns {:added identities} for those not in it, empty when no baseline exists yet,
  or {:error message} when the existing file cannot be read and so cannot be compared."
  [baseline found]
  (if-not (fs/exists? (:path baseline))
    {:added #{}}
    (let [{:keys [ok error]} (read-baseline baseline)]
      (if error
        {:error error}
        {:added (:new (diff-baseline ok found))}))))

(defn collect-identities
  "Collect all issue identities from run-results across all groups.

  `run-results` is a seq of {:source-dir ... :group-results [...]}."
  [run-results]
  (into #{}
        (mapcat (fn [{:keys [source-dir group-results]}]
                  (let [base (fs/absolute source-dir)]
                    (mapcat #(result->identities % base) group-results))))
        run-results))

(def default-config-path ".cljs-patrol/config.edn")

(defn read-config
  "Read the config file at the `Path` `config-path` and return the full map.

  Returns {} if the file is missing. On a malformed file, prints a
  warning to stderr and returns {} so the tool still runs against
  defaults — a broken config shouldn't crash CI, but silently
  ignoring one that never took effect is worse."
  [config-path]
  (if (fs/exists? config-path)
    (try
      (edn/read-string (fs/read-text config-path))
      (catch Exception e
        (binding [*out* *err*]
          (println (str "WARN: could not parse " config-path ": "
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
