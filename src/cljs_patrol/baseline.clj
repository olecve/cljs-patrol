(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint])
  (:import
   (java.time
    Instant)))

(def ^:private keyword-keyed-rules
  "Rules where the issue is uniquely identified by its keyword."
  #{:unused-subs :unused-events :phantom-subs :phantom-events
    :duplicate-subs :duplicate-events})

(def ^:private var-keyed-rules
  "Rules where the issue is identified by namespace + var name (from a namespaced keyword)."
  #{:unused-styles :defattrs-in-merge :defclass-as-sole-attr :mixed-token-groups})

(defn- relativize-path
  "Make an absolute file path relative to the current working directory."
  [path]
  (let [f (io/file path)]
    (if (.isAbsolute f)
      (str (.relativize (.toPath (io/file (System/getProperty "user.dir")))
                        (.toPath f)))
      path)))

(defn issue->identity
  "Extract the stable identity of an issue for baseline comparison.
  Returns a map with :rule and the minimum fields needed to uniquely identify
  the issue without depending on line numbers where possible.
  File paths are relativized to the current working directory."
  [rule issue]
  (cond
    (contains? keyword-keyed-rules rule)
    {:rule rule :key (:kw issue)}

    (contains? var-keyed-rules rule)
    (let [kw (or (:kw issue) (:decl-kw issue))]
      {:rule rule :ns (namespace kw) :var (name kw)})

    (= :deprecated-effects rule)
    {:rule rule :effect (:effect issue)
     :file (relativize-path (:file issue)) :line (:row issue)}

    (= :dynamic-sites rule)
    {:rule rule :form (:form issue)
     :file (relativize-path (:file issue)) :line (:row issue)}

    :else
    (throw (ex-info (str "Unknown rule for identity extraction: " rule)
                    {:rule rule :issue issue}))))

(defn result->identities
  "Extract identity maps for all issues in a single group's analysis result map."
  [result]
  (into #{}
        (mapcat (fn [[rule-key items]]
                  (when (sequential? items)
                    (map #(issue->identity rule-key %) items))))
        result))

(def baseline-version 1)

(def default-baseline-path ".cljs-patrol/baseline.edn")

(defn- sort-key
  "Produce a vector sort key for deterministic ordering of identity maps."
  [identity]
  [(str (:rule identity))
   (str (:ns identity ""))
   (str (:key identity ""))
   (str (:var identity ""))
   (str (:file identity ""))
   (str (:line identity ""))])

(defn- sort-issues [issues]
  (vec (sort-by sort-key issues)))

(defn- tool-version []
  (or (System/getProperty "cljs-patrol.version") "dev"))

(defn write-baseline
  "Write a baseline file at `path` with the given set of identity maps."
  [path issues]
  (let [parent (.getParentFile (io/file path))
        data {:version baseline-version
              :generated-at (str (Instant/now))
              :tool-version (tool-version)
              :issues (sort-issues issues)}]
    (when parent (.mkdirs parent))
    (with-open [w (io/writer path)]
      (pprint/pprint data w))))

(defn read-baseline
  "Read and validate a baseline file at `path`.
  Returns {:ok issues} on success, {:error message} on failure."
  [path]
  (let [f (io/file path)]
    (if-not (.exists f)
      {:error (str "Baseline file not found: " path
                   "\nRun --baseline-write first to create one.")}
      (try
        (let [data (edn/read-string (slurp f))]
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
          {:error (str "Failed to parse baseline file: " path "\n" (.getMessage e))})))))

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
        (mapcat (fn [{:keys [group-results]}]
                  (mapcat result->identities group-results)))
        run-results))

(def default-config-path ".cljs-patrol/config.edn")

(defn read-config
  "Read baseline config from `.cljs-patrol/config.edn`.
  Returns a map of baseline settings, or empty map if file doesn't exist."
  []
  (let [f (io/file default-config-path)]
    (if (.exists f)
      (try
        (let [data (edn/read-string (slurp f))]
          (get data :baseline {}))
        (catch Exception _
          {}))
      {})))

(defn merge-config
  "Merge config file settings with CLI opts. CLI flags take precedence.
  Config keys: :path, :strict, :quiet."
  [config cli-opts]
  (merge (cond-> {}
           (:path config) (assoc :baseline-path (:path config))
           (:strict config) (assoc :strict-baseline true)
           (:quiet config) (assoc :quiet-baseline true))
         cli-opts))
