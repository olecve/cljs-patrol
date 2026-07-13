(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.time
    Instant)))

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
  collapse to a single identity (rare in practice)."
  #{:img-alt-missing :invalid-tabindex :on-click-on-non-interactive
    :empty-interactive-element :missing-accessible-name})

(defn- relativize-path
  "Strip source-dir prefix from path to produce a portable relative path.
  Falls back to the original path if source-dir is nil or not a prefix."
  [source-dir path]
  (if source-dir
    (let [base (-> source-dir io/file .getAbsoluteFile .toPath)
          target (-> path io/file .getAbsoluteFile .toPath)]
      (if (.startsWith target base)
        (str (.relativize base target))
        path))
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

       (= :deprecated-effects rule)
       {:rule rule
        :effect (:effect issue)
        :file (rel (:file issue))
        :line (:row issue)}

       (= :pseudo-in-main-map rule)
       {:rule rule
        :ns (namespace (:kw issue))
        :var (name (:kw issue))
        :selector (:selector issue)}

       (= :consecutive-self-selectors rule)
       {:rule rule
        :ns (namespace (:kw issue))
        :var (name (:kw issue))
        :selectors (str/join "," (:selectors issue))}

       (contains? hiccup-site-rules rule)
       {:rule rule
        :tag (:kw issue)
        :file (rel (:file issue))
        :form (str/replace (str/trim (str (:form issue))) #"\s+" " ")}

       (= :dynamic-sites rule)
       {:rule rule
        :form (str/replace (str/trim (:form issue)) #"\s+" " ")
        :file (rel (:file issue))
        :line (:row issue)}

       :else
       (throw (ex-info (str "Unknown rule for identity extraction: " rule)
                       {:rule rule
                        :issue issue}))))))

(defn result->identities
  "Extract identity maps for all issues in a single group's analysis result map."
  ([result] (result->identities result nil))
  ([result source-dir]
   (into #{}
         (mapcat (fn [[rule-key items]]
                   (when (sequential? items)
                     (map #(issue->identity rule-key % source-dir) items))))
         result)))

(def baseline-version 2)

(def default-baseline-path ".cljs-patrol/baseline.edn")

(defn resolve-baseline-path
  "Resolve the baseline file path.
  Uses `configured-path` as-is when supplied; otherwise places the default
  path relative to the first source directory."
  [configured-path source-dirs]
  (if configured-path
    configured-path
    (let [root (first source-dirs)]
      (str (io/file root default-baseline-path)))))

(defn- sort-key
  "Vector of stringified identity fields used to sort baseline entries deterministically."
  [identity]
  (mapv #(str (get identity % ""))
        [:rule :ns :key :var :effect :tag :file :selector :selectors :form :line]))

(defn- sort-issues [issues]
  (vec (sort-by sort-key issues)))

(def ^:private tool-version
  (or (some-> (io/resource "cljs_patrol/VERSION") slurp str/trim not-empty)
      "dev"))

(defn write-baseline
  "Write a baseline file at `path` with the given set of identity maps."
  [path issues]
  (let [parent (.getParentFile (io/file path))
        sorted (sort-issues issues)]
    (when parent (.mkdirs parent))
    (with-open [w (io/writer path)]
      (.write w (str "{:version " baseline-version "\n"))
      (.write w (str " :generated-at \"" (Instant/now) "\"\n"))
      (.write w (str " :tool-version \"" tool-version "\"\n"))
      (.write w " :issues\n [")
      (doseq [[i issue] (map-indexed vector sorted)]
        (when (pos? i) (.write w "\n\n  "))
        (.write w "{")
        (doseq [[j [k v]] (map-indexed vector issue)]
          (when (pos? j) (.write w "\n   "))
          (.write w (str (pr-str k) " " (pr-str v))))
        (.write w "}"))
      (.write w "]}\n"))))

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
        (mapcat (fn [{:keys [source-dir group-results]}]
                  (mapcat #(result->identities % source-dir) group-results)))
        run-results))

(def default-config-path ".cljs-patrol/config.edn")

(defn read-config
  "Read `.cljs-patrol/config.edn` and return the full map.
  Returns {} if the file is missing, unreadable, or malformed."
  []
  (let [f (io/file default-config-path)]
    (if (.exists f)
      (try
        (edn/read-string (slurp f))
        (catch Exception _
          {}))
      {})))

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
