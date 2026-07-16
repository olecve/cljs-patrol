(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [cljs-patrol.fs :as fs]
   [clojure.edn :as edn]
   #?@(:clj  [[clojure.java.io :as io]]
       :default [])
   [clojure.string :as str])
  #?(:clj (:import (java.time Instant))))

(defn- now-iso []
  #?(:clj (str (Instant/now))
     :cljs (.toISOString (js/Date.))))

(defn- error-message [e]
  #?(:clj (.getMessage ^Throwable e)
     :cljs (.-message e)))

(defn- read-embedded-version []
  #?(:clj (some-> (io/resource "cljs_patrol/VERSION") slurp str/trim not-empty)
     :cljs nil))

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
    (fs/join-path (first source-dirs) default-baseline-path)))

(defn- sort-key
  "Vector of stringified identity fields used to sort baseline entries deterministically.
  Order chosen so entries cluster by rule, then by file, then by whatever
  field distinguishes findings within a file — keeping each file's a11y
  findings contiguous for readable diffs."
  [identity]
  (mapv #(str (get identity % ""))
        [:rule :file :ns :key :var :effect :tag :form :selector :selectors :line]))

(defn- sort-issues [issues]
  (vec (sort-by sort-key issues)))

(def ^:private tool-version
  (or (read-embedded-version) "dev"))

(defn- render-issue [issue]
  (str "{"
       (->> issue
            (map (fn [[k v]] (str (pr-str k) " " (pr-str v))))
            (str/join "\n   "))
       "}"))

(defn- render-baseline [issues]
  (let [sorted (sort-issues issues)]
    (str "{:version " baseline-version "\n"
         " :generated-at \"" (now-iso) "\"\n"
         " :tool-version \"" tool-version "\"\n"
         " :issues\n ["
         (str/join "\n\n  " (map render-issue sorted))
         "]}\n")))

(defn write-baseline
  "Write a baseline file at `path` with the given set of identity maps."
  [path issues]
  (when-let [parent (fs/parent-dir path)] (fs/mkdirs parent))
  (fs/spit-file path (render-baseline issues)))

(defn read-baseline
  "Read and validate a baseline file at `path`.
  Returns {:ok issues} on success, {:error message} on failure."
  [path]
  (if-not (fs/file-exists? path)
    {:error (str "Baseline file not found: " path
                 "\nRun --baseline-write first to create one.")}
    (try
      (let [data (edn/read-string (fs/slurp-file path))]
        (cond
          (not (map? data))
          {:error (str "Malformed baseline file: " path " (expected a map)")}

          (not= baseline-version (:version data))
          {:error (str "Baseline version mismatch in " path ": found version "
                       (:version data) ", expected " baseline-version "."
                       "\nRe-run --baseline-write to regenerate.")}

          :else
          {:ok (set (:issues data))}))
      (catch #?(:clj Exception :cljs :default) e
        {:error (str "Failed to parse baseline file: " path "\n" (error-message e))}))))

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
  (if (fs/file-exists? default-config-path)
    (try (edn/read-string (fs/slurp-file default-config-path))
         (catch #?(:clj Exception :cljs :default) _ {}))
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
