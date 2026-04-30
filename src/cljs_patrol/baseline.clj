(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import
   (java.time
    Instant)))

(def ^:private keyword-keyed-rules
  "Rules where the issue is uniquely identified by its keyword."
  #{:unused-subs :unused-events :phantom-subs :phantom-events})

(def ^:private keyword-plus-file-rules
  "Rules where duplicates of the same keyword exist in different files."
  #{:duplicate-subs :duplicate-events})

(def ^:private var-keyed-rules
  "Rules where the issue is identified by namespace + var name (from a namespaced keyword)."
  #{:unused-styles :defattrs-in-merge :defclass-as-sole-attr :mixed-token-groups})

(def ^:private site-rules
  "Per-call-site rules that require file + line for identity."
  #{:deprecated-effects :dynamic-sites})

(defn issue->identity
  "Extract the stable identity of an issue for baseline comparison.
  Returns a map with :rule and the minimum fields needed to uniquely identify
  the issue without depending on line numbers where possible."
  [rule issue]
  (cond
    (contains? keyword-keyed-rules rule)
    {:rule rule :key (:kw issue)}

    (contains? keyword-plus-file-rules rule)
    {:rule rule :key (:kw issue) :file (:file issue)}

    (contains? var-keyed-rules rule)
    (let [kw (or (:kw issue) (:decl-kw issue))]
      {:rule rule :ns (namespace kw) :var (name kw)})

    (= :deprecated-effects rule)
    {:rule rule :effect (:effect issue) :file (:file issue) :line (:row issue)}

    (= :dynamic-sites rule)
    {:rule rule :form (:form issue) :file (:file issue) :line (:row issue)}

    :else
    (throw (ex-info (str "Unknown rule for identity extraction: " rule)
                    {:rule rule :issue issue}))))

(defn result->identities
  "Given a group-id keyword and a single group's analysis result map,
  return a set of identity maps for all issues in the result."
  [group-id result]
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
      (let [data (edn/read-string (slurp f))]
        (cond
          (not (map? data))
          {:error (str "Malformed baseline file: " path " (expected a map)")}

          (not= baseline-version (:version data))
          {:error (str "Baseline version mismatch in " path ": found version "
                       (:version data) ", expected " baseline-version "."
                       "\nRe-run --baseline-write to regenerate.")}

          :else
          {:ok (set (:issues data))})))))

(defn diff-baseline
  "Compare found issues against a baseline.
  `baseline` and `found` are both sets of identity maps.
  Returns {:new #{...} :present #{...} :fixed #{...}}."
  [baseline found]
  {:new (into #{} (remove baseline) found)
   :present (into #{} (filter baseline) found)
   :fixed (into #{} (remove found) baseline)})
