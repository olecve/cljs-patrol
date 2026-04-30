(ns cljs-patrol.baseline
  "Baseline support for cljs-patrol: identity extraction, file I/O, and diff logic."
  (:require
   [clojure.string :as str]))

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
