(ns cljs-patrol.group
  "Protocol defining the interface for cljs-patrol rule groups."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defprotocol RuleGroup
  "Interface for a static analysis rule group."
  (group-id [g] "Keyword identifier, e.g. :re-frame.")
  (group-name [g] "Human-readable name, e.g. \"Re-frame\".")
  (parse-handlers [g] "Map of parser handler fns: {:handle-list f :handle-vector f :handle-token f}.")
  (analyze [g parsed-data] "Compute violations from parsed data. Returns a result map.")
  (summary-lines [g result] "Return [[label count] ...].")
  (failed? [g result] "Return truthy if the result warrants a non-zero exit code.")
  (suggestions [g] "Map of issue-key -> fix suggestion string.")
  (docs-anchors [g] "Map of issue-key -> anchor of the rule's heading on its docs page, docs/rules/<group-id>.md.")
  (rule->tier [g] "Map of rule->tier (:bugs, :deprecations, :cleanup). Rules absent are info-only.")
  (file-extensions [g] "Set of file extensions (e.g. #{\".cljs\" \".cljc\"}) this group applies to."))

(def cljs-extensions
  "What a group reads when it is a ClojureScript rule group, which is nearly all of them."
  #{".cljs" ".cljc"})

(defn extensions-for
  "Extension set for a group that can also read `.clj` under the experimental mode.
  Computed once per group rather than per `file-extensions` call, which the parser makes
  for every group on every file."
  [clj?]
  (cond-> cljs-extensions clj? (conj ".clj")))

(def tool-version
  "The released version this build carries, or \"dev\" for a build from source."
  (or (some-> (io/resource "cljs_patrol/VERSION") slurp str/trim not-empty)
      "dev"))

(defn docs-ref
  "Name the git ref whose docs describe `version`: its release tag, or main for a dev build."
  [version]
  (if (= "dev" version) "main" (str "v" version)))

(defn docs-url
  "Build the URL of a rule's section in the docs of the version running, or nil for a rule without one."
  [g rule]
  (when-let [anchor (get (docs-anchors g) rule)]
    (str "https://github.com/olecve/cljs-patrol/blob/" (docs-ref tool-version)
         "/docs/rules/" (name (group-id g)) ".md#" anchor)))

(defn docs-urls
  "Map every rule of the group that has a docs section to its URL."
  [g]
  (into {} (keep (fn [rule] (some->> (docs-url g rule) (vector rule)))) (keys (docs-anchors g))))

(defn guides
  "Map every rule of the group to what a report prints above its findings: {:text suggestion :docs url}."
  [g]
  (let [urls (docs-urls g)]
    (into {} (map (fn [[rule text]] [rule {:text text
                                           :docs (get urls rule)}]))
          (suggestions g))))
