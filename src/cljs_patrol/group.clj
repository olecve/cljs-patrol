(ns cljs-patrol.group
  "Protocol defining the interface for cljs-patrol rule groups.")

(defprotocol RuleGroup
  "Interface for a static analysis rule group."
  (group-id [g] "Keyword identifier, e.g. :re-frame.")
  (group-name [g] "Human-readable name, e.g. \"Re-frame\".")
  (parse-handlers [g] "Map of parser handler fns: {:handle-list f :handle-vector f :handle-token f}.")
  (analyze [g parsed-data] "Compute violations from parsed data. Returns a result map.")
  (summary-lines [g result] "Return [[label count] ...].")
  (failed? [g result] "Return truthy if the result warrants a non-zero exit code.")
  (suggestions [g] "Map of issue-key -> fix suggestion string.")
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
