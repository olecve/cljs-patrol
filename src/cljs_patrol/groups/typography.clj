(ns cljs-patrol.groups.typography
  "Rule group: detects mixed Figma typography token groups in Spade style declarations.

  NOTE: This is not a general-purpose rule. It assumes a Figma-based design system
  where composite typography tokens follow the naming convention
  {group}-{variant}-{font|letter-spacing|text-case} (e.g. action-small-font),
  referenced via a 'system-theme' alias. Projects with a different token system
  should not enable this group."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private style-decl-fns #{"defclass" "defattrs"})
(def ^:private system-theme-prefix "system-theme/")

(def ^:private typography-suffixes ["-font" "-letter-spacing" "-text-case"])

(defn- typography-token? [token-name]
  (some #(str/ends-with? token-name %) typography-suffixes))

(defn- token-group-prefix
  "Strip the typography suffix to get the group prefix.
  'action-small-font' -> 'action-small'
  'body-short-small-letter-spacing' -> 'body-short-small'"
  [token-name]
  (some (fn [suffix]
          (when (str/ends-with? token-name suffix)
            (subs token-name 0 (- (count token-name) (count suffix)))))
        typography-suffixes))

(defn- collect-children
  "Apply f to each immediate child of loc, returning a flat vector of results."
  [loc f]
  (when-let [child (z/down loc)]
    (loop [c child results []]
      (let [new-results (into results (f c))]
        (if-let [next (z/right c)]
          (recur next new-results)
          new-results)))))

(defn- collect-style-token-refs
  "Recursively collect all system-theme/* symbol references within loc's subtree."
  [loc file decl-kw]
  (when loc
    (if (= :token (z/tag loc))
      (let [raw (parser/raw loc)]
        (when (str/starts-with? raw system-theme-prefix)
          [{:type :style-token-ref
            :decl-kw decl-kw
            :token-name (subs raw (count system-theme-prefix))
            :file file
            :row (parser/position-row loc)}]))
      (collect-children loc #(collect-style-token-refs % file decl-kw)))))

(defn- handle-list [loc ns-name _aliases file]
  (let [op-token (z/down loc)
        operator (parser/sym-name op-token)]
    (when (contains? style-decl-fns operator)
      (let [name-loc (z/right op-token)
            style-name (parser/sym-name name-loc)
            decl-kw (when style-name (keyword ns-name style-name))
            tokens (collect-style-token-refs loc file decl-kw)]
        (when (seq tokens)
          {:decls [] :usages tokens :dynamics []})))))

(defn- analyze* [{:keys [usages]}]
  (let [token-refs (filter #(= :style-token-ref (:type %)) usages)
        by-decl (group-by :decl-kw token-refs)
        mixed-groups
        (vec (for [[decl-kw refs] by-decl
                   :let [typo-refs (filter #(typography-token? (:token-name %)) refs)
                         prefixes (set (keep #(token-group-prefix (:token-name %)) typo-refs))]
                   :when (> (count prefixes) 1)]
               {:decl-kw decl-kw
                :prefixes prefixes
                :file (:file (first refs))
                :row (:row (first refs))}))]
    {:mixed-token-groups mixed-groups}))

(defn- report* [{:keys [mixed-token-groups]}]
  (when (seq mixed-token-groups)
    (println "\n--- MIXED TYPOGRAPHY TOKEN GROUPS ---")
    (doseq [{:keys [decl-kw prefixes file row]} mixed-token-groups]
      (println (format "  %s  [%s]" decl-kw (str/join ", " prefixes)))
      (println (str "    at " file ":" row)))))

(defn- summary-lines* [{:keys [mixed-token-groups]}]
  [["Mixed typography token groups:" (count mixed-token-groups)]])

(defn- failed?* [{:keys [mixed-token-groups]}]
  (seq mixed-token-groups))

(defrecord TypographyGroup []
  group/RuleGroup
  (group-id [_] :typography)
  (group-name [_] "Typography")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (report [_ result] (report* result))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:mixed-token-groups
     (str "A style declaration uses typography tokens from more than one group. "
          "Each declaration must use tokens from a single group (same prefix). "
          "Example: action-small-font + action-small-letter-spacing + action-small-text-case. "
          "Available groups: caption-{small,medium,large,xlarge}, "
          "body-short-{small,medium,large}, body-long-{small,medium,large}, "
          "subhead-{small,medium,large}, heading-reg-{small,medium,large,xlarge}, "
          "heading-bold-{small,medium,large,xlarge}, action-{small,medium,large}, "
          "code-compact-{small,medium,large}, code-reg-{small,medium,large}.")})
  (html-sections [_]
    [{:title "Mixed Typography Token Groups"
      :description "Style declarations mixing tokens from different groups."
      :data-fn :mixed-token-groups
      :columns [:keyword :file :line]}]))

(def group (->TypographyGroup))
