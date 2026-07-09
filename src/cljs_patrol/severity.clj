(ns cljs-patrol.severity
  "Severity tier helpers.
  Aggregates tier classifications across rule groups and parses the --fail-on flag."
  (:require
   [cljs-patrol.group :as group]
   [clojure.string :as str]))

(def tiers #{:bugs :deprecations :cleanup})

(defn collect-rule->tier
  "Merge the rule->tier maps from each group into a single map."
  [groups]
  (into {} (mapcat group/rule->tier groups)))

(defn annotate-tiers
  "Attach :tier to each issue in a group result map, based on the group's rule->tier.
  Issues for info-only rules (absent from the map) get :tier nil."
  [group result]
  (let [tier-map (group/rule->tier group)]
    (->> result
         (map (fn [[rule-key v]]
                [rule-key (if (sequential? v)
                            (mapv #(assoc % :tier (get tier-map rule-key)) v)
                            v)]))
         (into {}))))

(defn tier->rules
  "Return the set of rules in the given rule->tier map that belong to `tier`."
  [rule->tier tier]
  (->> rule->tier
       (filter (fn [[_ t]] (= t tier)))
       (map key)
       set))

(defn- ->tokens
  "Normalize the --fail-on input to a seq of string tokens.
  Accepts a comma-separated string, a sequential of keywords/strings, or nil."
  [input]
  (cond
    (nil? input) []
    (sequential? input) (->> input (map name) (remove str/blank?))
    (string? input) (->> (str/split input #",")
                         (map str/trim)
                         (remove str/blank?))
    :else
    (throw (ex-info "Unsupported --fail-on input"
                    {:input input}))))

(defn parse-fail-on
  "Parse --fail-on input into a set of rule keys.
  Accepts tier names (bugs/deprecations/cleanup), individual rule keywords,
  or the meta value 'all'. Input may be a comma-separated string or a vector
  of keywords/strings. Validates against the given rule->tier map.
  Returns {:ok rules} on success, {:error msg} on unknown tokens."
  [input rule->tier]
  (let [tokens (->tokens input)]
    (if (empty? tokens)
      {:ok #{}}
      (let [all-rules (set (keys rule->tier))
            {:keys [rules unknown]}
            (reduce
             (fn [acc token]
               (cond
                 (= token "all")
                 (update acc :rules into all-rules)

                 (contains? tiers (keyword token))
                 (update acc :rules into (tier->rules rule->tier (keyword token)))

                 (contains? all-rules (keyword token))
                 (update acc :rules conj (keyword token))

                 :else
                 (update acc :unknown conj token)))
             {:rules #{}
              :unknown []}
             tokens)]
        (if (seq unknown)
          {:error (str "Unknown --fail-on tokens: " (str/join ", " unknown))}
          {:ok rules})))))

(defn count-by-fail-on
  "Return {:blocking N :warning M} for issues across the given `results`.
  Each entry in `results` is a group result map (rule-key -> items vector).
  Issues whose rule is in `fail-on-rules` count as blocking; the rest as warning.
  An empty `fail-on-rules` counts every issue as blocking (matches the
  default-fail-on-everything behavior)."
  [results fail-on-rules]
  (let [has-fail-on? (seq fail-on-rules)]
    (reduce
     (fn [acc result]
       (reduce-kv
        (fn [acc rule-key items]
          (if (and (sequential? items) (seq items))
            (if (or (not has-fail-on?) (contains? fail-on-rules rule-key))
              (update acc :blocking + (count items))
              (update acc :warning + (count items)))
            acc))
        acc
        result))
     {:blocking 0
      :warning 0}
     results)))

(def ^:private tier-order
  "Display order for tiers in list-rules output."
  [:bugs :deprecations :cleanup :info-only])

(defn list-rules
  "Return a map of tier -> sorted seq of {:rule :group :suggestion} entries.
  Rules absent from a group's rule->tier go under :info-only."
  [groups]
  (let [entries (for [g groups
                      :let [gid (group/group-id g)
                            tier-map (group/rule->tier g)
                            suggs (group/suggestions g)]
                      [rule suggestion] suggs]
                  {:rule rule
                   :group gid
                   :tier (get tier-map rule :info-only)
                   :suggestion suggestion})]
    (->> entries
         (group-by :tier)
         (map (fn [[tier es]]
                [tier (sort-by (juxt :group :rule) es)]))
         (into {}))))

(defn- truncate [s n]
  (if (<= (count s) n) s (str (subs s 0 n) "...")))

(defn format-rules
  "Render the result of `list-rules` as a human-readable string."
  [tiered]
  (str/join
   "\n"
   (for [tier tier-order
         :let [entries (get tiered tier)]
         :when (seq entries)
         line (concat [(str "\n" (if (= tier :info-only)
                                   "info-only (do not block CI):"
                                   (str (name tier) ":")))]
                      (for [{:keys [rule group suggestion]} entries]
                        (format "  %-25s (%s)  %s"
                                (str rule)
                                (name group)
                                (truncate suggestion 80))))]
     line)))
