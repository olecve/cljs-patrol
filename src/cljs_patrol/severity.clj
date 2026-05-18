(ns cljs-patrol.severity
  "Severity tier helpers: aggregate tier classifications across rule groups
  and parse the --fail-on flag."
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

(defn parse-fail-on
  "Parse a comma-separated --fail-on string into a set of rule keys.
  Accepts tier names (bugs/deprecations/cleanup), individual rule keywords,
  or the meta value 'all'. Validates against the given rule->tier map.
  Returns {:ok rules} on success, {:error msg} on unknown tokens."
  [s rule->tier]
  (if (str/blank? s)
    {:ok #{}}
    (let [tokens (->> (str/split s #",")
                      (map str/trim)
                      (remove str/blank?))
          all-rules (set (keys rule->tier))
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
        {:ok rules}))))
