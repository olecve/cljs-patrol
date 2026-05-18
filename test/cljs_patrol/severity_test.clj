(ns cljs-patrol.severity-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.groups.typography :as typography]
   [cljs-patrol.severity :as severity]
   [clojure.test :refer [deftest is testing]]))

(def ^:private all-groups
  [re-frame/group spade/group reagent/group typography/group])

(def ^:private rule->tier
  (severity/collect-rule->tier all-groups))

(deftest collect-rule->tier-test
  (testing "merges tier maps across groups"
    (is (= :bugs (get rule->tier :duplicate-subs)))
    (is (= :bugs (get rule->tier :duplicate-events)))
    (is (= :deprecations (get rule->tier :deprecated-effects)))
    (is (= :deprecations (get rule->tier :defclass-as-sole-attr)))
    (is (= :deprecations (get rule->tier :defattrs-in-merge)))
    (is (= :deprecations (get rule->tier :mixed-token-groups)))
    (is (= :cleanup (get rule->tier :unused-subs)))
    (is (= :cleanup (get rule->tier :unused-events)))
    (is (= :cleanup (get rule->tier :unused-styles)))
    (is (= :cleanup (get rule->tier :phantom-subs)))
    (is (= :cleanup (get rule->tier :phantom-events))))

  (testing "info-only rules are absent from the map"
    (is (nil? (get rule->tier :dynamic-sites)))))

(deftest group-tiers-test
  (testing "each group exposes its own tier classification"
    (is (= :bugs (-> (group/tiers re-frame/group) :duplicate-subs)))
    (is (= :cleanup (-> (group/tiers spade/group) :unused-styles)))
    (is (= :deprecations (-> (group/tiers reagent/group) :defclass-as-sole-attr)))
    (is (= :deprecations (-> (group/tiers typography/group) :mixed-token-groups)))))

(deftest tier->rules-test
  (is (= #{:duplicate-subs :duplicate-events}
         (severity/tier->rules rule->tier :bugs)))
  (is (= #{:deprecated-effects :defclass-as-sole-attr
           :defattrs-in-merge :mixed-token-groups}
         (severity/tier->rules rule->tier :deprecations)))
  (is (= #{:unused-subs :unused-events :unused-styles
           :phantom-subs :phantom-events}
         (severity/tier->rules rule->tier :cleanup)))
  (is (= #{} (severity/tier->rules rule->tier :unknown))
      "unknown tier returns empty set"))

(deftest parse-fail-on-test
  (testing "tier names"
    (is (= {:ok #{:duplicate-subs :duplicate-events}}
           (severity/parse-fail-on "bugs" rule->tier)))
    (is (= {:ok (severity/tier->rules rule->tier :deprecations)}
           (severity/parse-fail-on "deprecations" rule->tier)))
    (is (= {:ok (severity/tier->rules rule->tier :cleanup)}
           (severity/parse-fail-on "cleanup" rule->tier))))

  (testing "individual rule IDs"
    (is (= {:ok #{:phantom-subs}}
           (severity/parse-fail-on "phantom-subs" rule->tier)))
    (is (= {:ok #{:phantom-subs :phantom-events}}
           (severity/parse-fail-on "phantom-subs,phantom-events" rule->tier))))

  (testing "all expands to every classified rule"
    (is (= {:ok (set (keys rule->tier))}
           (severity/parse-fail-on "all" rule->tier))))

  (testing "mixed tiers and rules"
    (is (= {:ok (into (severity/tier->rules rule->tier :bugs)
                      [:deprecated-effects])}
           (severity/parse-fail-on "bugs,deprecated-effects" rule->tier))))

  (testing "whitespace is trimmed"
    (is (= {:ok #{:phantom-subs :phantom-events}}
           (severity/parse-fail-on " phantom-subs , phantom-events " rule->tier))))

  (testing "empty and blank input"
    (is (= {:ok #{}} (severity/parse-fail-on nil rule->tier)))
    (is (= {:ok #{}} (severity/parse-fail-on "" rule->tier)))
    (is (= {:ok #{}} (severity/parse-fail-on "   " rule->tier))))

  (testing "unknown tokens return error"
    (let [{:keys [error]} (severity/parse-fail-on "bogus" rule->tier)]
      (is (some? error))
      (is (re-find #"bogus" error)))
    (let [{:keys [error]} (severity/parse-fail-on "bugs,bogus,phantom-subs" rule->tier)]
      (is (some? error)
          "error returned even when some tokens are valid")
      (is (re-find #"bogus" error)))
    (let [{:keys [error]} (severity/parse-fail-on "bad-one,bad-two" rule->tier)]
      (is (re-find #"bad-one" error))
      (is (re-find #"bad-two" error)
          "both unknown tokens listed"))))
