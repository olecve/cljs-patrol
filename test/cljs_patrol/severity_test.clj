(ns cljs-patrol.severity-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.docstrings :as docstrings]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.groups.typography :as typography]
   [cljs-patrol.severity :as severity]
   [clojure.test :refer [deftest is testing]]))

(def ^:private all-groups
  [re-frame/group spade/group reagent/group typography/group docstrings/group])

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

(deftest annotate-tiers-test
  (testing "attaches :tier to each issue based on its rule key"
    (let [result {:duplicate-subs [{:kw :app/a
                                    :file "a.cljs"
                                    :row 1}]
                  :unused-subs [{:kw :app/b
                                 :file "b.cljs"
                                 :row 2}]
                  :phantom-subs [{:kw :app/c
                                  :file "c.cljs"
                                  :row 3}]
                  :deprecated-effects [{:effect ":dispatch-n"
                                        :file "d.cljs"
                                        :row 4}]
                  :dynamic-sites [{:form "(dispatch [x])"
                                   :file "e.cljs"
                                   :row 5}]}
          annotated (severity/annotate-tiers re-frame/group result)]
      (is (= :bugs (-> annotated :duplicate-subs first :tier)))
      (is (= :cleanup (-> annotated :unused-subs first :tier)))
      (is (= :cleanup (-> annotated :phantom-subs first :tier)))
      (is (= :deprecations (-> annotated :deprecated-effects first :tier)))
      (is (nil? (-> annotated :dynamic-sites first :tier))
          "info-only rules get :tier nil")))

  (testing "spade rules"
    (let [result {:unused-styles [{:kw :app.ui/a
                                   :file "a.cljs"
                                   :row 1}]
                  :defattrs-in-merge [{:kw :app.ui/b
                                       :file "b.cljs"
                                       :row 2}]}
          annotated (severity/annotate-tiers spade/group result)]
      (is (= :cleanup (-> annotated :unused-styles first :tier)))
      (is (= :deprecations (-> annotated :defattrs-in-merge first :tier)))))

  (testing "reagent rule"
    (let [result {:defclass-as-sole-attr [{:kw :app.ui/a
                                           :file "a.cljs"
                                           :row 1}]}
          annotated (severity/annotate-tiers reagent/group result)]
      (is (= :deprecations (-> annotated :defclass-as-sole-attr first :tier)))))

  (testing "typography rule"
    (let [result {:mixed-token-groups [{:decl-kw :app.styles/a
                                        :file "a.cljs"
                                        :row 1}]}
          annotated (severity/annotate-tiers typography/group result)]
      (is (= :deprecations (-> annotated :mixed-token-groups first :tier)))))

  (testing "empty rule vectors stay empty"
    (let [annotated (severity/annotate-tiers re-frame/group {:unused-subs []})]
      (is (= [] (:unused-subs annotated)))))

  (testing "non-sequential values pass through unchanged"
    (let [annotated (severity/annotate-tiers re-frame/group {:meta 42})]
      (is (= 42 (:meta annotated))))))

(deftest group-rule->tier-test
  (testing "each group exposes its own tier classification"
    (is (= :bugs (-> (group/rule->tier re-frame/group) :duplicate-subs)))
    (is (= :cleanup (-> (group/rule->tier spade/group) :unused-styles)))
    (is (= :deprecations (-> (group/rule->tier reagent/group) :defclass-as-sole-attr)))
    (is (= :deprecations (-> (group/rule->tier typography/group) :mixed-token-groups)))))

(deftest tier->rules-test
  (is (= #{:duplicate-subs :duplicate-events
           :reg-event-fx-empty :reg-event-db-empty}
         (severity/tier->rules rule->tier :bugs)))
  (is (= #{:deprecated-effects :defclass-as-sole-attr
           :defattrs-in-merge :mixed-token-groups}
         (severity/tier->rules rule->tier :deprecations)))
  (is (= #{:unused-subs :unused-events :unused-styles
           :phantom-subs :phantom-events
           :reg-sub-=>-1-arity :reg-event-fx-db-only
           :docstring-summary :docstring-indentation
           :docstring-leading-trailing-whitespace}
         (severity/tier->rules rule->tier :cleanup)))
  (is (= #{} (severity/tier->rules rule->tier :unknown))
      "unknown tier returns empty set"))

(deftest parse-fail-on-test
  (testing "tier names"
    (is (= {:ok #{:duplicate-subs :duplicate-events
                  :reg-event-fx-empty :reg-event-db-empty}}
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
          "both unknown tokens listed")))

  (testing "accepts vector input (from config file)"
    (is (= {:ok (severity/tier->rules rule->tier :bugs)}
           (severity/parse-fail-on [:bugs] rule->tier))
        "vector of keywords parsed as if comma-joined")
    (is (= {:ok #{:phantom-subs :phantom-events}}
           (severity/parse-fail-on [:phantom-subs :phantom-events] rule->tier))
        "vector of rule keywords parsed")))

(deftest list-rules-test
  (let [tiered (severity/list-rules all-groups)]
    (testing "groups every rule by tier"
      (is (= #{:duplicate-subs :duplicate-events
               :reg-event-fx-empty :reg-event-db-empty}
             (set (map :rule (:bugs tiered)))))
      (is (= #{:deprecated-effects :defclass-as-sole-attr
               :defattrs-in-merge :mixed-token-groups}
             (set (map :rule (:deprecations tiered)))))
      (is (= #{:unused-subs :unused-events :unused-styles
               :phantom-subs :phantom-events
               :reg-sub-=>-1-arity :reg-event-fx-db-only
               :docstring-summary :docstring-indentation
               :docstring-leading-trailing-whitespace}
             (set (map :rule (:cleanup tiered))))))

    (testing "info-only contains rules without a tier"
      (is (contains? (set (map :rule (:info-only tiered))) :dynamic-sites)))

    (testing "every entry has rule, group, tier, and suggestion"
      (doseq [tier [:bugs :deprecations :cleanup :info-only]
              entry (get tiered tier)]
        (is (every? entry [:rule :group :tier :suggestion])
            (str "entry under " tier " missing keys"))))))

(deftest list-rules-respects-group-filter-test
  (let [tiered (severity/list-rules [re-frame/group])
        all-groups (->> tiered vals (mapcat identity) (map :group) set)]
    (is (= #{:re-frame} all-groups)
        "only re-frame rules present when only re-frame group is enabled")))

(deftest format-rules-test
  (let [output (severity/format-rules (severity/list-rules all-groups))]
    (is (re-find #"bugs:" output))
    (is (re-find #"deprecations:" output))
    (is (re-find #"cleanup:" output))
    (is (re-find #"info-only" output))
    (is (re-find #":duplicate-subs" output))
    (is (re-find #":dynamic-sites" output))))

(deftest count-by-fail-on-test
  (let [results [{:duplicate-subs [{:kw :app/a}]
                  :unused-subs [{:kw :app/b} {:kw :app/c}]
                  :dynamic-sites []}]]
    (testing "splits issues by rule-key membership in fail-on-rules"
      (is (= {:blocking 1
              :warning 2}
             (severity/count-by-fail-on results #{:duplicate-subs}))))

    (testing "empty fail-on-rules counts everything as blocking"
      (is (= {:blocking 3
              :warning 0}
             (severity/count-by-fail-on results #{})))
      (is (= {:blocking 3
              :warning 0}
             (severity/count-by-fail-on results nil))))

    (testing "empty results returns zero counts"
      (is (= {:blocking 0
              :warning 0}
             (severity/count-by-fail-on [] #{:duplicate-subs}))))

    (testing "non-sequential values are skipped"
      (is (= {:blocking 0
              :warning 0}
             (severity/count-by-fail-on [{:meta 42}] #{:meta})))))

  (testing "sums counts across multiple result maps"
    (let [results [{:duplicate-subs [{:kw :a/x}]
                    :unused-subs [{:kw :a/y}]}
                   {:duplicate-subs [{:kw :b/x} {:kw :b/y}]
                    :unused-subs []}
                   {:unused-styles [{:kw :c/z}]}]]
      (is (= {:blocking 3
              :warning 2}
             (severity/count-by-fail-on results #{:duplicate-subs}))
          "3 duplicate-subs across 2 maps block; 1 unused-sub + 1 unused-style warn"))))
