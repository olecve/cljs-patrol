(ns cljs-patrol.integration.analysis-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/re-frame-spade-app/src/webapp")
(def ^:private all-groups [re-frame/group spade/group reagent/group])

(deftest full-analysis-test
  (let [{:keys [group-results]} (core/run fixture-dir all-groups)
        re-frame-result (nth group-results 0)
        spade-result (nth group-results 1)
        reagent-result (nth group-results 2)]

    (testing "detects unused re-frame subscription"
      (is (= 1 (count (:unused-subs re-frame-result))))
      (is (= #{:webapp.subs/unused-sub}
             (set (map :kw (:unused-subs re-frame-result))))))

    (testing "detects unused re-frame event"
      (is (= 1 (count (:unused-events re-frame-result))))
      (is (= #{:webapp.events/unused-event}
             (set (map :kw (:unused-events re-frame-result))))))

    (testing "detects phantom subscription"
      (is (= 1 (count (:phantom-subs re-frame-result))))
      (is (= #{:webapp.phantom/ghost-sub}
             (set (map :kw (:phantom-subs re-frame-result))))))

    (testing "does not flag used subscription as unused"
      (is (not (contains? (set (map :kw (:unused-subs re-frame-result)))
                          :webapp.subs/used-sub))))

    (testing "detects unused Spade styles"
      (is (= 2 (count (:unused-styles spade-result))))
      (is (= #{:webapp.styles/unused-style :webapp.styles/unused-attrs}
             (set (map :kw (:unused-styles spade-result))))))

    (testing "does not flag used style as unused"
      (is (not (contains? (set (map :kw (:unused-styles spade-result)))
                          :webapp.styles/container-style))))

    (testing "detects defattrs used in merge"
      (is (= 1 (count (:defattrs-in-merge spade-result))))
      (is (= #{:webapp.styles/merged-attrs}
             (set (map :kw (:defattrs-in-merge spade-result))))))

    (testing "detects pseudo-selectors misplaced inside the main style map"
      (let [findings (:pseudo-in-main-map spade-result)]
        (is (= 4 (count findings)))
        (is (= #{[:webapp.pseudo-styles/menu-item-style ":&:hover"]
                 [:webapp.pseudo-styles/card-section-attrs ":&:first-child"]
                 [:webapp.pseudo-styles/card-section-attrs ":&:last-child"]
                 [:webapp.pseudo-styles/tab-style ":&:focus-visible>svg"]}
               (set (map (juxt :kw :selector) findings))))))

    (testing "does not flag pseudo-selectors placed correctly in their own vector"
      (is (not (contains? (set (map :kw (:pseudo-in-main-map spade-result)))
                          :webapp.pseudo-styles/icon-button-style))))

    (testing "detects consecutive self-selectors in a sibling vector"
      (let [findings (:consecutive-self-selectors spade-result)]
        (is (= 2 (count findings)))
        (is (= #{[:webapp.pseudo-styles/badge-marker-attrs [":&:before" ":&:after"]]
                 [:webapp.pseudo-styles/callout-style [":&:hover" ":&:focus" ":&:focus-visible"]]}
               (set (map (juxt :kw :selectors) findings))))))

    (testing "does not flag a self-selector chained with a descendant class"
      (is (not (contains? (set (map :kw (:consecutive-self-selectors spade-result)))
                          :webapp.pseudo-styles/panel-style))))

    (testing "detects defclass used as sole attr"
      (is (= 3 (count (:defclass-as-sole-attr reagent-result))))
      (is (= #{:webapp.styles/sole-attr-style
               :webapp.styles/vector-sole-attr-style
               :webapp.local-styles/local-panel-style}
             (set (map :kw (:defclass-as-sole-attr reagent-result))))))

    (testing "does not flag defclass in multi-element :class vector"
      (is (not (contains? (set (map :kw (:defclass-as-sole-attr reagent-result)))
                          :webapp.styles/vector-multi-class-style))))

    (testing "detects redundant `into` around hiccup vectors"
      (let [findings (:redundant-into-hiccup reagent-result)]
        (is (= 3 (count findings)))
        (is (= #{:ul (symbol "card-body") (symbol "pseudo-styles/panel-style")}
               (set (map :kw findings))))))

    (testing "does not flag plain-Clojure into forms"
      (let [forms (set (map :form (:redundant-into-hiccup reagent-result)))]
        (is (not-any? #(re-find #"\(into \[\]" %) forms)
            "empty-vec into not flagged")
        (is (not-any? #(re-find #"\(into \[1 2 3\]" %) forms)
            "literal-head into not flagged")
        (is (not-any? #(re-find #"\(into \[:span\]\)" %) forms)
            "arity-1 into not flagged")))

    (testing "detects duplicate subscription registration"
      (is (= 2 (count (:duplicate-subs re-frame-result))))
      (is (= #{:webapp.subs/used-sub} (set (map :kw (:duplicate-subs re-frame-result))))))

    (testing "detects deprecated :dispatch-n effect"
      (is (= 1 (count (:deprecated-effects re-frame-result)))))

    (testing "deprecated effect has correct metadata"
      (let [dep (first (:deprecated-effects re-frame-result))]
        (is (= :deprecated (:type dep)))
        (is (= ":dispatch-n" (:effect dep)))))))

(deftest reagent-only-test
  (testing "reagent group works when run without spade group"
    (let [{:keys [group-results]} (core/run fixture-dir [reagent/group])
          reagent-result (nth group-results 0)]
      (is (= 3 (count (:defclass-as-sole-attr reagent-result))))
      (is (= #{:webapp.styles/sole-attr-style
               :webapp.styles/vector-sole-attr-style
               :webapp.local-styles/local-panel-style}
             (set (map :kw (:defclass-as-sole-attr reagent-result))))))))

(deftest issues-carry-tier-test
  (let [{:keys [group-results]} (core/run fixture-dir all-groups)
        re-frame-result (nth group-results 0)
        spade-result (nth group-results 1)
        reagent-result (nth group-results 2)]
    (testing "re-frame issues get correct tier"
      (is (every? #(= :bugs (:tier %)) (:duplicate-subs re-frame-result)))
      (is (every? #(= :cleanup (:tier %)) (:unused-subs re-frame-result)))
      (is (every? #(= :cleanup (:tier %)) (:phantom-subs re-frame-result)))
      (is (every? #(= :deprecations (:tier %)) (:deprecated-effects re-frame-result))))

    (testing "info-only rules get :tier nil"
      (is (every? #(nil? (:tier %)) (:dynamic-sites re-frame-result))))

    (testing "spade and reagent issues get correct tier"
      (is (every? #(= :cleanup (:tier %)) (:unused-styles spade-result)))
      (is (every? #(= :deprecations (:tier %)) (:defattrs-in-merge spade-result)))
      (is (every? #(= :bugs (:tier %)) (:pseudo-in-main-map spade-result)))
      (is (every? #(= :bugs (:tier %)) (:consecutive-self-selectors spade-result)))
      (is (every? #(= :deprecations (:tier %)) (:defclass-as-sole-attr reagent-result)))
      (is (every? #(= :cleanup (:tier %)) (:redundant-into-hiccup reagent-result))))))
