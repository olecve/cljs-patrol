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
        rf-result (nth group-results 0)
        spade-result (nth group-results 1)
        reagent-result (nth group-results 2)]

    (testing "detects unused re-frame subscription"
      (is (= 1 (count (:unused-subs rf-result))))
      (is (= #{:webapp.subs/unused-sub}
             (set (map :kw (:unused-subs rf-result))))))

    (testing "detects unused re-frame event"
      (is (= 1 (count (:unused-events rf-result))))
      (is (= #{:webapp.events/unused-event}
             (set (map :kw (:unused-events rf-result))))))

    (testing "detects phantom subscription"
      (is (= 1 (count (:phantom-subs rf-result))))
      (is (= #{:webapp.phantom/ghost-sub}
             (set (map :kw (:phantom-subs rf-result))))))

    (testing "does not flag used subscription as unused"
      (is (not (contains? (set (map :kw (:unused-subs rf-result)))
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

    (testing "detects defclass used as sole attr"
      (is (= 3 (count (:defclass-as-sole-attr reagent-result))))
      (is (= #{:webapp.styles/sole-attr-style
               :webapp.styles/vector-sole-attr-style
               :webapp.local-styles/local-panel-style}
             (set (map :kw (:defclass-as-sole-attr reagent-result))))))

    (testing "does not flag defclass in multi-element :class vector"
      (is (not (contains? (set (map :kw (:defclass-as-sole-attr reagent-result)))
                          :webapp.styles/vector-multi-class-style))))

    (testing "detects duplicate subscription registration"
      (is (= 2 (count (:duplicate-subs rf-result))))
      (is (= #{:webapp.subs/used-sub} (set (map :kw (:duplicate-subs rf-result))))))

    (testing "detects deprecated :dispatch-n effect"
      (is (= 1 (count (:deprecated-effects rf-result)))))

    (testing "deprecated effect has correct metadata"
      (let [dep (first (:deprecated-effects rf-result))]
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
