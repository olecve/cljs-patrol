(ns cljs-patrol.integration-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/re-frame-spade-app/src/webapp")
(def ^:private all-groups [re-frame/group spade/group])

(deftest full-analysis-test
  (let [{:keys [group-results]} (core/run fixture-dir all-groups)
        rf-result (nth group-results 0)
        spade-result (nth group-results 1)]

    (testing "detects unused re-frame subscription"
      (is (= #{:webapp.subs/unused-sub}
             (set (map :kw (:unused-subs rf-result))))))

    (testing "detects unused re-frame event"
      (is (= #{:webapp.events/unused-event}
             (set (map :kw (:unused-events rf-result))))))

    (testing "detects phantom subscription"
      (is (= #{:webapp.phantom/ghost-sub}
             (set (map :kw (:phantom-subs rf-result))))))

    (testing "does not flag used subscription as unused"
      (is (not (contains? (set (map :kw (:unused-subs rf-result)))
                          :webapp.subs/used-sub))))

    (testing "detects unused Spade styles"
      (is (= #{:webapp.styles/unused-style :webapp.styles/unused-attrs}
             (set (map :kw (:unused-styles spade-result))))))

    (testing "does not flag used style as unused"
      (is (not (contains? (set (map :kw (:unused-styles spade-result)))
                          :webapp.styles/container-style))))

    (testing "detects duplicate subscription registration"
      (is (= 2 (count (:duplicate-subs rf-result))))
      (is (= #{:webapp.subs/used-sub} (set (map :kw (:duplicate-subs rf-result))))))

    (testing "detects deprecated :dispatch-n effect"
      (is (= 1 (count (:deprecated-effects rf-result)))))

    (testing "deprecated effect has correct metadata"
      (let [dep (first (:deprecated-effects rf-result))]
        (is (= :deprecated (:type dep)))
        (is (= ":dispatch-n" (:effect dep)))))))
