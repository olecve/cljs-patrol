(ns cljs-patrol.integration.event-db-effects-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/event-db-effects-app/src/webapp")

(defn- kws [items]
  (set (map :kw items)))

(deftest reg-event-db-returning-effects-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [re-frame/group])
        result (first group-results)
        flagged (kws (:reg-event-db-returning-effects result))]

    (testing "canonical effects-shape return is flagged"
      (is (contains? flagged :webapp.events/cart-add-success-bug)))

    (testing "effects-style return with :fx key is flagged"
      (is (contains? flagged :webapp.events/with-fx-key-bug)))

    (testing "let-wrapped effects-style return is flagged"
      (is (contains? flagged :webapp.events/let-wrapped-bug)))

    (testing "reg-event-db with interceptors vector is flagged"
      (is (contains? flagged :webapp.events/with-interceptors-bug)))

    (testing "if-branch returning effects-style map is flagged"
      (is (contains? flagged :webapp.events/if-then-bug)))

    (testing "plain reg-event-db returning a db value is NOT flagged"
      (is (not (contains? flagged :webapp.events/plain-db-update))))

    (testing "reg-event-db using reduce with shadowed inner db is NOT flagged"
      (is (not (contains? flagged :webapp.events/reduce-over-accumulator))
          "the inner (fn [db item] ...) is not at the outer handler's tail"))

    (testing "reg-event-fx returning effects-style map is NOT flagged"
      (is (not (contains? flagged :webapp.events/correct-event-fx))
          "the rule only applies to reg-event-db, not reg-event-fx"))

    (testing "reg-event-fx with reduce containing inner db shadow is NOT flagged"
      (is (not (contains? flagged :webapp.events/correct-fx-with-reduce))))))
