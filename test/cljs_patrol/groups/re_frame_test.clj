(ns cljs-patrol.groups.re-frame-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [clojure.test :refer [deftest is testing]]))

(def ^:private sub-decl
  {:kw ::my-sub
   :type :sub
   :file "subs.cljs"
   :row 1})

(def ^:private event-decl
  {:kw ::my-event
   :type :event
   :file "events.cljs"
   :row 1})

(def ^:private sub-usage
  {:kw ::my-sub
   :type :sub
   :file "views.cljs"
   :row 10})

(def ^:private event-usage
  {:kw ::my-event
   :type :event
   :file "views.cljs"
   :row 20})

(deftest analyze-test
  (testing "all declared and used — no issues"
    (let [result (group/analyze re-frame/group {:declarations [sub-decl event-decl]
                                                :usages [sub-usage event-usage]
                                                :dynamic-sites []})]
      (is (empty? (:unused-subs result)))
      (is (empty? (:unused-events result)))
      (is (empty? (:phantom-subs result)))
      (is (empty? (:phantom-events result)))))

  (testing "declared but never used — unused"
    (let [result (group/analyze re-frame/group {:declarations [sub-decl event-decl]
                                                :usages []
                                                :dynamic-sites []})]
      (is (= 1 (count (:unused-subs result))))
      (is (= 1 (count (:unused-events result))))))

  (testing "used but never declared — phantom"
    (let [result (group/analyze re-frame/group {:declarations []
                                                :usages [sub-usage event-usage]
                                                :dynamic-sites []})]
      (is (= 1 (count (:phantom-subs result))))
      (is (= 1 (count (:phantom-events result))))))

  (testing "dynamic sites are passed through"
    (let [dyn {:form "(subscribe [ev])"
               :file "views.cljs"
               :row 5}
          result (group/analyze re-frame/group {:declarations []
                                                :usages []
                                                :dynamic-sites [dyn]})]
      (is (= [dyn] (:dynamic-sites result)))))

  (testing "duplicate registrations are detected"
    (let [sub-decl-2 (assoc sub-decl :row 99)
          result (group/analyze re-frame/group {:declarations [sub-decl sub-decl-2 event-decl]
                                                :usages []
                                                :dynamic-sites []})]
      (is (= 2 (count (:duplicate-subs result))))
      (is (empty? (:duplicate-events result)))))

  (testing "deprecated effects are separated from dynamic sites"
    (let [dep {:type :deprecated
               :effect ":dispatch-n"
               :form "[:dispatch-n [...]]"
               :file "f.cljs"
               :row 1}
          dyn {:form "(dispatch [ev])"
               :file "views.cljs"
               :row 5}
          result (group/analyze re-frame/group {:declarations []
                                                :usages []
                                                :dynamic-sites [dep dyn]})]
      (is (= [dep] (:deprecated-effects result)))
      (is (= [dyn] (:dynamic-sites result)))))

  (testing "reg-sub :=> with 1-arity fn is partitioned out of usages"
    (let [mismatch {:kw ::my-sub
                    :type :sugar-mismatch
                    :fn "last"
                    :file "subs.cljs"
                    :row 5}
          result (group/analyze re-frame/group {:declarations [sub-decl]
                                                :usages [mismatch]
                                                :dynamic-sites []})]
      (is (= [mismatch] (:reg-sub-=>-1-arity result)))
      (is (empty? (:phantom-subs result))
          "sugar-mismatch usages don't count as phantom")))

  (testing "reg-event-fx returning only :db is partitioned out of usages"
    (let [db-only {:kw ::my-event
                   :type :event-fx-db-only
                   :file "events.cljs"
                   :row 7}
          result (group/analyze re-frame/group {:declarations [event-decl]
                                                :usages [db-only]
                                                :dynamic-sites []})]
      (is (= [db-only] (:reg-event-fx-db-only result)))
      (is (empty? (:phantom-events result))
          "event-fx-db-only usages don't count as phantom")))

  (testing "reg-event-fx with empty effects is partitioned out of usages"
    (let [empty-fx {:kw ::my-event
                    :type :event-fx-empty
                    :file "events.cljs"
                    :row 9}
          result (group/analyze re-frame/group {:declarations [event-decl]
                                                :usages [empty-fx]
                                                :dynamic-sites []})]
      (is (= [empty-fx] (:reg-event-fx-empty result)))
      (is (empty? (:phantom-events result))
          "event-fx-empty usages don't count as phantom")))

  (testing "reg-event-db clobbering db is partitioned out of usages"
    (let [empty-db {:kw ::my-event
                    :type :event-db-empty
                    :file "events.cljs"
                    :row 11}
          result (group/analyze re-frame/group {:declarations [event-decl]
                                                :usages [empty-db]
                                                :dynamic-sites []})]
      (is (= [empty-db] (:reg-event-db-empty result)))
      (is (empty? (:phantom-events result))
          "event-db-empty usages don't count as phantom")))

  (testing "reg-event-db returning effects map is partitioned out of usages"
    (let [db-returning-effects {:kw ::my-event
                                :type :event-db-returning-effects
                                :file "events.cljs"
                                :row 13}
          result (group/analyze re-frame/group {:declarations [event-decl]
                                                :usages [db-returning-effects]
                                                :dynamic-sites []})]
      (is (= [db-returning-effects] (:reg-event-db-returning-effects result)))
      (is (empty? (:phantom-events result))
          "event-db-returning-effects usages don't count as phantom"))))

(deftest failed?-test
  (testing "fails on duplicate subs"
    (is (group/failed? re-frame/group {:duplicate-subs [sub-decl]
                                       :duplicate-events []
                                       :unused-subs []
                                       :unused-events []
                                       :deprecated-effects []})))

  (testing "fails on duplicate events"
    (is (group/failed? re-frame/group {:duplicate-subs []
                                       :duplicate-events [event-decl]
                                       :unused-subs []
                                       :unused-events []
                                       :deprecated-effects []})))

  (testing "fails on unused subs"
    (is (group/failed? re-frame/group {:duplicate-subs []
                                       :duplicate-events []
                                       :unused-subs [sub-decl]
                                       :unused-events []
                                       :deprecated-effects []})))

  (testing "fails on unused events"
    (is (group/failed? re-frame/group {:duplicate-subs []
                                       :duplicate-events []
                                       :unused-subs []
                                       :unused-events [event-decl]
                                       :deprecated-effects []})))

  (testing "fails on deprecated effects"
    (is (group/failed? re-frame/group {:duplicate-subs []
                                       :duplicate-events []
                                       :unused-subs []
                                       :unused-events []
                                       :deprecated-effects [{:effect ":dispatch-n"}]})))

  (testing "fails on reg-event-db returning effects map"
    (is (group/failed? re-frame/group {:duplicate-subs []
                                       :duplicate-events []
                                       :unused-subs []
                                       :unused-events []
                                       :deprecated-effects []
                                       :reg-event-db-returning-effects [event-decl]})))

  (testing "does not fail on phantom items only"
    (is (not (group/failed? re-frame/group {:duplicate-subs []
                                            :duplicate-events []
                                            :unused-subs []
                                            :unused-events []
                                            :deprecated-effects []
                                            :phantom-subs [sub-usage]
                                            :phantom-events [event-usage]}))))

  (testing "does not fail when clean"
    (is (not (group/failed? re-frame/group {:duplicate-subs []
                                            :duplicate-events []
                                            :unused-subs []
                                            :unused-events []
                                            :deprecated-effects []})))))

(deftest summary-lines-test
  (let [dep {:effect ":dispatch-n"
             :form "[:dispatch-n [...]]"
             :file "f.cljs"
             :row 1}
        sugar-mismatch {:kw ::sugar
                        :type :sugar-mismatch
                        :fn "last"
                        :file "subs.cljs"
                        :row 5}
        db-only-fx {:kw ::db-only
                    :type :event-fx-db-only
                    :file "events.cljs"
                    :row 7}
        empty-fx {:kw ::empty-fx
                  :type :event-fx-empty
                  :file "events.cljs"
                  :row 9}
        empty-db {:kw ::empty-db
                  :type :event-db-empty
                  :file "events.cljs"
                  :row 11}
        db-returning-effects {:kw ::db-returning-effects
                              :type :event-db-returning-effects
                              :file "events.cljs"
                              :row 13}
        result {:duplicate-subs [sub-decl sub-decl]
                :duplicate-events []
                :unused-subs [sub-decl]
                :unused-events []
                :phantom-subs [sub-usage]
                :phantom-events [event-usage]
                :deprecated-effects [dep]
                :reg-sub-=>-1-arity [sugar-mismatch]
                :reg-event-fx-db-only [db-only-fx db-only-fx]
                :reg-event-fx-empty [empty-fx]
                :reg-event-db-empty [empty-db]
                :reg-event-db-returning-effects [db-returning-effects]
                :dynamic-sites []}
        lines (group/summary-lines re-frame/group result)]
    (is (= [["Duplicate subscriptions:" 2]
            ["Duplicate events:" 0]
            ["Unused subscriptions:" 1]
            ["Unused events:" 0]
            ["Phantom subscriptions:" 1]
            ["Phantom events:" 1]
            ["Deprecated effects:" 1]
            ["reg-sub :=> with 1-arity fn:" 1]
            ["reg-event-fx returns only :db:" 2]
            ["reg-event-fx empty effects:" 1]
            ["reg-event-db clobbers db:" 1]
            ["reg-event-db returns effects map:" 1]
            ["Dynamic sites:" 0]]
           lines))))
