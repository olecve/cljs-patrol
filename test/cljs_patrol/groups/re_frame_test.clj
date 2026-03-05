(ns cljs-patrol.groups.re-frame-test
  (:require
   [cljs-patrol.groups.re-frame :as re-frame]
   [clojure.test :refer [deftest is testing]]))

(def ^:private sub-decl {:kw ::my-sub :type :sub :file "subs.cljs" :row 1})
(def ^:private event-decl {:kw ::my-event :type :event :file "events.cljs" :row 1})
(def ^:private sub-usage {:kw ::my-sub :type :sub :file "views.cljs" :row 10})
(def ^:private event-usage {:kw ::my-event :type :event :file "views.cljs" :row 20})

(deftest analyze-test
  (testing "all declared and used — no issues"
    (let [result (re-frame/analyze {:declarations [sub-decl event-decl]
                                    :usages [sub-usage event-usage]
                                    :dynamic-sites []})]
      (is (empty? (:unused-subs result)))
      (is (empty? (:unused-events result)))
      (is (empty? (:phantom-subs result)))
      (is (empty? (:phantom-events result)))))

  (testing "declared but never used — unused"
    (let [result (re-frame/analyze {:declarations [sub-decl event-decl]
                                    :usages []
                                    :dynamic-sites []})]
      (is (= 1 (count (:unused-subs result))))
      (is (= 1 (count (:unused-events result))))))

  (testing "used but never declared — phantom"
    (let [result (re-frame/analyze {:declarations []
                                    :usages [sub-usage event-usage]
                                    :dynamic-sites []})]
      (is (= 1 (count (:phantom-subs result))))
      (is (= 1 (count (:phantom-events result))))))

  (testing "dynamic sites are passed through"
    (let [dyn {:form "(subscribe [ev])" :file "views.cljs" :row 5}
          result (re-frame/analyze {:declarations [] :usages [] :dynamic-sites [dyn]})]
      (is (= [dyn] (:dynamic-sites result)))))

  (testing "deprecated effects are separated from dynamic sites"
    (let [dep {:type :deprecated :effect ":dispatch-n" :form "[:dispatch-n [...]]" :file "f.cljs" :row 1}
          dyn {:form "(dispatch [ev])" :file "views.cljs" :row 5}
          result (re-frame/analyze {:declarations [] :usages [] :dynamic-sites [dep dyn]})]
      (is (= [dep] (:deprecated-effects result)))
      (is (= [dyn] (:dynamic-sites result))))))

(deftest failed?-test
  (testing "fails on unused subs"
    (is (re-frame/failed? {:unused-subs [sub-decl] :unused-events [] :deprecated-effects []})))

  (testing "fails on unused events"
    (is (re-frame/failed? {:unused-subs [] :unused-events [event-decl] :deprecated-effects []})))

  (testing "fails on deprecated effects"
    (is (re-frame/failed? {:unused-subs [] :unused-events [] :deprecated-effects [{:effect ":dispatch-n"}]})))

  (testing "does not fail on phantom items only"
    (is (not (re-frame/failed? {:unused-subs [] :unused-events [] :deprecated-effects []
                                :phantom-subs [sub-usage] :phantom-events [event-usage]}))))

  (testing "does not fail when clean"
    (is (not (re-frame/failed? {:unused-subs [] :unused-events [] :deprecated-effects []})))))

(deftest summary-lines-test
  (let [dep {:effect ":dispatch-n" :form "[:dispatch-n [...]]" :file "f.cljs" :row 1}
        result {:unused-subs [sub-decl]
                :unused-events []
                :phantom-subs [sub-usage]
                :phantom-events [event-usage]
                :deprecated-effects [dep]
                :dynamic-sites []}
        lines (re-frame/summary-lines result)]
    (is (= 6 (count lines)))
    (is (= 1 (second (nth lines 0)))) ; unused subs
    (is (= 0 (second (nth lines 1)))) ; unused events
    (is (= 1 (second (nth lines 2)))) ; phantom subs
    (is (= 1 (second (nth lines 3)))) ; phantom events
    (is (= 1 (second (nth lines 4)))) ; deprecated effects
    (is (= 0 (second (nth lines 5)))))) ; dynamic sites
