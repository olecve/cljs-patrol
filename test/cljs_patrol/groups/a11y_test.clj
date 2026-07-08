(ns cljs-patrol.groups.a11y-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [clojure.test :refer [deftest is testing]]))

(defn- img-finding [row]
  {:type :img-alt-missing
   :kw :img
   :file "views.cljs"
   :row row})

(defn- tabindex-finding [row]
  {:type :invalid-tabindex
   :kw :div
   :file "views.cljs"
   :row row})

(defn- on-click-finding [row]
  {:type :on-click-on-non-interactive
   :kw :div
   :file "views.cljs"
   :row row})

(defn- empty-interactive-finding [row]
  {:type :empty-interactive-element
   :kw :button
   :file "views.cljs"
   :row row})

(deftest analyze-test
  (testing "no usages — no findings"
    (let [result (group/analyze a11y/group {:usages []})]
      (is (empty? (:img-alt-missing result)))
      (is (empty? (:invalid-tabindex result)))
      (is (empty? (:on-click-on-non-interactive result)))
      (is (empty? (:empty-interactive-element result)))))

  (testing "splits usages by :type across all rules"
    (let [other {:type :some-other-thing
                 :kw :div
                 :file "v.cljs"
                 :row 1}
          result (group/analyze a11y/group
                                {:usages [(img-finding 3)
                                          (tabindex-finding 5)
                                          (on-click-finding 7)
                                          (empty-interactive-finding 9)
                                          other
                                          (img-finding 8)]})]
      (is (= 2 (count (:img-alt-missing result))))
      (is (= #{3 8} (set (map :row (:img-alt-missing result)))))
      (is (= 1 (count (:invalid-tabindex result))))
      (is (= #{5} (set (map :row (:invalid-tabindex result)))))
      (is (= 1 (count (:on-click-on-non-interactive result))))
      (is (= #{7} (set (map :row (:on-click-on-non-interactive result)))))
      (is (= 1 (count (:empty-interactive-element result))))
      (is (= #{9} (set (map :row (:empty-interactive-element result))))))))

(deftest failed?-test
  (testing "fails when any img is missing alt"
    (is (group/failed? a11y/group {:img-alt-missing [(img-finding 5)]
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []})))

  (testing "fails when any tabindex is invalid"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex [(tabindex-finding 5)]
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []})))

  (testing "fails when any on-click-on-non-interactive is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive [(on-click-finding 5)]
                                   :empty-interactive-element []})))

  (testing "fails when any empty-interactive-element is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element [(empty-interactive-finding 5)]})))

  (testing "does not fail when clean"
    (is (not (group/failed? a11y/group {:img-alt-missing []
                                        :invalid-tabindex []
                                        :on-click-on-non-interactive []
                                        :empty-interactive-element []})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines a11y/group
                                   {:img-alt-missing [(img-finding 1) (img-finding 2)]
                                    :invalid-tabindex [(tabindex-finding 3)]
                                    :on-click-on-non-interactive [(on-click-finding 4) (on-click-finding 5)]
                                    :empty-interactive-element [(empty-interactive-finding 6)]})]
    (is (= 4 (count lines)))
    (is (= 2 (second (first lines))))
    (is (= 1 (second (second lines))))
    (is (= 2 (second (nth lines 2))))
    (is (= 1 (second (nth lines 3))))))

(deftest rule->tier-test
  (let [tiers (group/rule->tier a11y/group)]
    (testing "img-alt-missing is a bug"
      (is (= :bugs (get tiers :img-alt-missing))))

    (testing "invalid-tabindex is a bug"
      (is (= :bugs (get tiers :invalid-tabindex))))

    (testing "on-click-on-non-interactive is a bug"
      (is (= :bugs (get tiers :on-click-on-non-interactive))))

    (testing "empty-interactive-element is a bug"
      (is (= :bugs (get tiers :empty-interactive-element))))))

(deftest suggestions-test
  (let [suggestions (group/suggestions a11y/group)]
    (testing "img-alt-missing suggestion references WCAG"
      (is (re-find #"WCAG.*1\.1\.1" (:img-alt-missing suggestions))))

    (testing "invalid-tabindex suggestion references WCAG focus order"
      (is (re-find #"WCAG.*2\.4\.3" (:invalid-tabindex suggestions))))

    (testing "on-click-on-non-interactive suggestion references WCAG keyboard"
      (is (re-find #"WCAG.*2\.1\.1" (:on-click-on-non-interactive suggestions))))

    (testing "empty-interactive-element suggestion references WCAG name/role/value"
      (is (re-find #"WCAG.*4\.1\.2" (:empty-interactive-element suggestions))))))
