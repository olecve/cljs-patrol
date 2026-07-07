(ns cljs-patrol.groups.a11y-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [clojure.test :refer [deftest is testing]]))

(defn- finding [row]
  {:type :img-alt-missing
   :kw :img
   :file "views.cljs"
   :row row})

(deftest analyze-test
  (testing "no usages — no violations"
    (is (empty? (:img-alt-missing (group/analyze a11y/group {:usages []})))))

  (testing "filters usages by :img-alt-missing type"
    (let [other {:type :some-other-thing
                 :kw :div
                 :file "v.cljs"
                 :row 1}
          result (group/analyze a11y/group
                                {:usages [(finding 3) other (finding 8)]})]
      (is (= 2 (count (:img-alt-missing result))))
      (is (= #{3 8} (set (map :row (:img-alt-missing result))))))))

(deftest failed?-test
  (testing "fails when any img is missing alt"
    (is (group/failed? a11y/group {:img-alt-missing [(finding 5)]})))

  (testing "does not fail when clean"
    (is (not (group/failed? a11y/group {:img-alt-missing []})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines a11y/group
                                   {:img-alt-missing [(finding 1) (finding 2)]})]
    (is (= 1 (count lines)))
    (is (= 2 (second (first lines))))))

(deftest rule->tier-test
  (testing "img-alt-missing is a bug"
    (is (= :bugs (get (group/rule->tier a11y/group) :img-alt-missing)))))
