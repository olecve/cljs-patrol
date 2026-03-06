(ns cljs-patrol.groups.spade-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private defclass-decl {:kw :webapp.styles/container :type :defclass :file "styles.cljs" :row 1})
(def ^:private defattrs-decl {:kw :webapp.styles/btn-attrs :type :defattrs :file "styles.cljs" :row 5})
(def ^:private style-usage {:kw :webapp.styles/container :type :style-call :file "views.cljs" :row 10})

(deftest analyze-test
  (testing "no styles declared — nothing unused"
    (is (empty? (:unused-styles (group/analyze spade/group {:declarations [] :usages []})))))

  (testing "declared and used — not unused"
    (let [result (group/analyze spade/group {:declarations [defclass-decl] :usages [style-usage]})]
      (is (empty? (:unused-styles result)))))

  (testing "declared but not used — unused"
    (let [result (group/analyze spade/group {:declarations [defclass-decl defattrs-decl] :usages []})]
      (is (= 2 (count (:unused-styles result))))))

  (testing "ignores non-spade declaration types"
    (let [other {:kw :webapp/sub :type :sub :file "f.cljs" :row 1}
          result (group/analyze spade/group {:declarations [defclass-decl other] :usages []})]
      (is (= 1 (count (:unused-styles result)))))))

(deftest failed?-test
  (testing "fails when unused styles exist"
    (is (group/failed? spade/group {:unused-styles [defclass-decl]})))

  (testing "does not fail when no unused styles"
    (is (not (group/failed? spade/group {:unused-styles []})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines spade/group {:unused-styles [defclass-decl defattrs-decl]})]
    (is (= 1 (count lines)))
    (is (= 2 (second (first lines))))))
