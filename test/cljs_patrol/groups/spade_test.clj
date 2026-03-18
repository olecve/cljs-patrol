(ns cljs-patrol.groups.spade-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private defclass-decl {:kw :webapp.styles/container :type :defclass :file "styles.cljs" :row 1})
(def ^:private defattrs-decl {:kw :webapp.styles/btn-attrs :type :defattrs :file "styles.cljs" :row 5})
(def ^:private style-usage {:kw :webapp.styles/container :type :style-call :file "views.cljs" :row 10})
(def ^:private defattrs-merged {:kw :webapp.styles/merged-attrs :type :defattrs :file "styles.cljs" :row 7})
(def ^:private merge-usage {:kw :webapp.styles/merged-attrs :type :style-call :file "views.cljs" :row 12 :context :in-merge})
(def ^:private defclass-sole {:kw :webapp.styles/sole-style :type :defclass :file "styles.cljs" :row 9})
(def ^:private class-only-usage {:kw :webapp.styles/sole-style :type :style-call :file "views.cljs" :row 14 :context :class-only-map})

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
      (is (= 1 (count (:unused-styles result))))))

  (testing "defattrs-in-merge: flags defattrs used in merge"
    (let [result (group/analyze spade/group
                                {:declarations [defattrs-merged]
                                 :usages [merge-usage]})]
      (is (= 1 (count (:defattrs-in-merge result))))
      (is (= :webapp.styles/merged-attrs (:kw (first (:defattrs-in-merge result)))))))

  (testing "defattrs-in-merge: does not flag defattrs without merge usage"
    (let [plain-usage {:kw :webapp.styles/btn-attrs :type :style-call :file "v.cljs" :row 3 :context nil}
          result (group/analyze spade/group
                                {:declarations [defattrs-decl]
                                 :usages [plain-usage]})]
      (is (empty? (:defattrs-in-merge result)))))

  (testing "defclass-as-sole-attr: flags defclass used only in class-only-map"
    (let [result (group/analyze spade/group
                                {:declarations [defclass-sole]
                                 :usages [class-only-usage]})]
      (is (= 1 (count (:defclass-as-sole-attr result))))
      (is (= :webapp.styles/sole-style (:kw (first (:defclass-as-sole-attr result)))))))

  (testing "defclass-as-sole-attr: not flagged when any usage is not class-only-map"
    (let [mixed-usage {:kw :webapp.styles/sole-style :type :style-call :file "v.cljs" :row 20 :context nil}
          result (group/analyze spade/group
                                {:declarations [defclass-sole]
                                 :usages [class-only-usage mixed-usage]})]
      (is (empty? (:defclass-as-sole-attr result)))))

  (testing "defclass-as-sole-attr: not flagged when defclass has no usages"
    (let [result (group/analyze spade/group
                                {:declarations [defclass-sole]
                                 :usages []})]
      (is (empty? (:defclass-as-sole-attr result))))))

(deftest failed?-test
  (testing "fails when unused styles exist"
    (is (group/failed? spade/group {:unused-styles [defclass-decl]})))

  (testing "does not fail when no unused styles"
    (is (not (group/failed? spade/group {:unused-styles []}))))

  (testing "does not fail for defattrs-in-merge (warning only)"
    (is (not (group/failed? spade/group {:unused-styles []
                                         :defattrs-in-merge [defattrs-merged]
                                         :defclass-as-sole-attr []}))))

  (testing "does not fail for defclass-as-sole-attr (warning only)"
    (is (not (group/failed? spade/group {:unused-styles []
                                         :defattrs-in-merge []
                                         :defclass-as-sole-attr [defclass-sole]})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines spade/group {:unused-styles [defclass-decl defattrs-decl]
                                                :defattrs-in-merge [defattrs-merged]
                                                :defclass-as-sole-attr []})]
    (is (= 3 (count lines)))
    (is (= 2 (second (first lines))))
    (is (= 1 (second (second lines))))
    (is (= 0 (second (nth lines 2))))))
