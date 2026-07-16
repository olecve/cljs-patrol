(ns cljs-patrol.groups.spade-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private defclass-decl {:kw :webapp.styles/container
                              :type :defclass
                              :file "styles.cljs"
                              :row 1})
(def ^:private defattrs-decl {:kw :webapp.styles/btn-attrs
                              :type :defattrs
                              :file "styles.cljs"
                              :row 5})
(def ^:private style-usage {:kw :webapp.styles/container
                            :type :style-call
                            :file "views.cljs"
                            :row 10})
(def ^:private defattrs-merged {:kw :webapp.styles/merged-attrs
                                :type :defattrs
                                :file "styles.cljs"
                                :row 7})
(def ^:private merge-usage {:kw :webapp.styles/merged-attrs
                            :type :style-call
                            :file "views.cljs"
                            :row 12
                            :context :in-merge})
(def ^:private pseudo-hover {:kw :webapp.styles/menu-item-style
                             :type :pseudo-in-main-map
                             :selector ":&:hover"
                             :form ":webapp.styles/menu-item-style :&:hover"
                             :file "pseudo_styles.cljs"
                             :row 8})
(def ^:private pseudo-first-child {:kw :webapp.styles/card-section-attrs
                                   :type :pseudo-in-main-map
                                   :selector ":&:first-child"
                                   :form ":webapp.styles/card-section-attrs :&:first-child"
                                   :file "pseudo_styles.cljs"
                                   :row 14})
(def ^:private consecutive-before-after {:kw :webapp.styles/badge-marker-attrs
                                         :type :consecutive-self-selectors
                                         :selectors [":&:before" ":&:after"]
                                         :form ":webapp.styles/badge-marker-attrs [:&:before :&:after]"
                                         :file "pseudo_styles.cljs"
                                         :row 26})

(deftest analyze-test
  (testing "no styles declared — nothing unused"
    (is (empty? (:unused-styles (group/analyze spade/group {:declarations []
                                                            :usages []})))))

  (testing "declared and used — not unused"
    (let [result (group/analyze spade/group {:declarations [defclass-decl]
                                             :usages [style-usage]})]
      (is (empty? (:unused-styles result)))))

  (testing "declared but not used — unused"
    (let [result (group/analyze spade/group {:declarations [defclass-decl defattrs-decl]
                                             :usages []})]
      (is (= 2 (count (:unused-styles result))))))

  (testing "ignores non-spade declaration types"
    (let [other {:kw :webapp/sub
                 :type :sub
                 :file "f.cljs"
                 :row 1}
          result (group/analyze spade/group {:declarations [defclass-decl other]
                                             :usages []})]
      (is (= 1 (count (:unused-styles result))))))

  (testing "defattrs-in-merge: flags defattrs used in merge"
    (let [result (group/analyze spade/group
                                {:declarations [defattrs-merged]
                                 :usages [merge-usage]})]
      (is (= 1 (count (:defattrs-in-merge result))))
      (is (= :webapp.styles/merged-attrs (:kw (first (:defattrs-in-merge result)))))))

  (testing "defattrs-in-merge: does not flag defattrs without merge usage"
    (let [plain-usage {:kw :webapp.styles/btn-attrs
                       :type :style-call
                       :file "v.cljs"
                       :row 3
                       :context nil}
          result (group/analyze spade/group
                                {:declarations [defattrs-decl]
                                 :usages [plain-usage]})]
      (is (empty? (:defattrs-in-merge result)))))

  (testing "pseudo-in-main-map: surfaces findings from declarations"
    (let [result (group/analyze spade/group
                                {:declarations [defclass-decl pseudo-hover pseudo-first-child]
                                 :usages [style-usage]})]
      (is (= 2 (count (:pseudo-in-main-map result))))
      (is (= #{":&:hover" ":&:first-child"}
             (set (map :selector (:pseudo-in-main-map result)))))))

  (testing "pseudo-in-main-map: does not double-count style as unused"
    (let [result (group/analyze spade/group
                                {:declarations [pseudo-hover]
                                 :usages []})]
      (is (empty? (:unused-styles result)))))

  (testing "consecutive-self-selectors: surfaces findings from declarations"
    (let [result (group/analyze spade/group
                                {:declarations [defclass-decl consecutive-before-after]
                                 :usages [style-usage]})]
      (is (= 1 (count (:consecutive-self-selectors result))))
      (is (= [":&:before" ":&:after"]
             (:selectors (first (:consecutive-self-selectors result))))))))

(deftest failed?-test
  (testing "fails when unused styles exist"
    (is (group/failed? spade/group {:unused-styles [defclass-decl]})))

  (testing "does not fail when no unused styles"
    (is (not (group/failed? spade/group {:unused-styles []}))))

  (testing "does not fail for defattrs-in-merge (warning only)"
    (is (not (group/failed? spade/group {:unused-styles []
                                         :defattrs-in-merge [defattrs-merged]}))))

  (testing "fails when pseudo-in-main-map findings exist"
    (is (group/failed? spade/group {:unused-styles []
                                    :pseudo-in-main-map [pseudo-hover]})))

  (testing "fails when consecutive-self-selectors findings exist"
    (is (group/failed? spade/group {:unused-styles []
                                    :consecutive-self-selectors [consecutive-before-after]}))))

(deftest summary-lines-test
  (let [lines (group/summary-lines spade/group {:unused-styles [defclass-decl defattrs-decl]
                                                :defattrs-in-merge [defattrs-merged]
                                                :pseudo-in-main-map [pseudo-hover pseudo-first-child]
                                                :consecutive-self-selectors [consecutive-before-after]})]
    (is (= 4 (count lines)))
    (is (= 2 (second (nth lines 0))))
    (is (= 1 (second (nth lines 1))))
    (is (= 2 (second (nth lines 2))))
    (is (= 1 (second (nth lines 3))))))
