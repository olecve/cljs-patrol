(ns cljs-patrol.groups.css-order-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.css-order :as css-order]
   [cljs-patrol.groups.css-order.recess :as recess]
   [clojure.test :refer [deftest is testing]]))

(def ^:private order-finding {:kw :webapp.styles/banner-style
                              :type :css-property-order-outside-in
                              :selector ""
                              :property ":display"
                              :expected-before ":color"
                              :found-after ":color"
                              :form ":webapp.styles/banner-style {:color \"#333\" :display :block}"
                              :hint "Move :display before :color."
                              :file "order_styles.cljs"
                              :row 16})

(deftest recess-table-test
  (testing "the table is the full stylelint-config-recess-order property list"
    (is (= 496 recess/property-count)))

  (testing "ranks follow the table's own order, outside first"
    (is (< (recess/rank "position") (recess/rank "display")))
    (is (< (recess/rank "display") (recess/rank "padding")))
    (is (< (recess/rank "padding") (recess/rank "color")))
    (is (< (recess/rank "color") (recess/rank "background"))
        "the table puts typography ahead of background and border"))

  (testing "a property the table does not name sorts after every one it does"
    (is (= recess/unknown-rank (recess/rank "--custom-gap")))
    (is (= recess/unknown-rank (recess/rank "not-a-property")))
    (is (> (recess/rank "--custom-gap") (recess/rank "break-inside")))))

(deftest analyze-test
  (testing "surfaces order findings from declarations"
    (let [result (group/analyze css-order/group {:declarations [order-finding]})]
      (is (= 1 (count (:css-property-order-outside-in result))))
      (is (= ":display" (:property (first (:css-property-order-outside-in result)))))))

  (testing "ignores declarations from other groups"
    (let [other {:kw :webapp.styles/container
                 :type :defclass
                 :file "styles.cljs"
                 :row 1}
          result (group/analyze css-order/group {:declarations [other]})]
      (is (empty? (:css-property-order-outside-in result))))))

(deftest summary-lines-test
  (let [lines (group/summary-lines css-order/group {:css-property-order-outside-in [order-finding]})]
    (is (= 1 (count lines)))
    (is (= 1 (second (first lines))))))

(deftest failed?-test
  (testing "order findings never fail the run on their own"
    (is (not (group/failed? css-order/group {:css-property-order-outside-in [order-finding]}))
        "ordering is style hygiene; gate it with --fail-on when you want it blocking")))

(deftest rule->tier-test
  (is (= :cleanup (:css-property-order-outside-in (group/rule->tier css-order/group)))))
