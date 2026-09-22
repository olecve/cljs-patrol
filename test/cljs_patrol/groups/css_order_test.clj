(ns cljs-patrol.groups.css-order-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.css-order :as css-order]
   [cljs-patrol.groups.css-order.orders :as orders]
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

(deftest order-tables-test
  (testing "every table named in the docs loads, at the size its package publishes"
    (is (= [:recess :clean :concentric :smacss :idiomatic] orders/names))
    (is (= {:recess 496
            :clean 468
            :concentric 330
            :smacss 225
            :idiomatic 64}
           (into {} (map (fn [order] [order (count (orders/ranks order))])) orders/names))))

  (testing "recess ranks outside first, typography ahead of background"
    (is (< (orders/rank :recess "position") (orders/rank :recess "display")))
    (is (< (orders/rank :recess "display") (orders/rank :recess "padding")))
    (is (< (orders/rank :recess "padding") (orders/rank :recess "color")))
    (is (< (orders/rank :recess "color") (orders/rank :recess "background"))))

  (testing "concentric is the other way round — border and background ahead of text"
    (is (< (orders/rank :concentric "border") (orders/rank :concentric "color")))
    (is (< (orders/rank :concentric "background") (orders/rank :concentric "color"))))

  (testing "a property the chosen table does not name sorts after every one it does"
    (is (= orders/unknown-rank (orders/rank :recess "--custom-gap")))
    (is (= orders/unknown-rank (orders/rank :recess "not-a-property")))
    (is (> (orders/rank :recess "--custom-gap") (orders/rank :recess "break-inside")))
    (is (= orders/unknown-rank (orders/rank :idiomatic "color"))
        "idiomatic deliberately ranks only structural properties"))

  (testing "an unknown table name falls back to the default rather than stopping the run"
    (is (= :recess (orders/resolve-order nil)))
    (is (= :smacss (orders/resolve-order :smacss)))
    (is (= orders/default-order (orders/resolve-order :no-such-order)))))

(deftest make-group-test
  (testing "the group reports which table it is using"
    (is (= "CSS order (recess)" (group/group-name (css-order/make-group))))
    (is (= "CSS order (smacss)" (group/group-name (css-order/make-group {:order :smacss})))))

  (testing "the suggestion names the package the order came from"
    (let [text (:css-property-order-outside-in (group/suggestions (css-order/make-group {:order :concentric})))]
      (is (re-find #"stylelint-config-concentric-order" text))
      (is (re-find #"recess, clean, concentric, smacss, idiomatic" text)))))

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
