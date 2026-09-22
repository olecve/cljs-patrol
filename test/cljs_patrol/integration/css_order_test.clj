(ns cljs-patrol.integration.css-order-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.css-order :as css-order]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/css-order-app/src/webapp")

(defn- run []
  (core/run fixture-dir [css-order/group]))

(defn- findings []
  (:css-property-order-outside-in (first (:group-results (run)))))

(defn- by-style [items]
  (into {} (map (juxt :kw identity)) items))

(deftest flags-out-of-order-blocks-test
  (let [found (findings)
        styles (by-style found)]

    (testing "flags every out-of-order block and nothing else"
      (is (= #{:webapp.order-styles/banner-style
               :webapp.order-styles/widget-style
               :webapp.order-styles/link-style
               :webapp.order-styles/row-attrs
               :webapp.order-styles/panel-style
               :webapp.order-styles/muted-style}
             (set (keys styles)))))

    (testing "names the property out of place, what it must precede, and what it follows"
      (let [banner (get styles :webapp.order-styles/banner-style)]
        (is (= ":display" (:property banner)))
        (is (= ":color" (:expected-before banner)))
        (is (= ":color" (:found-after banner)))
        (is (= "Move :display before :color." (:hint banner)))))

    (testing "reads defattrs the same way as defclass"
      (is (= ":display" (:property (get styles :webapp.order-styles/row-attrs)))))

    (testing "reads a declaration whose name carries metadata"
      (is (= ":display" (:property (get styles :webapp.order-styles/muted-style)))
          "(defclass ^:private muted-style …) — the name sits behind a meta node"))

    (testing "reports one finding per block, at the offending property's row"
      (is (= 6 (count found)))
      (is (= 16 (:row (get styles :webapp.order-styles/banner-style)))))))

(deftest unranked-properties-test
  (let [styles (by-style (findings))]

    (testing "a ranked property after an unranked one is out of order"
      (let [widget (get styles :webapp.order-styles/widget-style)]
        (is (= ":padding" (:property widget)))
        (is (= ":--widget-gap" (:expected-before widget)))))

    (testing "unranked properties trailing the ranked ones are left alone"
      (is (not (contains? styles :webapp.order-styles/surface-style))
          "two custom properties in a row are unordered between themselves"))))

(deftest nested-selector-blocks-test
  (let [styles (by-style (findings))]

    (testing "a nested block is judged on its own, not folded into the base map"
      (let [link (get styles :webapp.order-styles/link-style)]
        (is (= ":&:hover" (:selector link))
            "the base map of link-style is in order; only its :&:hover block is not")
        (is (= ":padding" (:property link)))))

    (testing "a block nested inside a nested block is reached, and carries the selector path"
      (let [panel (get styles :webapp.order-styles/panel-style)]
        (is (= ":.section :&:focus-within" (:selector panel)))
        (is (= ":color" (:property panel)))))))

(deftest skipped-blocks-test
  (let [styles (by-style (findings))]

    (testing "a map a call builds is not read"
      (is (not (contains? styles :webapp.order-styles/themed-attrs))
          "the maps inside (merge …) / (if …) are out of order but are not literals of the body"))

    (testing "blocks under four properties are not judged"
      (is (not (contains? styles :webapp.order-styles/chip-style))))

    (testing "a block already in order is not flagged"
      (is (not (contains? styles :webapp.order-styles/card-style))))))

(deftest order-choice-test
  (testing "the chosen table decides what reads as out of order"
    (let [flagged (fn [order]
                    (set (map :kw (:css-property-order-outside-in
                                   (first (:group-results
                                           (core/run fixture-dir [(css-order/make-group {:order order})])))))))
          recess (flagged :recess)
          concentric (flagged :concentric)]
      (is (not (contains? recess :webapp.order-styles/card-style))
          "recess puts typography before background, so :color then :background is in order")
      (is (contains? concentric :webapp.order-styles/card-style)
          "concentric puts background before text, so the same block is out of order")
      (is (not= recess concentric)))))

(deftest tier-test
  (is (every? #(= :cleanup (:tier %)) (findings))))

(deftest baseline-flow-test
  (let [run-results [(run)]
        identities (baseline/collect-identities run-results)]

    (testing "every finding gets a baseline identity, one per block"
      (is (= 6 (count identities)))
      (is (every? #(= :css-property-order-outside-in (:rule %)) identities)))

    (testing "identity is the declaration plus the selector it sits under"
      (is (contains? identities {:rule :css-property-order-outside-in
                                 :ns "webapp.order-styles"
                                 :var "panel-style"
                                 :selector ":.section :&:focus-within"})))

    (testing "with everything baselined, a rerun reports nothing new"
      (let [{:keys [new present fixed]} (baseline/diff-baseline identities identities)]
        (is (empty? new))
        (is (= identities present))
        (is (empty? fixed))))

    (testing "a finding absent from the baseline is new"
      (let [without-banner (into #{}
                                 (remove #(= "banner-style" (:var %)))
                                 identities)
            {:keys [new]} (baseline/diff-baseline without-banner identities)]
        (is (= 1 (count new)))
        (is (= "banner-style" (:var (first new))))))))
