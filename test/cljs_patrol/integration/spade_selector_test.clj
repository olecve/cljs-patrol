(ns cljs-patrol.integration.spade-selector-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/spade-selector-app/src/webapp")

(defn- findings [rule-key]
  (get (first (:group-results (core/run fixture-dir [spade/group]))) rule-key))

(deftest ampersand-not-at-start-test
  (let [found (findings :spade-ampersand-not-at-start)
        flagged (set (map (juxt :kw :selector) found))]

    (testing "flags every string selector with an & past position 0"
      (is (= #{[:webapp.selector-styles/focus-within-style "li:focus-within &"]
               [:webapp.selector-styles/chained-ampersand-attrs "&:hover &"]
               [:webapp.selector-styles/mid-ampersand-style "foo & bar"]
               [:webapp.selector-styles/nested-block-style "aside:hover &"]}
             flagged)))

    (testing "does not flag an & that leads the selector"
      (is (not (contains? (set (map :kw found))
                          :webapp.selector-styles/ampersand-at-front-style))
          "\"&\", \"&:hover\" and \"&[data-x]\" all substitute correctly"))

    (testing "does not flag selectors without an &, nor an & in a property value"
      (is (not (contains? (set (map :kw found))
                          :webapp.selector-styles/no-ampersand-style))))

    (testing "findings carry the declaration form and the bug tier"
      (is (every? #(= :bugs (:tier %)) found))
      (is (contains? (set (map :form found))
                     ":webapp.selector-styles/mid-ampersand-style \"foo & bar\"")))))

(deftest keyword-combinator-selector-test
  (let [found (findings :spade-keyword-combinator-selector)
        flagged (set (map (juxt :kw :selectors) found))]

    (testing "flags a combinator keyword leading a selector vector"
      (is (= #{[:webapp.selector-styles/child-combinator-style [":>" ":span"]]
               [:webapp.selector-styles/adjacent-combinator-attrs [":+" ":p"]]
               [:webapp.selector-styles/mid-combinator-style [":.icon" ":>" ":img"]]
               [:webapp.selector-styles/nested-block-style [":>" ":button"]]}
             flagged)))

    (testing "does not flag the string form of a combinator"
      (is (not (contains? (set (map :kw found))
                          :webapp.selector-styles/string-combinator-style))
          "[\"> span\" {...}] is the correct spelling"))

    (testing "does not flag compound or descendant selector vectors"
      (is (not (contains? (set (map :kw found))
                          :webapp.selector-styles/descendant-style))
          "[:&:hover {...}], [:svg :path {...}] and [:&:hover :span {...}] are all valid"))

    (testing "findings carry the declaration form and the bug tier"
      (is (every? #(= :bugs (:tier %)) found))
      (is (contains? (set (map :form found))
                     ":webapp.selector-styles/child-combinator-style [:> :span]")))))
