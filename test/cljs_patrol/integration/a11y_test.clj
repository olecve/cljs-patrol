(ns cljs-patrol.integration.a11y-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.a11y :as a11y]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/a11y-app/src/blogapp")

(defn- rows [items]
  (frequencies (map :row items)))

(deftest img-alt-missing-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [img-alt-missing]} (first group-results)
        by-row (rows img-alt-missing)]

    (testing "flags [:img] with no attrs"
      (is (contains? by-row 30)
          "bad-no-attrs case — [:img] with no attrs slot"))

    (testing "flags [:img {...}] literal map without :alt"
      (is (contains? by-row 33)
          "bad-attrs-without-alt case — literal attrs map missing :alt key"))

    (testing "flags Hiccup .class shorthand without :alt"
      (is (contains? by-row 36)
          "bad-hiccup-class-without-alt case — :img.thumb with no :alt"))

    (testing "flags empty literal attrs map"
      (is (contains? by-row 40)
          "bad-empty-attrs case — [:img {}] literal empty map"))

    (testing "flags every :img on a line that has multiple siblings"
      (is (= 2 (get by-row 45))
          "bad-two-on-same-line case — two [:img] on row 45 both flagged"))

    (testing "flags [:img {::alt \"...\"}] — ::alt is not the HTML :alt attribute"
      (is (contains? by-row 49)
          "ok-namespaced-alt-key-does-not-satisfy case — ::foo/alt ≠ :alt"))

    (testing "does not flag [:img {:alt \"...\"}]"
      (is (not (contains? by-row 6))
          "ok-with-alt case — [:img {:src ... :alt \"Blogapp logo\"}]")
      (is (not (contains? by-row 14))
          "ok-hiccup-class-with-alt case — :img.hero with :alt")
      (is (not (contains? by-row 18))
          "ok-hiccup-id-with-alt case — :img#avatar with :alt"))

    (testing "does not flag decorative :alt \"\""
      (is (not (contains? by-row 10))
          "ok-decorative case — :alt \"\" is valid per WCAG for decorative images"))

    (testing "does not flag quoted-vector Hiccup"
      (is (not (contains? by-row 23))
          "ok-quoted-vector-skipped case — '[:img] is data, not a live Hiccup form"))

    (testing "does not flag meta-wrapped attrs (conservative)"
      (is (not (contains? by-row 27))
          "ok-meta-wrapped-attrs case — [:img ^:foo {...}] treated as :dynamic"))

    (testing "does not flag when attrs are a non-literal form (conservative)"
      (is (not (contains? by-row 53))
          "dynamic-attrs-skipped case — [:img (merge ...)] non-literal attrs"))

    (testing "every finding carries the :img tag as :kw"
      (is (every? #(= :img (:kw %)) img-alt-missing)))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) img-alt-missing)))

    (testing "every finding carries a :col so same-line siblings differ"
      (is (every? #(pos? (:col %)) img-alt-missing))
      (let [same-row-findings (filter #(= 45 (:row %)) img-alt-missing)
            same-row-cols (set (map :col same-row-findings))]
        (is (= 2 (count same-row-cols))
            "the two [:img] on row 45 have distinct :col values")))))

(deftest invalid-tabindex-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [invalid-tabindex]} (first group-results)
        by-row (rows invalid-tabindex)]

    (testing "flags positive integer tabindex"
      (is (contains? by-row 71)
          "bad-positive-tabindex case — {:tabIndex 1}")
      (is (contains? by-row 74)
          "bad-large-positive-tabindex case — {:tabIndex 100} on :button"))

    (testing "flags kebab-case :tab-index with positive value"
      (is (contains? by-row 87)
          "bad-kebab-positive-tabindex case — {:tab-index 5}"))

    (testing "flags string tabindex value"
      (is (contains? by-row 77)
          "bad-string-tabindex case — {:tabIndex \"1\"} is not an int"))

    (testing "flags float tabindex value"
      (is (contains? by-row 80)
          "bad-float-tabindex case — {:tabIndex 1.5} is not an int"))

    (testing "flags keyword tabindex value"
      (is (contains? by-row 83)
          "bad-keyword-tabindex case — {:tabIndex :something} is not an int"))

    (testing "does not flag :tabIndex 0 (in tab order — correct)"
      (is (not (contains? by-row 58))
          "ok-tabindex-zero case — 0 is a valid tabindex"))

    (testing "does not flag :tabIndex -1 (programmatic focus — correct)"
      (is (not (contains? by-row 61))
          "ok-tabindex-negative case — -1 is a valid tabindex"))

    (testing "does not flag kebab-case with 0"
      (is (not (contains? by-row 64))
          "ok-tab-index-kebab-zero case — kebab-case with 0 is valid"))

    (testing "does not flag non-literal tabindex value (conservative)"
      (is (not (contains? by-row 68))
          "ok-dynamic-tabindex case — {:tabIndex n} value is a symbol"))

    (testing "finding carries the element tag (not always :img)"
      (let [tags (set (map :kw invalid-tabindex))]
        (is (contains? tags :div))
        (is (contains? tags :button))))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) invalid-tabindex)))))
