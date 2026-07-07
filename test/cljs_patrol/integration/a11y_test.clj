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
      (is (contains? by-row 28)
          "bad-no-attrs case in views.cljs — [:img] with no attrs slot"))

    (testing "flags [:img {...}] literal map without :alt"
      (is (contains? by-row 31)
          "bad-attrs-without-alt case — literal attrs map missing :alt key"))

    (testing "flags Hiccup .class shorthand without :alt"
      (is (contains? by-row 34)
          "bad-hiccup-class-without-alt case — :img.thumb with no :alt"))

    (testing "flags empty literal attrs map"
      (is (contains? by-row 38)
          "bad-empty-attrs case — [:img {}] literal empty map"))

    (testing "flags every :img on a line that has multiple siblings"
      (is (= 2 (get by-row 43))
          "bad-two-on-same-line case — two [:img] on row 43 both flagged"))

    (testing "flags [:img {::alt \"...\"}] — ::alt is not the HTML :alt attribute"
      (is (contains? by-row 47)
          "ok-namespaced-alt-key-does-not-satisfy case — ::foo/alt ≠ :alt"))

    (testing "does not flag [:img {:alt \"...\"}]"
      (is (not (contains? by-row 4))
          "ok-with-alt case — [:img {:src ... :alt \"Blogapp logo\"}]")
      (is (not (contains? by-row 12))
          "ok-hiccup-class-with-alt case — :img.hero with :alt")
      (is (not (contains? by-row 16))
          "ok-hiccup-id-with-alt case — :img#avatar with :alt"))

    (testing "does not flag decorative :alt \"\""
      (is (not (contains? by-row 8))
          "ok-decorative case — :alt \"\" is valid per WCAG for decorative images"))

    (testing "does not flag quoted-vector Hiccup"
      (is (not (contains? by-row 21))
          "ok-quoted-vector-skipped case — '[:img] is data, not a live Hiccup form"))

    (testing "does not flag meta-wrapped attrs (conservative)"
      (is (not (contains? by-row 25))
          "ok-meta-wrapped-attrs case — [:img ^:foo {...}] treated as :dynamic"))

    (testing "does not flag when attrs are a non-literal form (conservative)"
      (is (not (contains? by-row 51))
          "dynamic-attrs-skipped case — [:img (merge ...)] non-literal attrs"))

    (testing "every finding carries the :img tag as :kw"
      (is (every? #(= :img (:kw %)) img-alt-missing)))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) img-alt-missing)))

    (testing "every finding carries a :col so same-line siblings differ"
      (is (every? #(pos? (:col %)) img-alt-missing))
      (let [row-43-findings (filter #(= 43 (:row %)) img-alt-missing)
            row-43-cols (set (map :col row-43-findings))]
        (is (= 2 (count row-43-cols))
            "the two [:img] on row 43 have distinct :col values")))))
