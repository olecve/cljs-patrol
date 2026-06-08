(ns cljs-patrol.groups.docstrings-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.docstrings :as docstrings]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/docstrings-app/src/myapp")

(defn- kws [items]
  (set (map :kw items)))

(deftest analyze-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [docstrings/group])
        result (first group-results)
        summary-kws (kws (:docstring-summary result))
        indent-kws (kws (:docstring-indentation result))
        whitespace-kws (kws (:docstring-leading-trailing-whitespace result))]

    (testing "summary rule flags missing period"
      (is (contains? summary-kws :myapp.core/summary-fail-no-period)))

    (testing "summary rule flags second sentence on same line as first"
      (is (contains? summary-kws :myapp.core/summary-fail-extra-prose)))

    (testing "summary rule ignores single-line docstrings missing a period"
      (is (not (contains? summary-kws :myapp.core/single-line-no-period))))

    (testing "summary rule passes well-formed multi-line docstring"
      (is (not (contains? summary-kws :myapp.core/good-multiline))))

    (testing "summary rule does not flag '?' inside Clojure identifiers"
      (is (not (contains? summary-kws :myapp.core/identifier-with-question-mark))))

    (testing "summary rule does not flag name-title abbreviations (Mr., Dr., etc.)"
      (is (not (contains? summary-kws :myapp.core/pass-name-title-abbreviation))))

    (testing "summary rule does not flag place-name abbreviations (St., etc.)"
      (is (not (contains? summary-kws :myapp.core/pass-place-abbreviation))))

    (testing "summary rule still flags transitional abbreviations (e.g., i.e.)"
      (is (contains? summary-kws :myapp.core/fail-eg-transition)))

    (testing "indentation rule flags shallow continuation lines"
      (is (contains? indent-kws :myapp.core/indent-fail)))

    (testing "indentation rule passes aligned continuation lines"
      (is (not (contains? indent-kws :myapp.core/good-multiline))))

    (testing "leading/trailing rule flags leading whitespace"
      (is (contains? whitespace-kws :myapp.core/whitespace-fail-leading)))

    (testing "leading/trailing rule flags trailing whitespace"
      (is (contains? whitespace-kws :myapp.core/whitespace-fail-trailing)))

    (testing "docstring starting with newline is reported once, not double-counted"
      (is (contains? whitespace-kws :myapp.core/whitespace-leading-newline))
      (is (not (contains? indent-kws :myapp.core/whitespace-leading-newline))
          "indentation should be suppressed when leading-whitespace already fires"))

    (testing "private vars are also checked"
      (is (contains? summary-kws :myapp.core/private-meta))
      (is (contains? summary-kws :myapp.core/private-defn-dash)))

    (testing "compliant private docstrings pass"
      (is (not (contains? summary-kws :myapp.core/compliant-private)))
      (is (not (contains? indent-kws :myapp.core/compliant-private))))

    (testing "def with no third arg treats the string as a value, not a docstring"
      (is (not (contains? summary-kws :myapp.core/value-only))))

    (testing "def with three args recognizes the docstring"
      (is (not (contains? summary-kws :myapp.core/documented-value))
          "compliant docstring should not flag"))

    (testing "defmulti docstring is checked"
      (is (not (contains? summary-kws :myapp.core/dispatcher))
          "compliant defmulti docstring should not flag"))

    (testing "defprotocol outer docstring is checked"
      (is (not (contains? summary-kws :myapp.core/Greeter))
          "compliant outer protocol docstring should not flag"))

    (testing "defprotocol method docstrings are also checked"
      (is (contains? summary-kws :myapp.core/greet)
          "method with bad summary should be flagged"))

    (testing "compliant defprotocol method docstrings pass"
      (is (not (contains? summary-kws :myapp.core/farewell))))))

(deftest analyze-filters-by-type-test
  (testing "analyze partitions usages into the three rule buckets"
    (let [usages [{:type :docstring-summary
                   :kw :a/x
                   :file "f"
                   :row 1}
                  {:type :docstring-indentation
                   :kw :a/y
                   :file "f"
                   :row 2}
                  {:type :docstring-leading-trailing-whitespace
                   :kw :a/z
                   :file "f"
                   :row 3}]
          result (group/analyze docstrings/group {:usages usages})]
      (is (= [:a/x] (map :kw (:docstring-summary result))))
      (is (= [:a/y] (map :kw (:docstring-indentation result))))
      (is (= [:a/z] (map :kw (:docstring-leading-trailing-whitespace result)))))))

(deftest failed?-test
  (testing "docstring rules never block CI by themselves"
    (is (not (group/failed? docstrings/group
                            {:docstring-summary [{:kw :a/x}]
                             :docstring-indentation [{:kw :a/y}]
                             :docstring-leading-trailing-whitespace [{:kw :a/z}]})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines docstrings/group
                                   {:docstring-summary [{:kw :a/x}]
                                    :docstring-indentation [{:kw :a/y} {:kw :a/z}]
                                    :docstring-leading-trailing-whitespace []})]
    (is (= 3 (count lines)))
    (is (= 1 (second (nth lines 0))))
    (is (= 2 (second (nth lines 1))))
    (is (= 0 (second (nth lines 2))))))
