(ns cljs-patrol.groups.typography-test
  (:require
   [cljs-patrol.groups.typography :as typography]
   [clojure.test :refer [deftest is testing]]))

(defn- token-ref [decl-kw token-name]
  {:type :style-token-ref
   :decl-kw decl-kw
   :token-name token-name
   :file "styles.cljs"
   :row 1})

(def ^:private kw :webapp.styles/my-style)

(deftest analyze-test
  (testing "no usages — no violations"
    (is (empty? (:mixed-token-groups (typography/analyze {:usages []})))))

  (testing "ignores non-typography usages"
    (let [other {:type :style-call :kw kw :file "f.cljs" :row 1}
          result (typography/analyze {:usages [other]})]
      (is (empty? (:mixed-token-groups result)))))

  (testing "single token group — no violation"
    (let [result (typography/analyze
                  {:usages [(token-ref kw "action-small-font")
                            (token-ref kw "action-small-letter-spacing")
                            (token-ref kw "action-small-text-case")]})]
      (is (empty? (:mixed-token-groups result)))))

  (testing "mixed token groups — violation"
    (let [result (typography/analyze
                  {:usages [(token-ref kw "action-small-font")
                            (token-ref kw "action-large-letter-spacing")]})]
      (is (= 1 (count (:mixed-token-groups result))))
      (is (= kw (:decl-kw (first (:mixed-token-groups result)))))
      (is (= #{"action-small" "action-large"} (:prefixes (first (:mixed-token-groups result)))))))

  (testing "non-typography tokens are ignored when checking prefixes"
    (let [result (typography/analyze
                  {:usages [(token-ref kw "action-small-font")
                            (token-ref kw "gray-700")]})]
      (is (empty? (:mixed-token-groups result)))))

  (testing "violations are per declaration"
    (let [kw-b :webapp.styles/other-style
          result (typography/analyze
                  {:usages [(token-ref kw "action-small-font")
                            (token-ref kw "action-large-letter-spacing")
                            (token-ref kw-b "body-short-small-font")
                            (token-ref kw-b "body-short-small-letter-spacing")]})]
      (is (= 1 (count (:mixed-token-groups result))))
      (is (= kw (:decl-kw (first (:mixed-token-groups result))))))))

(deftest failed?-test
  (testing "fails when mixed token groups exist"
    (is (typography/failed? {:mixed-token-groups [{:decl-kw kw}]})))

  (testing "does not fail when no violations"
    (is (not (typography/failed? {:mixed-token-groups []})))))

(deftest summary-lines-test
  (let [lines (typography/summary-lines {:mixed-token-groups [{:decl-kw kw} {:decl-kw kw}]})]
    (is (= 1 (count lines)))
    (is (= 2 (second (first lines))))))
