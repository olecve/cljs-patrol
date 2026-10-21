(ns webapp.get-by-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.screen :as s]
   [react-testing-library-cljs.screen :refer [get-by-role]]
   [react-testing-library-cljs.within :as within :refer [get-by-text]]
   [webapp.helpers :as helpers]))

(deftest bad-some-get-by-role
  (is (some? (s/get-by-role "button" {:name "Save"}))))

(deftest bad-some-get-by-text-with-message
  (is (some? (s/get-by-text "Saved"))
      "toast shows"))

(deftest bad-bare-get-by-label-text
  (is (s/get-by-label-text "Email")))

(deftest bad-not-nil-get-by-title
  (is (not (nil? (s/get-by-title "Close")))))

(deftest bad-boolean-get-by-role
  (is (boolean (s/get-by-role "dialog"))))

(deftest bad-seq-get-all-by-role
  (is (seq (s/get-all-by-role "row"))))

(deftest bad-not-empty-get-all-by-text
  (is (not-empty (s/get-all-by-text "Item"))))

(deftest bad-within-get-by-role [container]
  (is (some? (within/get-by-role container "button"))))

(deftest bad-referred-get-by-text [container]
  (is (some? (get-by-text container "Saved"))))

(deftest ok-query-by-role
  (is (some? (s/query-by-role "button" {:name "Save"}))))

(deftest ok-nil-query-by-role
  (is (nil? (s/query-by-role "button" {:name "Delete"}))))

(deftest ok-get-by-content
  (is (= "Saved" (.-textContent (s/get-by-role "status")))))

(deftest ok-get-all-by-count
  (is (= 2 (count (s/get-all-by-role "row")))))

(deftest ok-get-by-outside-is
  (s/get-by-role "button" {:name "Save"}))

(deftest ok-property-of-get-by
  (is (some? (.-parentElement (s/get-by-role "button")))))

(deftest ok-other-namespace-get-by
  (is (some? (helpers/get-by-role "button"))))

(deftest ok-unresolved-get-by
  (is (some? (get-by-title "Close"))))

(deftest ok-some-with-two-arguments
  (is (some? (s/get-by-role "button") :extra)))

(deftest ok-presence-outside-assertion
  (do (some? (s/get-by-role "dialog"))))

(deftest ok-negated-presence
  (is (not (some? (s/get-by-role "dialog")))))

(deftest bad-fully-qualified-get-by-role
  (is (some? (react-testing-library-cljs.screen/get-by-role "button"))))

(deftest bad-lookup-also-bound-as-value
  (let [button (s/get-by-role "button")]
    (is (some? (s/get-by-role "button")))
    (is (= "Save" (.-textContent button)))))

(deftest ok-let-shadows-referred-lookup
  (let [get-by-role (fn [& _] nil)]
    (is (some? (get-by-role "button")))))

(deftest ok-param-shadows-referred-lookup [get-by-role]
  (is (some? (get-by-role "button"))))

(deftest ok-letfn-shadows-referred-lookup
  (letfn [(get-by-role [_] nil)]
    (is (some? (get-by-role "button")))))

(deftest ok-with-redefs-of-lookup
  (with-redefs [s/get-by-role (constantly nil)]
    (is (some? (s/get-by-role "button")))))

(deftest ok-shadowed-is
  (let [is (fn [x] x)]
    (is (some? (s/get-by-role "button")))))

(deftest ok-nil-with-extra-argument
  (is (not (nil? (s/get-by-role "button") 1))))
