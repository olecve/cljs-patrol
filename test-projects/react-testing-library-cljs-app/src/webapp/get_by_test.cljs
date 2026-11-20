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

(defn- get-item-1 [] (s/get-by-role "radio" {:name "Item 1"}))

(defn- get-rows [] (s/get-all-by-role "row"))

(defn- query-item-2 [] (s/query-by-role "radio" {:name "Item 2"}))

(defn- get-checked-item [] (.-checked (s/get-by-role "radio" {:checked true})))

(defn- get-alert-text []
  (s/get-by-role "alert")
  (s/query-by-text "Saved"))

(deftest bad-some-get-by-helper
  (is (some? (get-item-1))))

(deftest bad-seq-get-all-by-helper
  (is (seq (get-rows))))

(deftest ok-query-by-helper
  (is (some? (query-item-2))))

(deftest ok-helper-reading-the-lookup
  (is (some? (get-checked-item))))

(deftest ok-helper-returning-something-else
  (is (some? (get-alert-text))))

(deftest ok-get-by-helper-acted-on
  (.click (get-item-1)))

(deftest ok-let-shadows-get-by-helper
  (let [get-item-1 (constantly nil)]
    (is (some? (get-item-1)))))

(do
  (defn- get-nested-dialog [] (s/get-by-role "dialog")))

(defn- get-redefined [] (s/get-by-role "dialog"))

(defn- get-redefined [] (.-checked (s/get-by-role "dialog")))

(deftest ok-helper-nested-in-do
  (is (some? (get-nested-dialog))))

(deftest ok-helper-defined-twice
  (is (some? (get-redefined))))

(defn- get-replaced-by-def [] (s/get-by-role "dialog"))

(def get-replaced-by-def (constantly nil))

(comment
  (defn- get-in-repl [] (s/get-by-role "dialog")))

(def quoted-helper '(defn- get-quoted [] (s/get-by-role "dialog")))

(deftest ok-helper-replaced-by-def
  (is (some? (get-replaced-by-def))))

(deftest ok-helper-only-in-comment
  (is (some? (get-in-repl))))

(deftest ok-helper-only-quoted
  (is (some? (get-quoted))))

(defn- get-redefined-in-do [] (s/get-by-role "dialog"))

(do
  (defn- get-redefined-in-do [] nil))

(defn- get-redefined-by-defonce [] (s/get-by-role "dialog"))

(defonce get-redefined-by-defonce nil)

(deftest ok-helper-redefined-in-do
  (is (some? (get-redefined-in-do))))

(deftest ok-helper-redefined-by-defonce
  (is (some? (get-redefined-by-defonce))))

(defn- get-with-discarded-copy [] (s/get-by-role "dialog"))

#_(defn- get-with-discarded-copy [] nil)

(deftest bad-some-helper-with-discarded-copy
  (is (some? (get-with-discarded-copy))))

(defn- get-passed-to-default [] (s/get-by-role "dialog"))

(default-handler get-passed-to-default)

(deftest bad-some-helper-also-passed-to-a-call
  (is (some? (get-passed-to-default))))

(deftest ok-helper-name-in-vector
  (is [get-item-1]))

(defn- get-also-a-test [] (s/get-by-role "dialog"))

(deftest get-also-a-test
  (is (some? (get-also-a-test))))
