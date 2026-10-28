(ns app.messages-test
  (:require
   [app.messages :as messages :refer [view-results-label]]
   [cljs.test :refer [deftest is]]))

(def local-heading "Local heading")

(deftest aliased-constant
  (is (= "We couldn't read this file" messages/file-failed-message)))

(deftest reversed-order
  (is (= messages/generic-failed-message "Smart search failed")))

(deftest fully-qualified
  (is (= "support@example.com" app.messages/support-email)
      "support address"))

(deftest referred-constant
  (is (= "View Results" view-results-label)))

(deftest same-namespace-constant
  (is (= "Local heading" local-heading)))

(deftest different-literal
  (is (= "We could not read this file" messages/file-failed-message)))

(deftest computed-constant
  (is (= "Smart Search completed" messages/completed-message)))

(deftest reader-conditional-constant
  (is (= "Browser" messages/platform-label)))

(deftest fn-output
  (is (= "Search case \"Untitled\" is ready for review" (messages/completed-description nil))))

(deftest shadowed-by-local
  (let [view-results-label "View Results"]
    (is (= "View Results" view-results-label))))

(deftest three-way-comparison
  (is (= "View Results" messages/view-results-label "View Results")))

(deftest not-equal
  (is (not= "View Results" messages/view-results-label)))

(deftest compile-time-define
  (is (= "localhost" messages/api-host)))

(deftest keyword-destructuring
  (let [{:keys [:view-results-label]} {:view-results-label "View Results"}]
    (is (= "View Results" view-results-label))))

(deftest qualified-destructuring
  (let [{:keys [messages/view-results-label]} {:app.messages/view-results-label "View Results"}]
    (is (= "View Results" view-results-label))))

(deftest redefined-constant
  (with-redefs [messages/view-results-label "View Results"]
    (is (= "View Results" messages/view-results-label))))

(deftest rebound-constant
  (binding [messages/*greeting* "Hello"]
    (is (= "Hello" messages/*greeting*))))

(deftest nested-comment
  (comment
    (is (= "View Results" messages/view-results-label))))

(deftest written-out-quote
  (quote (is (= "View Results" messages/view-results-label))))

(deftest discarded
  #_(is (= "View Results" messages/view-results-label)))

(deftest conflicting-definitions
  (is (= "Cljs" messages/duplicate-label)))

(deftest unrecorded-values
  (is (= "Old" messages/legacy-label))
  (is (= "View Results" messages/label-pattern))
  (is (= "View Results" messages/js-config))
  (is (= "" messages/unbound)))

(deftest outside-is
  (is (not (= "View Results" messages/view-results-label))))

(comment
  (is (= "We couldn't read this file" messages/file-failed-message)))

(deftest unknown-alias
  (is (= "We couldn't read this file" msgs/file-failed-message)))
