(ns cljs-patrol.integration.literal-constant-assertion-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.tests :as tests]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private results
  (delay (->> (core/run-all ["test-projects/literal-constant-app/src" "test-projects/literal-constant-app/test"]
                            [tests/group])
              (map #(-> % :group-results first :literal-constant-assertion)))))

(defn- forms [] (map :form (second @results)))

(deftest literal-constant-assertion-flags-repeated-definition-test
  (is (some #(str/includes? % "messages/file-failed-message") (forms))
      "aliased constant flagged")
  (is (some #(str/includes? % "messages/generic-failed-message \"Smart") (forms))
      "constant first, docstring def flagged")
  (is (some #(str/includes? % "app.messages/support-email") (forms))
      "fully qualified private constant flagged, message or not")
  (is (some #(= "(is (= \"View Results\" view-results-label))" %) (forms))
      "referred constant flagged")
  (is (some #(str/includes? % "local-heading") (forms))
      "constant of test namespace flagged")
  (is (= 5 (count (forms)))
      "nothing else flagged"))

(deftest literal-constant-assertion-judges-own-directory-test
  (is (empty? (first @results))
      "assertion outside test code left alone, constants there still count"))

(deftest literal-constant-assertion-skips-files-without-ns-test
  (is (not-any? #(= "(is (= \"Hello\" heading))" (:form %))
                (-> (core/run-all ["test-projects/literal-constant-app/src" "test-projects/literal-constant-app/test"]
                                  [(tests/make-group {:paths ["test"]})])
                    second :group-results first :literal-constant-assertion))
      "constant of file without ns form left alone, though every such file shares one namespace"))

(deftest literal-constant-assertion-summary-test
  (is (some #{["Literal constant assertion:" 5]}
            (group/summary-lines tests/group {:literal-constant-assertion (second @results)}))
      "summary counts findings"))

(deftest literal-constant-assertion-hint-test
  (is (= (str "file-failed-message is defined as this same string, so the assertion repeats its definition. "
              "Assert on what the code does with it.")
         (->> (second @results) (filter #(str/includes? (:form %) "file-failed-message")) first :hint))
      "hint names constant"))

(deftest literal-constant-assertion-baseline-identity-test
  (is (= {:rule :literal-constant-assertion
          :kind 'is
          :file "app/messages_test.cljs"
          :form "(is (= \"View Results\" view-results-label))"}
         (baseline/issue->identity :literal-constant-assertion
                                   {:kw 'is
                                    :form "(is (= \"View Results\" view-results-label))"
                                    :file "test-projects/literal-constant-app/test/app/messages_test.cljs"
                                    :row 18}
                                   "test-projects/literal-constant-app/test"))
      "keyed by form and file"))
