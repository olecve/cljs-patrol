(ns shared-ids.statement-render-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [react-testing-library-cljs.reagent.render :refer [render!]]
   [react-testing-library-cljs.screen :as s]))

(deftest renders-in-a-later-fn
  (let [setup! (fn [] (render! [:button {:data-testid "hook"
                                         :type :button}
                                "Hook"]))]
    (is (some? (s/get-by-test-id "hook")))
    (setup!)))

(deftest renders-in-a-branch
  (when (some? js/document)
    (render! [:button {:data-testid "branch"
                       :type :button}
              "Branch"]))
  (is (some? (s/get-by-test-id "branch"))))

(deftest renders-in-a-clj-branch
  #?(:clj (render! [:button {:data-testid "clj"
                             :type :button}
                    "Clj"]))
  (is (some? (s/get-by-test-id "clj"))))

(deftest renders-in-a-binding
  (let [result (render! [:button {:data-testid "bound"
                                  :type :button}
                         "Bound"])]
    (is (some? result))
    (is (some? (s/get-by-test-id "bound")))))

(deftest renders-in-a-testing-block
  (testing "in a group"
    (render! [:button {:data-testid "grouped"
                       :type :button}
              "Grouped"])
    (is (some? (s/get-by-test-id "grouped")))))

(deftest renders-in-a-try
  (try
    (render! [:button {:data-testid "tried"
                       :type :button}
              "Tried"])
    (is (some? (s/get-by-test-id "tried")))
    (finally
      (js/console.log "done"))))
