(ns cljs-patrol.integration.testing-library-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.testing-library :as testing-library]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test-projects/testing-library-app/src/webapp")

(defn- result []
  (-> (core/run fixture-dir [testing-library/group]) :group-results first))

(defn- fn-names-of
  "Name the deftest each finding sits in, as the fixture's `:test` records it."
  [findings]
  (set (map :test findings)))

(deftest get-by-presence-check-fixture-test
  (let [findings (:get-by-presence-check (result))]
    (is (= #{"bad-some-get-by-role"
             "bad-some-get-by-text-with-message"
             "bad-bare-get-by-label-text"
             "bad-not-nil-get-by-title"
             "bad-boolean-get-by-role"
             "bad-seq-get-all-by-role"
             "bad-not-empty-get-all-by-text"
             "bad-within-get-by-role"
             "bad-referred-get-by-text"
             "bad-fully-qualified-get-by-role"
             "bad-lookup-also-bound-as-value"}
           (fn-names-of findings))
        "only presence check on throwing lookup is reported")
    (is (= #{"Assert presence with s/query-by-role, which returns nil on a miss, or call s/get-by-role outside the is."
             "Assert presence with query-by-text, which returns nil on a miss, or call get-by-text outside the is."}
           (->> findings
                (filter #(contains? #{"bad-some-get-by-role" "bad-referred-get-by-text"} (:test %)))
                (map :hint)
                set))
        "hint names query-by twin under same alias")
    (is (= [(str "Assert presence with (seq (s/query-all-by-role …)), which is empty on a miss, "
                 "or call s/get-all-by-role outside the is.")]
           (->> findings (filter #(= "bad-seq-get-all-by-role" (:test %))) (map :hint)))
        "query-all-by returns empty vector, so hint wraps it in seq")))
