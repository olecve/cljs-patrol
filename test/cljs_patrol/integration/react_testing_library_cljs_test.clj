(ns cljs-patrol.integration.react-testing-library-cljs-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.react-testing-library-cljs :as react-testing-library-cljs]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test-projects/react-testing-library-cljs-app/src/webapp")

(defn- result []
  (-> (core/run fixture-dir [react-testing-library-cljs/group]) :group-results first))

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
    (is (= #{"s/get-by-role throws on a miss, so this check never fails. Use s/query-by-role."
             "get-by-text throws on a miss, so this check never fails. Use query-by-text."}
           (->> findings
                (filter #(contains? #{"bad-some-get-by-role" "bad-referred-get-by-text"} (:test %)))
                (map :hint)
                set))
        "hint names query-by twin under same alias")
    (is (= ["s/get-all-by-role throws on a miss, so this check never fails. Use (seq (s/query-all-by-role …))."]
           (->> findings (filter #(= "bad-seq-get-all-by-role" (:test %))) (map :hint)))
        "query-all-by returns empty vector, so hint wraps it in seq")))

(deftest lookup-helper-name-fixture-test
  (let [findings (:lookup-helper-name (result))]
    (is (= {"master-prompt" "Rename to get-master-prompt, after the get-by-role lookup it returns."
            "query-save-button" "Rename to get-save-button, after the get-by-role lookup it returns."
            "get-toast" "Rename to query-toast, after the query-by-text lookup it returns."
            "loaded-panel" "Rename to find-loaded-panel, after the find-by-role lookup it returns."
            "rows" "Rename to get-rows, after the get-all-by-role lookup it returns."
            "referred-hint" "Rename to query-referred-hint, after the query-by-text lookup it returns."
            "radio-in-group" "Rename to get-radio-in-group, after the get-by-role lookup it returns."
            "documented-button" "Rename to get-documented-button, after the get-by-role lookup it returns."
            "get-spinner" "Rename to find-spinner, after the find-by-role lookup it returns."
            "query-menu" "Rename to find-menu, after the find-by-role lookup it returns."
            "hinted-params" "Rename to get-hinted-params, after the get-by-role lookup it returns."
            "with-discard" "Rename to get-with-discard, after the get-by-role lookup it returns."
            "with-attr-map" "Rename to get-with-attr-map, after the get-by-role lookup it returns."
            "in-comment" "Rename to get-in-comment, after the get-by-role lookup it returns."}
           (into {} (map (juxt (comp str :kw) :hint)) findings))
        "only helper returning lookup under wrong prefix is reported, with its rename")))
