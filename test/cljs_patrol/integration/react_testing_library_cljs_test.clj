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

(deftest lookup-helper-name-fixture-test
  (let [findings (:lookup-helper-name (result))]
    (is (= {"master-prompt" "Rename it get-master-prompt, after the get-by-role lookup it returns."
            "query-save-button" "Rename it get-save-button, after the get-by-role lookup it returns."
            "get-toast" "Rename it query-toast, after the query-by-text lookup it returns."
            "loaded-panel" "Rename it find-loaded-panel, after the find-by-role lookup it returns."
            "rows" "Rename it get-rows, after the get-all-by-role lookup it returns."
            "referred-hint" "Rename it query-referred-hint, after the query-by-text lookup it returns."
            "radio-in-group" "Rename it get-radio-in-group, after the get-by-role lookup it returns."
            "documented-button" "Rename it get-documented-button, after the get-by-role lookup it returns."
            "get-spinner" "Rename it find-spinner, after the find-by-role lookup it returns."
            "query-menu" "Rename it find-menu, after the find-by-role lookup it returns."
            "hinted-params" "Rename it get-hinted-params, after the get-by-role lookup it returns."
            "with-discard" "Rename it get-with-discard, after the get-by-role lookup it returns."
            "with-attr-map" "Rename it get-with-attr-map, after the get-by-role lookup it returns."
            "in-comment" "Rename it get-in-comment, after the get-by-role lookup it returns."}
           (into {} (map (juxt (comp str :kw) :hint)) findings))
        "only helper returning lookup under wrong prefix is reported, with its rename")))
