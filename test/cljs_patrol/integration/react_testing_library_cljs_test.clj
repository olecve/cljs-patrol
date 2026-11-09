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

(deftest test-id-query-on-role-fixture-test
  (let [findings (:test-id-query-on-role (result))]
    (is (= #{"finds-send-button" "finds-save-button" "finds-save-button-within" "finds-home-link" "finds-edit-area"
             "finds-agree-box" "finds-query-input" "finds-find-input" "finds-pick-input" "finds-go-input"
             "finds-country-select" "finds-tags-select" "finds-sized-select" "finds-settings-dialog"
             "finds-all-picked-buttons" "finds-send-button-past-discarded-id" "finds-listed-box" "finds-size-one"
             "finds-plain-in-span" "finds-inner-space" "finds-hidden-false" "finds-list-nil" "finds-in-component"
             "finds-built-key-free" "finds-other-one" "finds-kw-elsewhere"
             "finds-in-open-dialog" "finds-size-zero" "finds-multiple-zero" "finds-mid-a-end" "finds-hidden-nil"
             "finds-with-break" "finds-uneval-child"}
           (fn-names-of findings))
        "a test id on a written-out element with a role and a name, and no other mention of it")
    (is (= {"finds-send-button" "Query it by role \"button\" and its name."
            "finds-home-link" "Query it by role \"link\" and its name."
            "finds-edit-area" "Query it by role \"textbox\" and its name."
            "finds-agree-box" "Query it by role \"checkbox\" and its name."
            "finds-query-input" "Query it by role \"textbox\" and its name."
            "finds-find-input" "Query it by role \"searchbox\" and its name."
            "finds-pick-input" "Query it by role \"combobox\" and its name."
            "finds-go-input" "Query it by role \"button\" and its name."
            "finds-country-select" "Query it by role \"combobox\" and its name."
            "finds-tags-select" "Query it by role \"listbox\" and its name."
            "finds-sized-select" "Query it by role \"listbox\" and its name."
            "finds-settings-dialog" "Query it by role \"dialog\" and its name."
            "finds-listed-box" "Query it by role \"checkbox\" and its name."
            "finds-size-one" "Query it by role \"listbox\" and its name."
            "finds-size-zero" "Query it by role \"combobox\" and its name."
            "finds-multiple-zero" "Query it by role \"combobox\" and its name."}
           (-> (into {} (map (juxt :test :hint)) findings)
               (select-keys ["finds-send-button" "finds-home-link" "finds-edit-area" "finds-agree-box"
                             "finds-query-input" "finds-find-input" "finds-pick-input" "finds-go-input"
                             "finds-country-select" "finds-tags-select" "finds-sized-select"
                             "finds-settings-dialog" "finds-listed-box" "finds-size-one" "finds-size-zero"
                             "finds-multiple-zero"])))
        "hint names the role a query finds the element by")
    (is (every? #(= :cleanup (:tier %)) findings)
        "carries the cleanup tier")))

(deftest test-id-built-by-opaque-call-fixture-test
  (is (empty? (:test-id-query-on-role
               (-> (core/run "test-projects/react-testing-library-cljs-app/src/opaque"
                             [react-testing-library-cljs/group])
                   :group-results
                   first)))
      "a test id built by a call with no literal part may be any id"))

(defn- configured-findings [config]
  (->> (core/run "test-projects/react-testing-library-cljs-app/src/configured"
                 [(react-testing-library-cljs/make-group config)])
       :group-results
       first
       :test-id-query-on-role
       (into {} (map (juxt :test :hint)))))

(deftest test-id-elements-config-test
  (is (= {"finds-ui-send" "Query it by role \"button\" and its name."
          "finds-call-argument" "Query it by role \"button\" and its name."
          "finds-label-text" "Query it by role \"button\" and its name."
          "finds-ui-save" "Query it by role \"button\" and its name."
          "finds-ui-tab" "Query it by role \"tab\" and its name."
          "finds-ui-email" "Query it by role \"textbox\" and its name."
          "finds-ui-toggle" "Query it by role \"button\" and its name."}
         (configured-findings {:test-id-elements {'my.ui/button {:tag :button
                                                                 :unknown-with #{:href :icon-only}}
                                                  'my.ui/text-field :input
                                                  'my.ui/panel :div
                                                  'my.ui/label {:tag :span
                                                                :unknown-with :icon-only}
                                                  'my.widgets/* :button}}))
      "a configured component reads as its element, unless a prop it changes the element with is written")
  (is (= #{"finds-call-argument" "finds-in-unreadable-panel"}
         (set (keys (configured-findings nil))))
      "a component no entry names decides where its test id goes, and hides only through literal props")
  (is (= #{"finds-call-argument" "finds-label-icon-only" "finds-label-text" "finds-ui-icon" "finds-ui-link"
           "finds-ui-save" "finds-ui-send" "finds-ui-tab"}
         (set (keys (configured-findings {:test-id-elements {'my.ui/* :div
                                                             'my.ui/button :button}}))))
      (str "an exact key wins over a glob, a call written as a list is no element, and without :unknown-with "
           "every call reads as the element")))

(deftest test-id-elements-config-errors-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"maps my.ui/button to no tag"
                        (react-testing-library-cljs/make-group {:test-id-elements {'my.ui/button {:unknown-with :href}}}))
      "an entry without a tag")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":unknown-with must be a keyword"
                        (react-testing-library-cljs/make-group {:test-id-elements {'my.ui/button {:tag :button
                                                                                                  :unknown-with "href"}}}))
      "an :unknown-with that is no keyword")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":unknown-with must be a keyword"
                        (react-testing-library-cljs/make-group {:test-id-elements {'my.ui/button {:tag :button
                                                                                                  :unknown-with ["href"]}}}))
      "an :unknown-with collection holding no keyword")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"keys must be fully qualified symbols"
                        (react-testing-library-cljs/make-group {:test-id-elements {"my.ui/button" :button}}))
      "a string key")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"keys must be fully qualified symbols"
                        (react-testing-library-cljs/make-group {:test-id-elements {'button :button}}))
      "a symbol key with no namespace")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be a map"
                        (react-testing-library-cljs/make-group {:test-id-elements [:button]}))
      "a config that is no map"))
