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
             "bad-lookup-also-bound-as-value"
             "bad-some-get-by-helper"
             "bad-seq-get-all-by-helper"}
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
        "query-all-by returns empty vector, so hint wraps it in seq")
    (is (= {"bad-some-get-by-helper" (str "get-item-1 returns get-by-role, which throws on a miss, so this check never"
                                          " fails. Use a helper that returns query-by-role.")
            "bad-seq-get-all-by-helper" (str "get-rows returns get-all-by-role, which throws on a miss, so this check"
                                             " never fails. Use a helper that returns query-all-by-role, inside seq.")}
           (->> findings
                (filter #(contains? #{"bad-some-get-by-helper" "bad-seq-get-all-by-helper"} (:test %)))
                (map (juxt :test :hint))
                (into {})))
        "hint names helper and query-by lookup it should return")))

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
             "finds-plain-in-span" "finds-plain-past-discard"
             "finds-inner-space" "finds-hidden-false" "finds-list-nil" "finds-in-component"
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

(deftest test-id-query-on-role-reads-what-a-test-may-render-test
  (let [findings (->> (core/run "test-projects/react-testing-library-cljs-app/src/shared_ids"
                                [react-testing-library-cljs/group])
                      :group-results
                      first
                      :test-id-query-on-role
                      (map (juxt :test :form))
                      set)]
    (is (contains? findings ["finds-first-and-last-by-test-id" "(s/get-by-test-id \"first\")"])
        "first button and \"first\" string in other tests' bodies, in this file and another, are not on this test's screen")
    (is (contains? findings ["finds-first-and-last-by-test-id" "(s/get-by-test-id \"last\")"])
        "last button in another file's test body is not on this test's screen, though a runner requires every test")
    (is (contains? findings ["finds-save-in-a-component" "(s/get-by-test-id \"save\")"])
        "element outside every test, in a component the test renders, still counts")
    (is (not-any? #(= "get-open-button" (first %)) findings)
        "lookup outside a test reads the whole run, so the second Open button a test renders keeps it alone")
    (is (not-any? #(= "closes-from-the-toolbar" (first %)) findings)
        "defmethod outside a test body, in a test file, may render in any test")
    (is (not-any? #(= "submits-from-the-second-button" (first %)) findings)
        "second Submit button in the lookup's own test body keeps it alone")
    (is (contains? findings ["confirms-privately" "(s/get-by-test-id \"ok\")"])
        "clojure.test's deftest- is a test too, so another test's OK button is not on its screen")
    (is (contains? findings ["agrees-in-cljs" "(s/get-by-test-id \"yes\")"])
        "test in a reader conditional is a test too, so another test's Yes button is not on its screen")
    (is (contains? findings ["goes-with-metadata" "(s/get-by-test-id \"go\")"])
        "test wrapped in metadata is a test too, so another test's Go button is not on its screen")
    (is (not-any? #(= "keeps-from-the-second-button" (first %)) findings)
        "helper next to a test in a reader conditional is outside it, so its Keep button counts")
    (is (contains? findings ["stores" "(s/get-by-test-id \"store\")"])
        "test of the same name in another reader conditional branch is another test")
    (is (contains? findings ["finds-the-literal-id" "(s/get-by-test-id \"built\")"])
        "id another test's body may build is not on this test's screen")
    (is (not-any? #(= "finds-an-id-it-may-build" (first %)) findings)
        "id the lookup's own test body may build keeps it alone")
    (is (not-any? #(= "draws-without-cleanup" (first %)) findings)
        "test that renders without render! may still show what another test left, so it reads the whole run")
    (is (contains? findings ["checks" "(s/get-by-test-id \"check\")"])
        "test of the same name on the same line, in another reader conditional branch, is another test")
    (is (contains? findings ["nested-check" "(s/get-by-test-id \"nest\")"])
        "test inside a defn is still a test, so another test's body changes nothing")
    (is (not-any? #(= "paints-after-render" (first %)) findings)
        "Paint button another test draws without render! stays on screen, since cleanup leaves it")
    (is (not-any? #(= "looks-before-render" (first %)) findings)
        "lookup before its test's render! still sees what earlier tests left, so it reads the whole run")
    (is (not-any? #(= "renders-only-in-a-discard" (first %)) findings)
        "render! inside #_ never runs, so the test reads the whole run")
    (is (not-any? #(= "renders-only-in-a-comment" (first %)) findings)
        "render! inside (comment …) never runs, so the test reads the whole run")
    (is (empty? (filter #(contains? #{"renders-in-a-later-fn" "renders-in-a-branch" "renders-in-a-clj-branch"} (first %))
                        findings))
        "render! in a fn, a branch or a :clj branch may not run first, so the test reads the whole run")
    (is (contains? findings ["renders-in-a-binding" "(s/get-by-test-id \"bound\")"])
        "render! in a let binding runs first")
    (is (contains? findings ["renders-in-a-testing-block" "(s/get-by-test-id \"grouped\")"])
        "render! in a testing block runs first")
    (is (contains? findings ["renders-in-a-try" "(s/get-by-test-id \"tried\")"])
        "render! in a try body runs first")
    (is (not-any? #(= "looks-inside-the-render" (first %)) findings)
        "lookup in render!'s own arguments runs before it, so the test reads the whole run")))

(deftest test-id-query-on-role-findings-follow-file-and-row-test
  (let [findings (->> (core/run "test-projects/react-testing-library-cljs-app/src/shared_ids"
                                [react-testing-library-cljs/group])
                      :group-results
                      first
                      :test-id-query-on-role)]
    (is (= (distinct (map :file findings)) (dedupe (map :file findings)))
        "findings of one file come together")
    (is (every? #(apply <= (map :row %)) (vals (group-by :file findings)))
        "findings of one file follow its rows")))

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
           "finds-ui-save" "finds-ui-send" "finds-ui-tab" "finds-ui-menu-popover"}
         (set (keys (configured-findings {:test-id-elements {'my.ui/* :div
                                                             'my.ui/button :button}}))))
      (str "an exact key wins over a glob, a call written as a list is no element, and without :unknown-with "
           "every call reads as the element")))

(deftest test-id-elements-role-test
  (is (= {"finds-ui-palette" "Query it by role \"dialog\" and its name."
          "finds-ui-tab-list" "Query it by role \"tablist\" and its name."
          "finds-ui-menu-popover" "Query it by role \"menu\" and its name."
          "finds-ui-tab-text" "Query it by role \"tab\" and its name."
          "finds-ui-native-open" "Query it by role \"dialog\" and its name."}
         (select-keys (configured-findings {:test-id-elements {'my.ui/popover-content {:role "dialog"}
                                                               'my.ui/tab-list {:role :tablist}
                                                               'my.ui/tab {:role "tab"
                                                                           :tag :button}
                                                               'my.ui/native-dialog {:role "dialog"
                                                                                     :tag :dialog}}})
                      ["finds-ui-palette" "finds-ui-unnamed-popover" "finds-ui-tab-list" "finds-ui-menu-popover"
                       "finds-ui-tab-text" "finds-ui-tab-presentation" "finds-ui-hidden-popover"
                       "finds-ui-shared-popover" "finds-ui-native-closed" "finds-ui-native-open"]))
      (str "a configured role wins over its tag and a written :role over both, a keyword role reads as its name, and "
           "an unnamed, hidden, shared or closed element is left alone")))

(deftest test-id-elements-config-errors-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"maps my.ui/button to no tag or role"
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
      "a config that is no map")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":role must be a role a query finds"
                        (react-testing-library-cljs/make-group {:test-id-elements {'my.ui/popover {:role "dialgo"}}}))
      "a role no query finds")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":tag must be a keyword"
                        (react-testing-library-cljs/make-group {:test-id-elements {'my.ui/popover {:role "dialog"
                                                                                                   :tag "div"}}}))
      "a tag that is no keyword beside a role"))
