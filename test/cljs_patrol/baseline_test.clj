(ns cljs-patrol.baseline-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.test :refer [deftest is]]))

(deftest issue->identity-keyword-keyed
  (is (= {:rule :unused-subs :key :app.subs/users}
         (baseline/issue->identity :unused-subs
                                   {:kw :app.subs/users :type :sub
                                    :file "src/app/subs.cljs" :row 10}))
      "unused-subs uses rule + key")
  (is (= {:rule :unused-events :key :app.events/init}
         (baseline/issue->identity :unused-events
                                   {:kw :app.events/init :type :event
                                    :file "src/app/events.cljs" :row 5}))
      "unused-events uses rule + key")
  (is (= {:rule :phantom-subs :key :app.subs/missing}
         (baseline/issue->identity :phantom-subs
                                   {:kw :app.subs/missing :type :sub
                                    :file "src/app/views.cljs" :row 20}))
      "phantom-subs uses rule + key")
  (is (= {:rule :phantom-events :key :app.events/ghost}
         (baseline/issue->identity :phantom-events
                                   {:kw :app.events/ghost :type :event
                                    :file "src/app/handlers.cljs" :row 30}))
      "phantom-events uses rule + key"))

(deftest issue->identity-duplicate-rules
  (is (= {:rule :duplicate-subs :key :app.subs/users :file "src/app/subs.cljs"}
         (baseline/issue->identity :duplicate-subs
                                   {:kw :app.subs/users :type :sub
                                    :file "src/app/subs.cljs" :row 10}))
      "duplicate-subs uses rule + key + file")
  (is (= {:rule :duplicate-events :key :app.events/init :file "src/app/events.cljs"}
         (baseline/issue->identity :duplicate-events
                                   {:kw :app.events/init :type :event
                                    :file "src/app/events.cljs" :row 5}))
      "duplicate-events uses rule + key + file"))

(deftest issue->identity-var-keyed
  (is (= {:rule :unused-styles :ns "app.ui" :var "container-style"}
         (baseline/issue->identity :unused-styles
                                   {:kw :app.ui/container-style :type :defclass
                                    :file "src/app/ui.cljs" :row 8}))
      "unused-styles uses rule + ns + var")
  (is (= {:rule :defattrs-in-merge :ns "app.ui" :var "box-attrs"}
         (baseline/issue->identity :defattrs-in-merge
                                   {:kw :app.ui/box-attrs :type :defattrs
                                    :file "src/app/ui.cljs" :row 12}))
      "defattrs-in-merge uses rule + ns + var")
  (is (= {:rule :defclass-as-sole-attr :ns "app.ui" :var "btn-class"}
         (baseline/issue->identity :defclass-as-sole-attr
                                   {:kw :app.ui/btn-class :type :defclass
                                    :file "src/app/ui.cljs" :row 15}))
      "defclass-as-sole-attr uses rule + ns + var")
  (is (= {:rule :mixed-token-groups :ns "app.styles" :var "heading"}
         (baseline/issue->identity :mixed-token-groups
                                   {:decl-kw :app.styles/heading
                                    :prefixes #{"action-small" "body-short-small"}
                                    :file "src/app/styles.cljs" :row 20}))
      "mixed-token-groups uses :decl-kw for rule + ns + var"))

(deftest issue->identity-site-rules
  (is (= {:rule :deprecated-effects :effect ":dispatch-n"
          :file "src/app/events.cljs" :line 42}
         (baseline/issue->identity :deprecated-effects
                                   {:type :deprecated :effect ":dispatch-n"
                                    :form ":dispatch-n"
                                    :file "src/app/events.cljs" :row 42}))
      "deprecated-effects uses rule + effect + file + line")
  (is (= {:rule :dynamic-sites :form "(rf/dispatch [ev])"
          :file "src/app/handlers.cljs" :line 55}
         (baseline/issue->identity :dynamic-sites
                                   {:form "(rf/dispatch [ev])"
                                    :file "src/app/handlers.cljs" :row 55}))
      "dynamic-sites uses rule + form + file + line"))

(deftest issue->identity-unknown-rule
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown rule"
        (baseline/issue->identity :bogus-rule {:kw :foo}))))

(deftest issue->identity-ignores-volatile-fields
  (let [id1 (baseline/issue->identity :unused-subs
                                      {:kw :app/foo :type :sub :file "a.cljs" :row 1})
        id2 (baseline/issue->identity :unused-subs
                                      {:kw :app/foo :type :sub :file "a.cljs" :row 99})]
    (is (= id1 id2) "line number changes don't affect keyword-keyed identity"))
  (let [id1 (baseline/issue->identity :unused-subs
                                      {:kw :app/foo :type :sub :file "old.cljs" :row 1})
        id2 (baseline/issue->identity :unused-subs
                                      {:kw :app/foo :type :sub :file "new.cljs" :row 1})]
    (is (= id1 id2) "file changes don't affect keyword-keyed identity")))

(deftest result->identities
  (let [result {:unused-subs [{:kw :app/a :type :sub :file "a.cljs" :row 1}
                              {:kw :app/b :type :sub :file "b.cljs" :row 2}]
                :phantom-events [{:kw :app/c :type :event :file "c.cljs" :row 3}]
                :dynamic-sites [{:form "(rf/dispatch [x])" :file "d.cljs" :row 4}]}]
    (is (= #{{:rule :unused-subs :key :app/a}
             {:rule :unused-subs :key :app/b}
             {:rule :phantom-events :key :app/c}
             {:rule :dynamic-sites :form "(rf/dispatch [x])" :file "d.cljs" :line 4}}
           (baseline/result->identities :re-frame result))))
  (is (= #{} (baseline/result->identities :re-frame {:unused-subs [] :phantom-events []}))
      "empty result returns empty set"))
