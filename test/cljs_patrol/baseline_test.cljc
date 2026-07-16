(ns cljs-patrol.baseline-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.fs :as fs]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]))

(deftest issue->identity-test
  (testing "keyword-keyed rules use rule + key"
    (is (= {:rule :unused-subs
            :key :app.subs/users}
           (baseline/issue->identity :unused-subs
                                     {:kw :app.subs/users
                                      :type :sub
                                      :file "src/app/subs.cljs"
                                      :row 10}))
        "unused-subs")
    (is (= {:rule :unused-events
            :key :app.events/init}
           (baseline/issue->identity :unused-events
                                     {:kw :app.events/init
                                      :type :event
                                      :file "src/app/events.cljs"
                                      :row 5}))
        "unused-events")
    (is (= {:rule :phantom-subs
            :key :app.subs/missing}
           (baseline/issue->identity :phantom-subs
                                     {:kw :app.subs/missing
                                      :type :sub
                                      :file "src/app/views.cljs"
                                      :row 20}))
        "phantom-subs")
    (is (= {:rule :phantom-events
            :key :app.events/ghost}
           (baseline/issue->identity :phantom-events
                                     {:kw :app.events/ghost
                                      :type :event
                                      :file "src/app/handlers.cljs"
                                      :row 30}))
        "phantom-events"))

  (testing "duplicate rules use rule + key (file not included)"
    (is (= {:rule :duplicate-subs
            :key :app.subs/users}
           (baseline/issue->identity :duplicate-subs
                                     {:kw :app.subs/users
                                      :type :sub
                                      :file "src/app/subs.cljs"
                                      :row 10}))
        "duplicate-subs")
    (is (= {:rule :duplicate-events
            :key :app.events/init}
           (baseline/issue->identity :duplicate-events
                                     {:kw :app.events/init
                                      :type :event
                                      :file "src/app/events.cljs"
                                      :row 5}))
        "duplicate-events"))

  (testing "var-keyed rules use rule + ns + var"
    (is (= {:rule :unused-styles
            :ns "app.ui"
            :var "container-style"}
           (baseline/issue->identity :unused-styles
                                     {:kw :app.ui/container-style
                                      :type :defclass
                                      :file "src/app/ui.cljs"
                                      :row 8}))
        "unused-styles")
    (is (= {:rule :defattrs-in-merge
            :ns "app.ui"
            :var "box-attrs"}
           (baseline/issue->identity :defattrs-in-merge
                                     {:kw :app.ui/box-attrs
                                      :type :defattrs
                                      :file "src/app/ui.cljs"
                                      :row 12}))
        "defattrs-in-merge")
    (is (= {:rule :defclass-as-sole-attr
            :ns "app.ui"
            :var "btn-class"}
           (baseline/issue->identity :defclass-as-sole-attr
                                     {:kw :app.ui/btn-class
                                      :type :defclass
                                      :file "src/app/ui.cljs"
                                      :row 15}))
        "defclass-as-sole-attr")
    (is (= {:rule :mixed-token-groups
            :ns "app.styles"
            :var "heading"}
           (baseline/issue->identity :mixed-token-groups
                                     {:decl-kw :app.styles/heading
                                      :prefixes #{"action-small" "body-short-small"}
                                      :file "src/app/styles.cljs"
                                      :row 20}))
        "mixed-token-groups uses :decl-kw"))

  (testing "site rules use rule + file + line"
    (is (= {:rule :deprecated-effects
            :effect ":dispatch-n"
            :file "src/app/events.cljs"
            :line 42}
           (baseline/issue->identity :deprecated-effects
                                     {:type :deprecated
                                      :effect ":dispatch-n"
                                      :form ":dispatch-n"
                                      :file "src/app/events.cljs"
                                      :row 42}))
        "deprecated-effects")
    (is (= {:rule :pseudo-in-main-map
            :ns "app.ui"
            :var "menu-item-style"
            :selector ":&:hover"}
           (baseline/issue->identity :pseudo-in-main-map
                                     {:kw :app.ui/menu-item-style
                                      :type :pseudo-in-main-map
                                      :selector ":&:hover"
                                      :form ":app.ui/menu-item-style :&:hover"
                                      :file "src/app/ui.cljs"
                                      :row 12}))
        "pseudo-in-main-map identifies by ns + var + selector")
    (is (= {:rule :consecutive-self-selectors
            :ns "app.ui"
            :var "badge-marker-attrs"
            :selectors ":&:before,:&:after"}
           (baseline/issue->identity :consecutive-self-selectors
                                     {:kw :app.ui/badge-marker-attrs
                                      :type :consecutive-self-selectors
                                      :selectors [":&:before" ":&:after"]
                                      :form ":app.ui/badge-marker-attrs [:&:before :&:after]"
                                      :file "src/app/ui.cljs"
                                      :row 20}))
        "consecutive-self-selectors identifies by ns + var + joined selectors")
    (is (= {:rule :dynamic-sites
            :form "(rf/dispatch [ev])"
            :file "src/app/handlers.cljs"
            :line 55}
           (baseline/issue->identity :dynamic-sites
                                     {:form "(rf/dispatch [ev])"
                                      :file "src/app/handlers.cljs"
                                      :row 55}))
        "dynamic-sites")
    (is (= {:rule :dynamic-sites
            :form "(dispatch [on-change {:tab tab :sort sort}])"
            :file "src/views.cljs"
            :line 10}
           (baseline/issue->identity :dynamic-sites
                                     {:form "(dispatch [on-change {:tab tab\n                            :sort sort}])"
                                      :file "src/views.cljs"
                                      :row 10}))
        "multiline form collapses to single line")
    (is (= {:rule :img-alt-missing
            :tag :img
            :file "src/views.cljs"
            :form "[:img {:src \"/a.png\"}]"}
           (baseline/issue->identity :img-alt-missing
                                     {:type :img-alt-missing
                                      :kw :img
                                      :form "[:img {:src \"/a.png\"}]"
                                      :file "src/views.cljs"
                                      :row 12
                                      :col 5}))
        "img-alt-missing keyed by tag + file + form; line/col recorded but not in identity")
    (let [same-form (fn [row col]
                      (baseline/issue->identity :img-alt-missing
                                                {:kw :img
                                                 :form "[:img {:src \"/a.png\"}]"
                                                 :file "views.cljs"
                                                 :row row
                                                 :col col}))]
      (is (= (same-form 12 5) (same-form 30 5))
          "identity survives line shifts from reformatting")
      (is (= (same-form 12 5) (same-form 12 30))
          "identity survives column shifts from reformatting"))
    (let [form-a (baseline/issue->identity :img-alt-missing
                                           {:kw :img
                                            :form "[:img {:src \"/a.png\"}]"
                                            :file "views.cljs"
                                            :row 12
                                            :col 5})
          form-b (baseline/issue->identity :img-alt-missing
                                           {:kw :img
                                            :form "[:img {:src \"/b.png\"}]"
                                            :file "views.cljs"
                                            :row 12
                                            :col 5})]
      (is (not= form-a form-b)
          "distinct form snippets get distinct identities"))
    (is (= {:rule :missing-accessible-name
            :tag :textarea
            :file "src/views.cljs"
            :form "[:textarea {:placeholder \"…\"}]"}
           (baseline/issue->identity :missing-accessible-name
                                     {:type :missing-accessible-name
                                      :kw :textarea
                                      :form "[:textarea {:placeholder \"…\"}]"
                                      :file "src/views.cljs"
                                      :row 42
                                      :col 3}))
        "missing-accessible-name keyed by tag + file + form"))

  (testing "unknown rule throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) #"Unknown rule"
                          (baseline/issue->identity :bogus-rule {:kw :foo}))))

  (testing "ignores volatile fields"
    (let [id1 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo
                                         :type :sub
                                         :file "a.cljs"
                                         :row 1})
          id2 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo
                                         :type :sub
                                         :file "a.cljs"
                                         :row 99})]
      (is (= id1 id2)
          "line number change"))
    (let [id1 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo
                                         :type :sub
                                         :file "old.cljs"
                                         :row 1})
          id2 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo
                                         :type :sub
                                         :file "new.cljs"
                                         :row 1})]
      (is (= id1 id2)
          "file change")))

  (testing "relativizes file paths against source-dir"
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated
                                        :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "/projects/myapp/src/events.cljs"
                                        :row 42}
                                       "/projects/myapp")]
      (is (= "src/events.cljs" (:file id))
          "absolute path becomes relative to source-dir"))
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated
                                        :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "src/events.cljs"
                                        :row 42})]
      (is (= "src/events.cljs" (:file id))
          "relative path without source-dir stays unchanged"))
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated
                                        :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "/other/project/src/events.cljs"
                                        :row 42}
                                       "/projects/myapp")]
      (is (= "/other/project/src/events.cljs" (:file id))
          "path outside source-dir stays unchanged"))))

(deftest result->identities-test
  (let [result {:unused-subs [{:kw :app/a
                               :type :sub
                               :file "a.cljs"
                               :row 1}
                              {:kw :app/b
                               :type :sub
                               :file "b.cljs"
                               :row 2}]
                :phantom-events [{:kw :app/c
                                  :type :event
                                  :file "c.cljs"
                                  :row 3}]
                :dynamic-sites [{:form "(rf/dispatch [x])"
                                 :file "d.cljs"
                                 :row 4}]}]
    (is (= #{{:rule :unused-subs
              :key :app/a}
             {:rule :unused-subs
              :key :app/b}
             {:rule :phantom-events
              :key :app/c}
             {:rule :dynamic-sites
              :form "(rf/dispatch [x])"
              :file "d.cljs"
              :line 4}}
           (baseline/result->identities result))))
  (is (= #{} (baseline/result->identities {:unused-subs []
                                           :phantom-events []}))
      "empty result returns empty set"))

(def ^:private test-issues
  #{{:rule :unused-subs
     :key :app/b}
    {:rule :unused-subs
     :key :app/a}
    {:rule :unused-styles
     :ns "app.ui"
     :var "z-style"}
    {:rule :unused-styles
     :ns "app.ui"
     :var "a-style"}})

(defn- tmp-baseline-path []
  (fs/tmp-file-path "baseline-" ".edn"))

(deftest write-baseline-test
  (testing "round-trip preserves issue set"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [{:keys [ok]} (baseline/read-baseline path)]
        (is (= test-issues ok)))))

  (testing "deterministic sort order"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [data (edn/read-string (fs/slurp-file path))
            rules (mapv :rule (:issues data))]
        (is (= rules (sort-by str rules))))))

  (testing "includes metadata"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [data (edn/read-string (fs/slurp-file path))]
        (is (= baseline/baseline-version (:version data)))
        (is (string? (:generated-at data)))
        (is (string? (:tool-version data)))
        (is (seq (:tool-version data))
            "tool-version is never blank"))))

  (testing "tool-version falls back to \"dev\" when no VERSION resource is present"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [data (edn/read-string (fs/slurp-file path))]
        (is (= "dev" (:tool-version data))
            "test classpath has no cljs_patrol/VERSION resource"))))

  (testing "creates parent directories"
    (let [dir (fs/join-path (fs/tmp-dir) (str "cljs-patrol-test-" (fs/nano-time)))
          path (fs/join-path (fs/join-path dir "sub") "baseline.edn")]
      (try
        (baseline/write-baseline path test-issues)
        (is (fs/file-exists? path))
        (finally
          (fs/delete-tree! dir))))))

(deftest read-baseline-test
  (testing "missing file"
    (let [{:keys [error]} (baseline/read-baseline "/nonexistent/baseline.edn")]
      (is (some? error))
      (is (re-find #"--baseline-write" error)
          "hints to run --baseline-write")))

  (testing "version mismatch"
    (let [path (tmp-baseline-path)]
      (fs/spit-file path (pr-str {:version 999
                          :issues []}))
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error))
        (is (re-find #"version" error)))))

  (testing "malformed data"
    (let [path (tmp-baseline-path)]
      (fs/spit-file path "[1 2 3]")
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error)))))

  (testing "unparseable EDN"
    (let [path (tmp-baseline-path)]
      (fs/spit-file path "{:version 1 :issues [unclosed")
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error)
            "returns error instead of throwing")))))

(def ^:private id-a {:rule :unused-subs
                     :key :app/a})
(def ^:private id-b {:rule :unused-subs
                     :key :app/b})
(def ^:private id-c {:rule :unused-subs
                     :key :app/c})

(deftest diff-baseline-test
  (is (= {:new #{id-a id-b}
          :present #{}
          :fixed #{}}
         (baseline/diff-baseline #{} #{id-a id-b}))
      "all new")
  (is (= {:new #{}
          :present #{id-a id-b}
          :fixed #{}}
         (baseline/diff-baseline #{id-a id-b} #{id-a id-b}))
      "all present")
  (is (= {:new #{}
          :present #{}
          :fixed #{id-a id-b}}
         (baseline/diff-baseline #{id-a id-b} #{}))
      "all fixed")
  (is (= {:new #{id-c}
          :present #{id-a}
          :fixed #{id-b}}
         (baseline/diff-baseline #{id-a id-b} #{id-a id-c}))
      "mixed")
  (is (= {:new #{}
          :present #{}
          :fixed #{}}
         (baseline/diff-baseline #{} #{}))
      "both empty"))

(deftest collect-identities-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [{:kw :app/a
                                                      :type :sub
                                                      :file "a.cljs"
                                                      :row 1}]
                                       :phantom-events []}
                                      {:unused-styles [{:kw :app.ui/s
                                                        :type :defclass
                                                        :file "s.cljs"
                                                        :row 2}]}]}]
        ids (baseline/collect-identities run-results)]
    (is (= #{{:rule :unused-subs
              :key :app/a}
             {:rule :unused-styles
              :ns "app.ui"
              :var "s"}}
           ids))))

(deftest collect-identities-when-empty-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs []
                                       :phantom-events []}
                                      {:unused-styles []}]}]]
    (is (= #{} (baseline/collect-identities run-results)))))

(def ^:private defaults
  {:baseline-path nil
   :strict-baseline false
   :quiet-baseline false})

(deftest merge-config-test
  (is (= (assoc defaults :strict-baseline true)
         (baseline/merge-config {:baseline {:strict true}} {}))
      "config sets strict")
  (is (= (assoc defaults :quiet-baseline true)
         (baseline/merge-config {:baseline {:quiet true}} {}))
      "config sets quiet")
  (is (= (assoc defaults :baseline-path "custom/path.edn")
         (baseline/merge-config {:baseline {:path "custom/path.edn"}} {}))
      "config sets path")
  (is (= defaults
         (baseline/merge-config {} {}))
      "empty config and empty cli yields all defaults")
  (is (= (-> defaults
             (assoc :baseline-path "cli.edn")
             (assoc :strict-baseline true))
         (baseline/merge-config {:baseline {:path "config.edn"
                                            :strict true}}
                                {:baseline-path "cli.edn"}))
      "cli path overrides config path"))

(deftest resolve-baseline-path-test
  (is (= "/projects/myapp/.cljs-patrol/baseline.edn"
         (baseline/resolve-baseline-path nil ["/projects/myapp"]))
      "defaults to source-dir/.cljs-patrol/baseline.edn")
  (is (= "custom/baseline.edn"
         (baseline/resolve-baseline-path "custom/baseline.edn" ["/projects/myapp"]))
      "explicit path overrides default")
  (is (= "src/myapp/.cljs-patrol/baseline.edn"
         (baseline/resolve-baseline-path nil ["src/myapp"]))
      "works with relative source dirs"))
