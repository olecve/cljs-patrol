(ns cljs-patrol.baseline-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest issue->identity-test
  (testing "keyword-keyed rules use rule + key"
    (is (= {:rule :unused-subs :key :app.subs/users}
           (baseline/issue->identity :unused-subs
                                     {:kw :app.subs/users :type :sub
                                      :file "src/app/subs.cljs" :row 10}))
        "unused-subs")
    (is (= {:rule :unused-events :key :app.events/init}
           (baseline/issue->identity :unused-events
                                     {:kw :app.events/init :type :event
                                      :file "src/app/events.cljs" :row 5}))
        "unused-events")
    (is (= {:rule :phantom-subs :key :app.subs/missing}
           (baseline/issue->identity :phantom-subs
                                     {:kw :app.subs/missing :type :sub
                                      :file "src/app/views.cljs" :row 20}))
        "phantom-subs")
    (is (= {:rule :phantom-events :key :app.events/ghost}
           (baseline/issue->identity :phantom-events
                                     {:kw :app.events/ghost :type :event
                                      :file "src/app/handlers.cljs" :row 30}))
        "phantom-events"))

  (testing "duplicate rules use rule + key (file not included)"
    (is (= {:rule :duplicate-subs :key :app.subs/users}
           (baseline/issue->identity :duplicate-subs
                                     {:kw :app.subs/users :type :sub
                                      :file "src/app/subs.cljs" :row 10}))
        "duplicate-subs")
    (is (= {:rule :duplicate-events :key :app.events/init}
           (baseline/issue->identity :duplicate-events
                                     {:kw :app.events/init :type :event
                                      :file "src/app/events.cljs" :row 5}))
        "duplicate-events"))

  (testing "var-keyed rules use rule + ns + var"
    (is (= {:rule :unused-styles :ns "app.ui" :var "container-style"}
           (baseline/issue->identity :unused-styles
                                     {:kw :app.ui/container-style :type :defclass
                                      :file "src/app/ui.cljs" :row 8}))
        "unused-styles")
    (is (= {:rule :defattrs-in-merge :ns "app.ui" :var "box-attrs"}
           (baseline/issue->identity :defattrs-in-merge
                                     {:kw :app.ui/box-attrs :type :defattrs
                                      :file "src/app/ui.cljs" :row 12}))
        "defattrs-in-merge")
    (is (= {:rule :defclass-as-sole-attr :ns "app.ui" :var "btn-class"}
           (baseline/issue->identity :defclass-as-sole-attr
                                     {:kw :app.ui/btn-class :type :defclass
                                      :file "src/app/ui.cljs" :row 15}))
        "defclass-as-sole-attr")
    (is (= {:rule :mixed-token-groups :ns "app.styles" :var "heading"}
           (baseline/issue->identity :mixed-token-groups
                                     {:decl-kw :app.styles/heading
                                      :prefixes #{"action-small" "body-short-small"}
                                      :file "src/app/styles.cljs" :row 20}))
        "mixed-token-groups uses :decl-kw"))

  (testing "site rules use rule + file + line"
    (is (= {:rule :deprecated-effects :effect ":dispatch-n"
            :file "src/app/events.cljs" :line 42}
           (baseline/issue->identity :deprecated-effects
                                     {:type :deprecated :effect ":dispatch-n"
                                      :form ":dispatch-n"
                                      :file "src/app/events.cljs" :row 42}))
        "deprecated-effects")
    (is (= {:rule :dynamic-sites :form "(rf/dispatch [ev])"
            :file "src/app/handlers.cljs" :line 55}
           (baseline/issue->identity :dynamic-sites
                                     {:form "(rf/dispatch [ev])"
                                      :file "src/app/handlers.cljs" :row 55}))
        "dynamic-sites")
    (is (= {:rule :dynamic-sites :form "(dispatch [on-change {:tab tab :sort sort}])"
            :file "src/views.cljs" :line 10}
           (baseline/issue->identity :dynamic-sites
                                     {:form "(dispatch [on-change {:tab tab\n                            :sort sort}])"
                                      :file "src/views.cljs" :row 10}))
        "multiline form collapses to single line"))

  (testing "unknown rule throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown rule"
          (baseline/issue->identity :bogus-rule {:kw :foo}))))

  (testing "ignores volatile fields"
    (let [id1 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo :type :sub :file "a.cljs" :row 1})
          id2 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo :type :sub :file "a.cljs" :row 99})]
      (is (= id1 id2)
          "line number change"))
    (let [id1 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo :type :sub :file "old.cljs" :row 1})
          id2 (baseline/issue->identity :unused-subs
                                        {:kw :app/foo :type :sub :file "new.cljs" :row 1})]
      (is (= id1 id2)
          "file change")))

  (testing "relativizes file paths against source-dir"
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "/projects/myapp/src/events.cljs" :row 42}
                                       "/projects/myapp")]
      (is (= "src/events.cljs" (:file id))
          "absolute path becomes relative to source-dir"))
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "src/events.cljs" :row 42})]
      (is (= "src/events.cljs" (:file id))
          "relative path without source-dir stays unchanged"))
    (let [id (baseline/issue->identity :deprecated-effects
                                       {:type :deprecated :effect ":dispatch-n"
                                        :form ":dispatch-n"
                                        :file "/other/project/src/events.cljs" :row 42}
                                       "/projects/myapp")]
      (is (= "/other/project/src/events.cljs" (:file id))
          "path outside source-dir stays unchanged"))))

(deftest result->identities-test
  (let [result {:unused-subs [{:kw :app/a :type :sub :file "a.cljs" :row 1}
                              {:kw :app/b :type :sub :file "b.cljs" :row 2}]
                :phantom-events [{:kw :app/c :type :event :file "c.cljs" :row 3}]
                :dynamic-sites [{:form "(rf/dispatch [x])" :file "d.cljs" :row 4}]}]
    (is (= #{{:rule :unused-subs :key :app/a}
             {:rule :unused-subs :key :app/b}
             {:rule :phantom-events :key :app/c}
             {:rule :dynamic-sites :form "(rf/dispatch [x])" :file "d.cljs" :line 4}}
           (baseline/result->identities result))))
  (is (= #{} (baseline/result->identities {:unused-subs [] :phantom-events []}))
      "empty result returns empty set"))

(def ^:private test-issues
  #{{:rule :unused-subs :key :app/b}
    {:rule :unused-subs :key :app/a}
    {:rule :unused-styles :ns "app.ui" :var "z-style"}
    {:rule :unused-styles :ns "app.ui" :var "a-style"}})

(defn- tmp-baseline-path []
  (let [f (java.io.File/createTempFile "baseline" ".edn")]
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest write-baseline-test
  (testing "round-trip preserves issue set"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [{:keys [ok]} (baseline/read-baseline path)]
        (is (= test-issues ok)))))

  (testing "deterministic sort order"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [data (edn/read-string (slurp path))
            rules (mapv :rule (:issues data))]
        (is (= rules (sort-by str rules))))))

  (testing "includes metadata"
    (let [path (tmp-baseline-path)]
      (baseline/write-baseline path test-issues)
      (let [data (edn/read-string (slurp path))]
        (is (= baseline/baseline-version (:version data)))
        (is (string? (:generated-at data)))
        (is (string? (:tool-version data))))))

  (testing "creates parent directories"
    (let [dir (io/file (System/getProperty "java.io.tmpdir")
                       (str "cljs-patrol-test-" (System/nanoTime)))
          path (str (.getAbsolutePath dir) "/sub/baseline.edn")]
      (try
        (baseline/write-baseline path test-issues)
        (is (.exists (io/file path)))
        (finally
          (run! #(.delete %) (reverse (file-seq dir))))))))

(deftest read-baseline-test
  (testing "missing file"
    (let [{:keys [error]} (baseline/read-baseline "/nonexistent/baseline.edn")]
      (is (some? error))
      (is (re-find #"--baseline-write" error)
          "hints to run --baseline-write")))

  (testing "version mismatch"
    (let [path (tmp-baseline-path)]
      (spit path (pr-str {:version 999 :issues []}))
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error))
        (is (re-find #"version" error)))))

  (testing "malformed data"
    (let [path (tmp-baseline-path)]
      (spit path "[1 2 3]")
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error)))))

  (testing "unparseable EDN"
    (let [path (tmp-baseline-path)]
      (spit path "{:version 1 :issues [unclosed")
      (let [{:keys [error]} (baseline/read-baseline path)]
        (is (some? error)
            "returns error instead of throwing")))))

(def ^:private id-a {:rule :unused-subs :key :app/a})
(def ^:private id-b {:rule :unused-subs :key :app/b})
(def ^:private id-c {:rule :unused-subs :key :app/c})

(deftest diff-baseline-test
  (is (= {:new #{id-a id-b} :present #{} :fixed #{}}
         (baseline/diff-baseline #{} #{id-a id-b}))
      "all new")
  (is (= {:new #{} :present #{id-a id-b} :fixed #{}}
         (baseline/diff-baseline #{id-a id-b} #{id-a id-b}))
      "all present")
  (is (= {:new #{} :present #{} :fixed #{id-a id-b}}
         (baseline/diff-baseline #{id-a id-b} #{}))
      "all fixed")
  (is (= {:new #{id-c} :present #{id-a} :fixed #{id-b}}
         (baseline/diff-baseline #{id-a id-b} #{id-a id-c}))
      "mixed")
  (is (= {:new #{} :present #{} :fixed #{}}
         (baseline/diff-baseline #{} #{}))
      "both empty"))

(deftest collect-identities-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [{:kw :app/a :type :sub :file "a.cljs" :row 1}]
                                       :phantom-events []}
                                      {:unused-styles [{:kw :app.ui/s :type :defclass
                                                        :file "s.cljs" :row 2}]}]}]
        ids (baseline/collect-identities run-results)]
    (is (= #{{:rule :unused-subs :key :app/a}
             {:rule :unused-styles :ns "app.ui" :var "s"}}
           ids))))

(deftest collect-identities-when-empty-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [] :phantom-events []}
                                      {:unused-styles []}]}]]
    (is (= #{} (baseline/collect-identities run-results)))))

(deftest merge-config-test
  (is (= {:strict-baseline true}
         (baseline/merge-config {:strict true} {}))
      "config sets strict")
  (is (= {:quiet-baseline true}
         (baseline/merge-config {:quiet true} {}))
      "config sets quiet")
  (is (= {:baseline-path "custom/path.edn"}
         (baseline/merge-config {:path "custom/path.edn"} {}))
      "config sets path")
  (is (= {}
         (baseline/merge-config {} {}))
      "empty config and empty cli")
  (is (= {:baseline-path "cli.edn" :strict-baseline true}
         (baseline/merge-config {:path "config.edn" :strict true} {:baseline-path "cli.edn"}))
      "cli path overrides config path"))
