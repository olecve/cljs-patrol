(ns cljs-patrol.integration.experimental-clj-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.docstrings :as docstrings]
   [cljs-patrol.groups.tests :as tests]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/clj-app")

(def ^:private assemble-groups #'core/assemble-groups)

(defn- clj-reading-groups [clj?]
  (into #{}
        (comp (filter #(contains? (group/file-extensions %) ".clj"))
              (map group/group-id))
        (assemble-groups {} clj?)))

(defn- findings [rule-group rule]
  (-> (core/run fixture-dir [rule-group]) :group-results first rule))

(deftest enabled-extensions-test
  (testing "the set is the union of what the enabled groups declare"
    (is (= #{".cljs" ".cljc"}
           (parser/enabled-extensions [(docstrings/make-group) (a11y/make-group nil)])))
    (is (= #{".cljs" ".cljc" ".clj"}
           (parser/enabled-extensions [(docstrings/make-group {:clj? true}) (a11y/make-group nil)]))
        "one group asking for .clj is enough for discovery to walk those files"))

  (testing "no enabled group means no extension to look for"
    (is (empty? (parser/enabled-extensions [])))))

(deftest docstrings-on-clj-test
  (testing "off by default, a .clj file is not read at all"
    (is (empty? (findings (docstrings/make-group) :docstring-summary))))

  (testing "on, the same file is read"
    (let [found (findings (docstrings/make-group {:clj? true}) :docstring-summary)]
      (is (= 1 (count found)))
      (is (str/ends-with? (:file (first found)) "core.clj"))
      (is (= :app.core/summarize (:kw (first found)))))))

(deftest tests-group-on-clj-test
  (testing "off by default, a .clj test file is not read at all"
    (is (empty? (findings (tests/make-group) :assertion-message-inline))))

  (testing "on, the assertion in the .clj test is flagged"
    (let [found (findings (tests/make-group {:clj? true}) :assertion-message-inline)]
      (is (= 1 (count found)))
      (is (str/ends-with? (:file (first found)) "core_test.clj")))))

(deftest cljs-only-groups-ignore-clj-test
  (testing "a11y reads the .cljs file"
    (let [found (findings (a11y/make-group nil) :img-alt-missing)]
      (is (= 1 (count found))
          "both fixture files hold the same :img, and only the .cljs one is a11y's to read")
      (is (str/ends-with? (:file (first found)) "widget.cljs"))))

  (testing "experimental mode does not widen a group that did not ask for it"
    (let [found (-> (core/run fixture-dir [(a11y/make-group nil) (docstrings/make-group {:clj? true})])
                    :group-results first :img-alt-missing)]
      (is (= 1 (count found))
          "discovery now walks .clj for the docstrings group, and a11y still skips those files")
      (is (str/ends-with? (:file (first found)) "widget.cljs")))))

(deftest assemble-groups-wiring-test
  (testing "off, nothing reads .clj"
    (is (empty? (clj-reading-groups false))))

  (testing "on, the two groups whose rules are about Clojure the language"
    (is (= #{:docstrings :tests} (clj-reading-groups true))
        "every group that reads Hiccup, re-frame or Spade stays on .cljs/.cljc")))
