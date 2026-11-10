(ns cljs-patrol.parser-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.parser :as parser]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(deftest resolve-kw-test
  (testing "plain keyword"
    (is (= :plain (parser/resolve-kw ":plain" "my.ns" {}))))

  (testing "qualified keyword passes through unchanged"
    (is (= :other.ns/name (parser/resolve-kw ":other.ns/name" "my.ns" {}))))

  (testing "auto-resolved local keyword"
    (is (= :my.ns/local (parser/resolve-kw "::local" "my.ns" {}))))

  (testing "auto-resolved aliased keyword"
    (is (= :full.ns/name (parser/resolve-kw "::alias/name" "my.ns" {"alias" "full.ns"}))))

  (testing "unknown alias returns nil"
    (is (nil? (parser/resolve-kw "::unknown/name" "my.ns" {})))))

(deftest resolve-sym-test
  (testing "unqualified symbol resolves to current ns"
    (is (= :my.ns/fn-name (parser/resolve-sym "fn-name" "my.ns" {}))))

  (testing "aliased symbol resolves via alias map"
    (is (= :full.ns/fn-name (parser/resolve-sym "alias/fn-name" "my.ns" {"alias" "full.ns"}))))

  (testing "unknown alias returns nil"
    (is (nil? (parser/resolve-sym "unknown/fn-name" "my.ns" {})))))

(deftest distinct-by-test
  (testing "removes duplicates by key, last one wins"
    (let [result (parser/distinct-by :id [{:id 1
                                           :v :a}
                                          {:id 2
                                           :v :b}
                                          {:id 1
                                           :v :c}])]
      (is (= 2 (count result)))
      (is (= #{1 2} (set (map :id result))))))

  (testing "empty collection returns empty"
    (is (empty? (parser/distinct-by :id []))))

  (testing "no duplicates returns all items"
    (is (= 3 (count (parser/distinct-by :id [{:id 1} {:id 2} {:id 3}]))))))

(defn- run-project
  "Write `files`, a map of relative path to source, as a project, and return its first group's results."
  [rule-group files]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "parser" (make-array java.nio.file.attribute.FileAttribute 0)))
        written (mapv (fn [[path source]]
                        (let [file (io/file dir path)]
                          (io/make-parents file)
                          (spit file source)
                          file))
                      files)]
    (try
      (binding [*err* (java.io.StringWriter.)]
        (-> (core/run (str dir) [rule-group]) :group-results first))
      (finally
        (doseq [file (rseq written)] (.delete file))
        (doseq [d (->> (file-seq dir) (filter #(.isDirectory %)) reverse)] (.delete d))))))

(deftest macro-alias-keeps-require-alias-of-same-name
  (let [results (run-project re-frame/group
                             {"app/events.cljs" "(ns app.events (:require [re-frame.core :as rf]))
(rf/reg-event-db ::save (fn [db _] db))"
                              "app/views.cljs" "(ns app.views
  (:require [app.events :as e] [re-frame.core :as rf])
  (:require-macros [app.macros :as e]))
(defn view [] [:button {:on-click #(rf/dispatch [::e/save])}])"})]
    (is (empty? (:phantom-events results))
        "resolves keyword through require alias")
    (is (empty? (:unused-events results))
        "counts dispatch toward event")))

(deftest macro-aliases-and-refers-are-kept-apart
  (let [ns-info (parser/find-ns-info (z/of-string "(ns app.views
  (:require [app.events :as e :refer [save]] [app.ui :refer-macros [panel]])
  (:require-macros [app.macros :as e :refer [save with-let]]))"))]
    (is (= {:aliases {"e" "app.events"}
            :refers {"save" "app.events"}
            :macro-aliases {"e" "app.macros"}
            :macro-refers {"save" "app.macros"
                           "with-let" "app.macros"
                           "panel" "app.ui"}}
           (select-keys ns-info [:aliases :refers :macro-aliases :macro-refers]))
        "keeps require-macros and refer-macros names out of aliases and refers")))
