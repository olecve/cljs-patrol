(ns cljs-patrol.parser-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.temp-project :as temp-project]
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

(deftest position-reads-node-metadata
  (let [loc (-> (z/of-string "(ns a)\n\n  (f\n   x)") z/right)]
    (is (= [3 3] (parser/position loc))
        "start of form")
    (is (= [[3 3] [4 6]] (parser/position-span loc))
        "start and end of form")
    (is (= 3 (parser/position-row loc))
        "row of form")))

(deftest position-falls-back-without-metadata
  (let [marker-loc (-> (z/of-string "#?(:cljs 1)") z/down)]
    (is (= [0 1] (parser/position marker-loc))
        "fallback for reader conditional marker")
    (is (nil? (parser/position-span marker-loc))
        "no span for reader conditional marker")
    (is (= 0 (parser/position-row marker-loc))
        "row 0 for reader conditional marker")))

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

(deftest macro-alias-keeps-require-alias-of-same-name
  (let [{:keys [results]} (temp-project/run-group re-frame/group
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

(defn- memo-probe
  "Build a group that records, for every vector, the memo in force and whether a style declaration encloses it."
  [seen]
  (reify group/RuleGroup
    (group-id [_] :memo-probe)
    (group-name [_] "Memo probe")
    (analyze [_ _] {})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})
    (parse-handlers [_]
      {:handle-vector (fn [loc _ file]
                        (swap! seen conj {:file file
                                          :memo parser/*file-memo*
                                          :style-decl? (hiccup/inside-style-decl? loc)})
                        nil)})))

(deftest analyze-file-gives-each-file-own-memo
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "cljs-patrol" (make-array java.nio.file.attribute.FileAttribute 0)))
        styles (io/file dir "styles.cljs")
        views (io/file dir "views.cljs")
        seen (atom [])
        probe (memo-probe seen)]
    (try
      (spit styles "(defclass c [] [:a])")
      (spit views "(defn v [] [:a])")
      (parser/analyze-file styles [probe])
      (parser/analyze-file views [probe])
      (let [[in-styles in-views] (map (fn [file] (filter #(= (str file) (:file %)) @seen)) [styles views])]
        (is (= [[true true] [false false]] (map #(map :style-decl? %) [in-styles in-views]))
            "each file answered from own forms")
        (is (every? some? (map :memo @seen))
            "memo bound while file is read")
        (is (every? #(identical? (:memo (first in-styles)) (:memo %)) in-styles)
            "one memo throughout file")
        (is (not (identical? (:memo (first in-styles)) (:memo (first in-views))))
            "fresh memo per file")
        (is (nil? parser/*file-memo*)
            "memo unbound after file"))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete ^java.io.File file))))))
