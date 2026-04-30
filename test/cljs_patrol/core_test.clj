(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def ^:private filter-groups #'cljs-patrol.core/filter-groups)
(def ^:private filter-run-results #'cljs-patrol.core/filter-run-results)

(deftest filter-run-results-test
  (let [abs #(.getAbsolutePath (java.io.File. %))
        item-a {:kw :a/sub :file "src/a.cljs" :row 1}
        item-b {:kw :b/sub :file "src/b.cljs" :row 1}
        run-results [{:source-dir "src"
                      :group-results [{:unused-subs [item-a item-b]
                                       :unused-events []}]}]]
    (testing "filters items to requested files only"
      (let [result (filter-run-results run-results [(abs "src/a.cljs")])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (= 1 (count subs)))
        (is (= "src/a.cljs" (:file (first subs))))))

    (testing "empty result when no files match"
      (let [result (filter-run-results run-results [(abs "src/other.cljs")])
            subs (get-in result [0 :group-results 0 :unused-subs])]
        (is (empty? subs))))))

(deftest filter-groups-test
  (testing "no filters returns all groups"
    (is (= 4 (count (filter-groups {})))))

  (testing "--only selects specific group"
    (let [groups (filter-groups {:only #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups {:disable #{:spade}})]
      (is (= 3 (count groups)))
      (is (= #{:re-frame :reagent :typography} (set (map group/group-id groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups {:only #{:re-frame} :disable #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (group/group-id (first groups)))))))

(def ^:private fixture-dir "test/projects/re-frame-spade-app/src/webapp")

(deftest baseline-write-integration-test
  (let [enabled-groups [re-frame/group spade/group]
        run-results [(core/run fixture-dir enabled-groups)]
        identities (baseline/collect-identities enabled-groups run-results)
        dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "cljs-patrol-bw-" (System/nanoTime)))
        path (str (.getAbsolutePath dir) "/baseline.edn")]
    (try
      (baseline/write-baseline path identities)
      (is (.exists (io/file path)) "baseline file created")
      (let [{:keys [ok]} (baseline/read-baseline path)]
        (is (set? ok) "reads back as a set")
        (is (pos? (count ok)) "contains issues from fixture project")
        (is (every? :rule ok) "every identity has a :rule"))
      (finally
        (run! #(.delete %) (reverse (file-seq dir)))))))
