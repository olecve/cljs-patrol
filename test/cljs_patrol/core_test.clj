(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.core]
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
    (is (= 2 (count (filter-groups {})))))

  (testing "--only selects specific group"
    (let [groups (filter-groups {:only #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups {:disable #{:spade}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups {:only #{:re-frame} :disable #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups)))))))
