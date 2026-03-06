(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private filter-groups #'cljs-patrol.core/filter-groups)
(def ^:private filter-run-results #'cljs-patrol.core/filter-run-results)
(def ^:private load-extra-groups #'cljs-patrol.core/load-extra-groups)

(def ^:private all-groups [re-frame/group spade/group])

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
    (is (= 2 (count (filter-groups {} all-groups)))))

  (testing "--only selects specific group"
    (let [groups (filter-groups {:only #{:re-frame}} all-groups)]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups {:disable #{:spade}} all-groups)]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups {:only #{:re-frame} :disable #{:re-frame}} all-groups)]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "extra groups are included"
    (let [extra {:id :extra :name "Extra"}
          groups (filter-groups {} (conj all-groups extra))]
      (is (= 3 (count groups))))))

(deftest load-extra-groups-test
  (testing "loads a var by qualified symbol string"
    (let [groups (load-extra-groups ["cljs-patrol.groups.re-frame/group"])]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "throws on unqualified symbol"
    (is (thrown? clojure.lang.ExceptionInfo
          (load-extra-groups ["just-a-name"]))))

  (testing "throws on non-existent var"
    (is (thrown? clojure.lang.ExceptionInfo
          (load-extra-groups ["cljs-patrol.groups.re-frame/no-such-var"])))))
