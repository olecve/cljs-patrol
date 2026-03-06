(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.core]
   [clojure.test :refer [deftest is testing]]))

(def ^:private filter-groups #'cljs-patrol.core/filter-groups)
(def ^:private filter-run-results #'cljs-patrol.core/filter-run-results)
(def ^:private merge-config #'cljs-patrol.core/merge-config)

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
  (testing "no filters returns default groups only"
    (let [groups (filter-groups {})]
      (is (= 2 (count groups)))
      (is (= #{:re-frame :spade} (set (map :id groups))))))

  (testing "--only selects specific group"
    (let [groups (filter-groups {:only #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--only can select non-default group"
    (let [groups (filter-groups {:only #{:typography}})]
      (is (= 1 (count groups)))
      (is (= :typography (:id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups {:disable #{:spade}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups {:only #{:re-frame} :disable #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups)))))))

(deftest merge-config-test
  (testing "config :groups sets :only when CLI --only not provided"
    (let [opts (merge-config {} {:groups [:re-frame :typography]})]
      (is (= #{:re-frame :typography} (:only opts)))))

  (testing "CLI --only takes precedence over config :groups"
    (let [opts (merge-config {:only #{:spade}} {:groups [:re-frame :typography]})]
      (is (= #{:spade} (:only opts)))))

  (testing "config :output sets output when CLI --output not provided"
    (let [opts (merge-config {} {:output :edn})]
      (is (= :edn (:output opts)))))

  (testing "CLI --output takes precedence over config :output"
    (let [opts (merge-config {:output :html} {:output :edn})]
      (is (= :html (:output opts)))))

  (testing "nil config leaves opts unchanged"
    (let [opts (merge-config {:only #{:spade}} nil)]
      (is (= #{:spade} (:only opts))))))
