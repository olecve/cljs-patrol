(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.core]
   [clojure.test :refer [deftest is testing]]))

(def ^:private parse-args #'cljs-patrol.core/parse-args)
(def ^:private filter-groups #'cljs-patrol.core/filter-groups)
(def ^:private filter-run-results #'cljs-patrol.core/filter-run-results)

(deftest parse-args-test
  (testing "empty args"
    (is (= [{} []]
           (parse-args []))))

  (testing "single source dir"
    (is (= [{} ["src/cljs"]]
           (parse-args ["src/cljs"]))))

  (testing "multiple source dirs"
    (let [[_ dirs] (parse-args ["src/a" "src/b"])]
      (is (= ["src/a" "src/b"] dirs))))

  (testing "--only single group"
    (let [[opts _] (parse-args ["--only" "re-frame" "src"])]
      (is (= #{:re-frame} (:only opts)))))

  (testing "--only multiple groups"
    (let [[opts _] (parse-args ["--only" "re-frame,spade" "src"])]
      (is (= #{:re-frame :spade} (:only opts)))))

  (testing "--disable flag"
    (let [[opts _] (parse-args ["--disable" "spade" "src"])]
      (is (= #{:spade} (:disable opts)))))

  (testing "--output html"
    (let [[opts _] (parse-args ["--output" "html" "src"])]
      (is (= :html (:output opts)))))

  (testing "--output edn"
    (let [[opts _] (parse-args ["--output" "edn" "src"])]
      (is (= :edn (:output opts)))))

  (testing "--files single file"
    (let [[opts _] (parse-args ["--files" "src/app/subs.cljs" "src"])]
      (is (= ["src/app/subs.cljs"] (:files opts)))))

  (testing "--files multiple files"
    (let [[opts _] (parse-args ["--files" "src/a.cljs,src/b.cljs" "src"])]
      (is (= ["src/a.cljs" "src/b.cljs"] (:files opts)))))

  (testing "no --files defaults to nil"
    (let [[opts _] (parse-args ["src"])]
      (is (nil? (:files opts))))))

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
