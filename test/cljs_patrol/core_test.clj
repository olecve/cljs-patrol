(ns cljs-patrol.core-test
  (:require
   [cljs-patrol.core]
   [clojure.test :refer [deftest is testing]]))

(def ^:private parse-args #'cljs-patrol.core/parse-args)
(def ^:private filter-groups #'cljs-patrol.core/filter-groups)

(deftest parse-args-test
  (testing "empty args"
    (is (= [{:only nil :disabled nil :output nil} []]
           (parse-args []))))

  (testing "single source dir"
    (is (= [{:only nil :disabled nil :output nil} ["src/cljs"]]
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
      (is (= #{:spade} (:disabled opts)))))

  (testing "--output html"
    (let [[opts _] (parse-args ["--output" "html" "src"])]
      (is (= :html (:output opts)))))

  (testing "--output edn"
    (let [[opts _] (parse-args ["--output" "edn" "src"])]
      (is (= :edn (:output opts))))))

(deftest filter-groups-test
  (testing "no filters returns all groups"
    (is (= 2 (count (filter-groups {:only nil :disabled nil})))))

  (testing "--only selects specific group"
    (let [groups (filter-groups {:only #{:re-frame} :disabled nil})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--disable removes specific group"
    (let [groups (filter-groups {:only nil :disabled #{:spade}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups))))))

  (testing "--only takes precedence over --disable"
    (let [groups (filter-groups {:only #{:re-frame} :disabled #{:re-frame}})]
      (is (= 1 (count groups)))
      (is (= :re-frame (:id (first groups)))))))
