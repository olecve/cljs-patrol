(ns cljs-patrol.reporters.edn-test
  (:require
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.reporters.edn :as edn-reporter]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]))

(def ^:private kw-item {:kw :my-ns/sub :file "src/subs.cljs" :row 3 :type :sub})

(deftest print-report-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [kw-item]
                                       :unused-events []
                                       :phantom-subs []
                                       :phantom-events []
                                       :duplicate-subs []
                                       :duplicate-events []
                                       :deprecated-effects []
                                       :dynamic-sites []}]}]
        output (with-out-str
                 (edn-reporter/print-report [re-frame/group] ["src"] run-results))
        parsed (edn/read-string output)]

    (testing "output is valid EDN"
      (is (map? parsed)))

    (testing "contains source-dirs"
      (is (contains? parsed :source-dirs))
      (is (vector? (:source-dirs parsed))))

    (testing "contains results keyed by group-id"
      (is (contains? (:results parsed) :re-frame)))

    (testing "contains suggestions keyed by group-id"
      (is (contains? (:suggestions parsed) :re-frame)))

    (testing "result items have absolute file paths"
      (let [items (get-in parsed [:results :re-frame :unused-subs])]
        (is (= 1 (count items)))
        (is (.isAbsolute (java.io.File. (:file (first items)))))))

    (testing "suggestions contain expected keys"
      (let [suggs (get-in parsed [:suggestions :re-frame])]
        (is (contains? suggs :unused-subs))
        (is (contains? suggs :phantom-events))))))

(deftest print-report-merges-multiple-dirs-test
  (let [item-a {:kw :a/sub :file "src/a.cljs" :row 1 :type :sub}
        item-b {:kw :b/sub :file "src/b.cljs" :row 2 :type :sub}
        run-results [{:source-dir "src"
                      :group-results [{:unused-subs [item-a]
                                       :unused-events []
                                       :phantom-subs []
                                       :phantom-events []
                                       :duplicate-subs []
                                       :duplicate-events []
                                       :deprecated-effects []
                                       :dynamic-sites []}]}
                     {:source-dir "other"
                      :group-results [{:unused-subs [item-b]
                                       :unused-events []
                                       :phantom-subs []
                                       :phantom-events []
                                       :duplicate-subs []
                                       :duplicate-events []
                                       :deprecated-effects []
                                       :dynamic-sites []}]}]
        output (with-out-str
                 (edn-reporter/print-report [re-frame/group] ["src" "other"] run-results))
        parsed (edn/read-string output)]

    (testing "merges unused-subs from multiple source dirs"
      (let [items (get-in parsed [:results :re-frame :unused-subs])]
        (is (= 2 (count items)))))))
