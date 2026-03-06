(ns cljs-patrol.reporters.html-test
  (:require
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.reporters.html :as html]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private key->title #'cljs-patrol.reporters.html/key->title)
(def ^:private infer-columns #'cljs-patrol.reporters.html/infer-columns)
(def ^:private aggregate-sections #'cljs-patrol.reporters.html/aggregate-sections)

(deftest key->title-test
  (is (= "Unused subs" (key->title :unused-subs)))
  (is (= "Phantom events" (key->title :phantom-events)))
  (is (= "Dynamic sites" (key->title :dynamic-sites))))

(deftest infer-columns-test
  (testing "form items use form/file/line columns"
    (is (= [:form :file :line]
           (infer-columns {:form "(rf/dispatch [::ev])" :file "f.cljs" :row 1}))))

  (testing "kw items use keyword/file/line columns"
    (is (= [:keyword :file :line]
           (infer-columns {:kw :my/sub :file "f.cljs" :row 1})))))

(def ^:private kw-item {:kw :my-ns/sub :file "src/subs.cljs" :row 3 :type :sub})
(def ^:private form-item {:form "(rf/dispatch [::ev])" :file "src/views.cljs" :row 7})

(deftest aggregate-sections-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [kw-item]
                                       :dynamic-sites [form-item]}]}]
        sections (aggregate-sections re-frame/group 0 run-results)]

    (testing "one section per sequential key"
      (is (= 2 (count sections))))

    (testing "kw section has correct structure"
      (let [s (first (filter #(= "Unused subs" (:title %)) sections))]
        (is (some? s))
        (is (= [:keyword :file :line] (:columns s)))
        (is (= [kw-item] (:items s)))))

    (testing "form section has correct structure"
      (let [s (first (filter #(= "Dynamic sites" (:title %)) sections))]
        (is (some? s))
        (is (= [:form :file :line] (:columns s)))
        (is (= [form-item] (:items s)))))

    (testing "sections include suggestion descriptions"
      (let [s (first (filter #(= "Unused subs" (:title %)) sections))]
        (is (str/includes? (:description s) "reg-sub"))))))

(deftest write-report-test
  (let [run-results [{:source-dir "src"
                      :group-results [{:unused-subs [kw-item]
                                       :unused-events []
                                       :phantom-subs []
                                       :phantom-events []
                                       :duplicate-subs []
                                       :duplicate-events []
                                       :deprecated-effects []
                                       :dynamic-sites []}]}]
        tmp-file (java.io.File/createTempFile "cljs-patrol-test" ".html")]
    (try
      (html/write-report [re-frame/group] run-results (.getPath tmp-file))
      (let [content (slurp tmp-file)]
        (testing "produces HTML document"
          (is (str/includes? content "<!DOCTYPE html>")))
        (testing "includes group name"
          (is (str/includes? content "Re-frame")))
        (testing "includes keyword from result"
          (is (str/includes? content ":my-ns/sub"))))
      (finally
        (.delete tmp-file)))))
