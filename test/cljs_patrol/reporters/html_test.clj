(ns cljs-patrol.reporters.html-test
  (:require
   [cljs-patrol.disk :as disk]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.memory-fs :as memory-fs]
   [cljs-patrol.reporters.html :as html]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private key->title #'html/key->title)
(def ^:private infer-columns #'html/infer-columns)
(def ^:private aggregate-sections #'html/aggregate-sections)

(deftest key->title-test
  (is (= "Unused subs" (key->title :unused-subs)))
  (is (= "Phantom events" (key->title :phantom-events)))
  (is (= "Dynamic sites" (key->title :dynamic-sites))))

(deftest infer-columns-test
  (testing "form items use form/file/line columns"
    (is (= [:form :file :line]
           (infer-columns [{:form "(rf/dispatch [::ev])"
                            :file "f.cljs"
                            :row 1}]))))

  (testing "kw items use keyword/file/line columns"
    (is (= [:keyword :file :line]
           (infer-columns [{:kw :my/sub
                            :file "f.cljs"
                            :row 1}]))))

  (testing "a section gains a Fix column when some item carries a hint"
    (is (= [:form :file :line :hint]
           (infer-columns [{:form "(rf/dispatch [::ev])"
                            :file "f.cljs"
                            :row 1
                            :hint "Do the thing."}]))
        "hinted findings get somewhere to show the fix")
    (is (= [:form :file :line :hint]
           (infer-columns [{:form "(a)"
                            :file "f.cljs"
                            :row 1}
                           {:form "(b)"
                            :file "f.cljs"
                            :row 2
                            :hint "Do the thing."}]))
        "one hinted item is enough, since the column is per section")
    (is (= [:keyword :file :line]
           (infer-columns [{:kw :my/sub
                            :file "f.cljs"
                            :row 1}]))
        "groups that attach no hint keep the narrower table")))

(def ^:private kw-item {:kw :my-ns/sub
                        :file "src/subs.cljs"
                        :row 3
                        :type :sub})
(def ^:private form-item {:form "(rf/dispatch [::ev])"
                          :file "src/views.cljs"
                          :row 7})

(deftest aggregate-sections-test
  (let [run-results [{:source-dir (disk/path "src")
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
        (is (str/includes? (:description s) "reg-sub"))
        (is (str/includes? (:description s) "Docs: https://github.com/olecve/cljs-patrol/blob/")
            "description carries docs link")))))

(deftest write-report-test
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [kw-item]
                                         :unused-events []
                                         :phantom-subs []
                                         :phantom-events []
                                         :duplicate-subs []
                                         :duplicate-events []
                                         :deprecated-effects []
                                         :dynamic-sites []}]}]
          tmp-file (memory-fs/path file-system "/work/report.html")]
      (html/write-report [re-frame/group] run-results tmp-file)
      (let [content (memory-fs/read-text tmp-file)]
        (testing "produces HTML document"
          (is (str/includes? content "<!DOCTYPE html>")))
        (testing "includes group name"
          (is (str/includes? content "Re-frame")))
        (testing "includes keyword from result"
          (is (str/includes? content ":my-ns/sub")))))))

(deftest reports-print-source-folders-as-typed
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :source-text "src/"
                        :group-results [{:unused-subs []}]}
                       {:source-dir (disk/path "test")
                        :source-text "test//"
                        :group-results [{:unused-subs []}]}]
          report-file (memory-fs/path file-system "/work/report.html")
          baseline-file (memory-fs/path file-system "/work/baseline-report.html")]
      (html/write-report [re-frame/group] run-results report-file)
      (html/write-baseline-report [re-frame/group] run-results baseline-file #{} 0)
      (is (str/includes? (memory-fs/read-text report-file) " | Analyzed: src/, test//")
          "report header keeps typed text")
      (is (str/includes? (memory-fs/read-text baseline-file) " | Analyzed: src/, test//")
          "baseline report header keeps typed text"))))

(deftest write-report-opens-only-sections-with-findings
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :source-text "src"
                        :group-results [{:unused-subs [kw-item]
                                         :unused-events []}]}]
          report-file (memory-fs/path file-system "/work/report.html")]
      (html/write-report [re-frame/group] run-results report-file)
      (is (= ["<details open>" "<details>"] (re-seq #"<details[^>]*>" (memory-fs/read-text report-file)))
          "section with findings open, empty one closed"))))

(deftest write-baseline-report-reads-identities-against-first-source-dir
  (with-open [file-system (memory-fs/unix)]
    (let [item {:type :deprecated
                :effect ":dispatch-n"
                :form ":dispatch-n"
                :file "src/events.cljs"
                :row 1}
          run-results [{:source-dir (disk/path "src")
                        :source-text "src"
                        :group-results [{:deprecated-effects [item]}]}
                       {:source-dir (disk/path "test")
                        :source-text "test"
                        :group-results [{:deprecated-effects []}]}]
          report-file (memory-fs/path file-system "/work/report.html")]
      (html/write-baseline-report [re-frame/group] run-results report-file
                                  #{{:rule :deprecated-effects
                                     :effect ":dispatch-n"
                                     :file "events.cljs"
                                     :line 1}}
                                  0)
      (is (str/includes? (memory-fs/read-text report-file) "class=\"new-issue\"")
          "finding relative to first source dir matches new identity"))))

(deftest write-baseline-report-of-empty-run-results
  (with-open [file-system (memory-fs/unix)]
    (let [report-file (memory-fs/path file-system "/work/report.html")]
      (html/write-baseline-report [re-frame/group] [] report-file #{} 0)
      (is (str/includes? (memory-fs/read-text report-file) "cljs-patrol report (baseline)")
          "empty run results give report"))))

(deftest write-report-writes-lone-surrogate-as-question-mark
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :source-text "src"
                        :group-results [{:dynamic-sites [{:form "(rf/dispatch [x\uD83D])"
                                                          :file "src/a.cljs"
                                                          :row 1}]}]}]
          report-file (memory-fs/path file-system "/work/report.html")]
      (html/write-report [re-frame/group] run-results report-file)
      (is (str/includes? (memory-fs/read-text report-file) "(rf/dispatch [x?])")
          "lone surrogate written as ?, as clojure.java.io/writer writes it")
      (is (str/includes? (memory-fs/read-text report-file) "vscode://file//work/src/a.cljs:1")
          "link holds absolute path on file system of report"))))

(deftest write-baseline-report-test
  (with-open [file-system (memory-fs/unix)]
    (let [new-item {:kw :app/new-sub
                    :type :sub
                    :file "src/new.cljs"
                    :row 1}
          old-item {:kw :app/old-sub
                    :type :sub
                    :file "src/old.cljs"
                    :row 2}
          run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [new-item old-item]
                                         :unused-events []
                                         :phantom-subs []
                                         :phantom-events []
                                         :duplicate-subs []
                                         :duplicate-events []
                                         :deprecated-effects []
                                         :dynamic-sites []}]}]
          new-ids #{{:rule :unused-subs
                     :key :app/new-sub}}
          tmp-file (memory-fs/path file-system "/work/report.html")]
      (html/write-baseline-report [re-frame/group] run-results tmp-file new-ids 3)
      (let [content (memory-fs/read-text tmp-file)]
        (is (str/includes? content "new-issue")
            "marks new issues with CSS class")
        (is (str/includes? content "baseline-issue")
            "marks baseline issues with CSS class")
        (is (str/includes? content "baseline")
            "includes baseline in title")
        (is (str/includes? content "3 baseline issues no longer present")
            "shows fixed count banner")))))

(deftest write-report-marks-blocking-test
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [kw-item]
                                         :unused-events []
                                         :phantom-subs []
                                         :phantom-events []
                                         :duplicate-subs []
                                         :duplicate-events []
                                         :deprecated-effects []
                                         :dynamic-sites []}]}]
          tmp-file (memory-fs/path file-system "/work/report.html")]
      (html/write-report [re-frame/group] run-results tmp-file
                         #{:unused-subs})
      (let [content (memory-fs/read-text tmp-file)]
        (is (str/includes? content "BLOCKING")
            "blocking badge present")))))

(deftest write-baseline-report-with-fail-on-test
  (with-open [file-system (memory-fs/unix)]
    (let [run-results [{:source-dir (disk/path "src")
                        :group-results [{:unused-subs [{:kw :app/new-sub
                                                        :type :sub
                                                        :file "src/new.cljs"
                                                        :row 1}]
                                         :unused-events []
                                         :phantom-subs []
                                         :phantom-events []
                                         :duplicate-subs []
                                         :duplicate-events []
                                         :deprecated-effects []
                                         :dynamic-sites []}]}]
          new-ids #{{:rule :unused-subs
                     :key :app/new-sub}}
          tmp-file (memory-fs/path file-system "/work/report.html")]
      (html/write-baseline-report [re-frame/group] run-results tmp-file
                                  new-ids 0
                                  #{:unused-subs} 1 0)
      (let [content (memory-fs/read-text tmp-file)]
        (is (str/includes? content "BLOCKING")
            "blocking badge applied in baseline HTML")
        (is (str/includes? content "1 blocking, 0 warnings")
            "tier summary panel shown")))))