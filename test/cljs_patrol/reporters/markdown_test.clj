(ns cljs-patrol.reporters.markdown-test
  (:require
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.reporters.markdown :as md-reporter]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private kw-item {:kw :my-ns/sub
                        :file "src/subs.cljs"
                        :row 3
                        :type :sub})
(def ^:private event-item {:kw :my-ns/event
                           :file "src/events.cljs"
                           :row 10
                           :type :event})
(def ^:private style-item {:kw :my-ns/container
                           :file "src/styles.cljs"
                           :row 5
                           :type :defclass})
(def ^:private dynamic-item {:form "(rf/dispatch [event-kw])"
                             :file "src/views.cljs"
                             :row 8})

(defn- re-frame-result [overrides]
  (merge {:unused-subs []
          :unused-events []
          :phantom-subs []
          :phantom-events []
          :duplicate-subs []
          :duplicate-events []
          :deprecated-effects []
          :dynamic-sites []}
         overrides))

(defn- run-report
  ([groups run-results]
   (with-out-str
     (md-reporter/print-report groups ["src"] run-results)))
  ([run-results]
   (run-report [re-frame/group] run-results)))

(deftest report-header-test
  (let [output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {})]}])]
    (testing "starts with report header"
      (is (str/starts-with? output "# cljs-patrol report")))))

(deftest unused-subs-section-test
  (let [output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {:unused-subs [kw-item]})]}])]
    (testing "renders section title with count"
      (is (str/includes? output "## Unused subs (1)")))
    (testing "renders keyword and absolute file path"
      (is (str/includes? output "`:my-ns/sub`"))
      (is (re-find #"src/subs\.cljs:3`" output)))
    (testing "includes suggestion"
      (is (str/includes? output "Registered with reg-sub but never subscribed to")))))

(deftest dynamic-sites-section-test
  (let [output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {:dynamic-sites [dynamic-item]})]}])]
    (testing "renders form-based entries"
      (is (str/includes? output "## Dynamic sites (1)"))
      (is (str/includes? output "(rf/dispatch [event-kw])")))))

(deftest empty-sections-omitted-test
  (let [output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {:unused-subs [kw-item]})]}])]
    (testing "empty sections are not rendered"
      (is (not (str/includes? output "## Unused events")))
      (is (not (str/includes? output "## Phantom subs"))))))

(deftest summary-table-test
  (let [output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {:unused-subs [kw-item]
                                                         :unused-events [event-item]})]}])]
    (testing "summary table is present"
      (is (str/includes? output "## Summary"))
      (is (str/includes? output "| Category | Count |")))
    (testing "summary reflects counts"
      (is (str/includes? output "| Unused subscriptions: | 1 |"))
      (is (str/includes? output "| Unused events: | 1 |")))))

(deftest multiple-groups-test
  (let [output (run-report
                [re-frame/group spade/group]
                [{:source-dir "src"
                  :group-results [(re-frame-result {:unused-subs [kw-item]})
                                  {:unused-styles [style-item]}]}])]
    (testing "renders sections from both groups"
      (is (str/includes? output "## Unused subs (1)"))
      (is (str/includes? output "## Unused styles (1)")))
    (testing "summary includes both groups"
      (is (str/includes? output "| Unused styles: | 1 |")))))

(deftest multiple-source-dirs-test
  (let [item-a {:kw :a/sub
                :file "src/a.cljs"
                :row 1
                :type :sub}
        item-b {:kw :b/sub
                :file "other/b.cljs"
                :row 2
                :type :sub}
        output (run-report [{:source-dir "src"
                             :group-results [(re-frame-result {:unused-subs [item-a]})]}
                            {:source-dir "other"
                             :group-results [(re-frame-result {:unused-subs [item-b]})]}])]
    (testing "merges results from multiple source dirs"
      (is (str/includes? output "## Unused subs (2)"))
      (is (str/includes? output "`:a/sub`"))
      (is (str/includes? output "`:b/sub`")))))
