(ns cljs-patrol.reporters.console-test
  (:require
   [cljs-patrol.reporters.console :as console]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private key->title #'cljs-patrol.reporters.console/key->title)

(deftest key->title-test
  (is (= "Unused subs" (key->title :unused-subs)))
  (is (= "Phantom events" (key->title :phantom-events)))
  (is (= "Dynamic sites" (key->title :dynamic-sites)))
  (is (= "Duplicate subs" (key->title :duplicate-subs))))

(def ^:private kw-item {:kw :my-ns/my-sub :file "src/subs.cljs" :row 5})
(def ^:private form-item {:form "(rf/dispatch [::my-event])" :file "src/views.cljs" :row 12})

(deftest report-kw-sections-test
  (testing "prints kw-based sections using keyword format"
    (let [out (with-out-str
                (console/report {:unused-subs [kw-item]
                                 :phantom-events []}))]
      (is (str/includes? out "Unused subs"))
      (is (str/includes? out ":my-ns/my-sub"))
      (is (str/includes? out "src/subs.cljs"))
      (is (str/includes? out "Phantom events"))
      (is (str/includes? out "(none)"))))

  (testing "empty sections print (none)"
    (let [out (with-out-str
                (console/report {:unused-subs []}))]
      (is (str/includes? out "(none)"))))

  (testing "non-sequential values are skipped"
    (let [out (with-out-str
                (console/report {:unused-subs [kw-item]
                                 :some-scalar 42}))]
      (is (not (str/includes? out "42"))))))

(deftest report-form-sections-test
  (testing "prints form-based sections using form format"
    (let [out (with-out-str
                (console/report {:dynamic-sites [form-item]}))]
      (is (str/includes? out "Dynamic sites"))
      (is (str/includes? out "src/views.cljs"))
      (is (str/includes? out "(rf/dispatch [::my-event])")))))
