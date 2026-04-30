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

(deftest report-with-baseline-test
  (let [new-item {:kw :app/new-sub :type :sub :file "src/new.cljs" :row 1}
        old-item {:kw :app/old-sub :type :sub :file "src/old.cljs" :row 2}
        new-ids #{{:rule :unused-subs :key :app/new-sub}}
        result {:unused-subs [new-item old-item]}
        out (with-out-str (console/report-with-baseline result new-ids))]
    (is (str/includes? out "[NEW]")
        "tags new issues")
    (is (str/includes? out "[BASE]")
        "tags baseline issues")
    (is (str/includes? out ":app/new-sub"))
    (is (str/includes? out ":app/old-sub"))))

(deftest report-with-baseline-when-dynamic-test
  (let [new-dynamic {:form "(rf/dispatch [x])" :file "a.cljs" :row 1}
        old-dynamic {:form "(rf/subscribe [y])" :file "b.cljs" :row 2}
        new-ids #{{:rule :dynamic-sites :form "(rf/dispatch [x])" :file "a.cljs" :line 1}}
        result {:dynamic-sites [new-dynamic old-dynamic]}
        out (with-out-str (console/report-with-baseline result new-ids))]
    (is (str/includes? out "[NEW]")
        "tags new dynamic site")
    (is (str/includes? out "[BASE]")
        "tags baseline dynamic site")))

(deftest report-with-baseline-when-quiet-test
  (let [new-item {:kw :app/new-sub :type :sub :file "src/new.cljs" :row 1}
        old-item {:kw :app/old-sub :type :sub :file "src/old.cljs" :row 2}
        new-ids #{{:rule :unused-subs :key :app/new-sub}}
        result {:unused-subs [new-item old-item]}
        out (with-out-str (console/report-with-baseline result new-ids true))]
    (is (str/includes? out ":app/new-sub")
        "shows new issues")
    (is (not (str/includes? out ":app/old-sub"))
        "suppresses baseline issues")))

(deftest report-with-baseline-when-quiet-all-baseline-test
  (let [old-item {:kw :app/old-sub :type :sub :file "src/old.cljs" :row 2}
        result {:unused-subs [old-item]}
        out (with-out-str (console/report-with-baseline result #{} true))]
    (is (= "" out)
        "no output when all issues are baseline and quiet")))
