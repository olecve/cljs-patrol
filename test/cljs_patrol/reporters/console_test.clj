(ns cljs-patrol.reporters.console-test
  (:require
   [cljs-patrol.reporters.console :as console]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private key->title #'console/key->title)

(deftest key->title-test
  (is (= "Unused subs" (key->title :unused-subs)))
  (is (= "Phantom events" (key->title :phantom-events)))
  (is (= "Dynamic sites" (key->title :dynamic-sites)))
  (is (= "Duplicate subs" (key->title :duplicate-subs))))

(def ^:private kw-item {:kw :my-ns/my-sub
                        :file "src/subs.cljs"
                        :row 5})
(def ^:private form-item {:form "(rf/dispatch [::my-event])"
                          :file "src/views.cljs"
                          :row 12})

(def ^:private kw-item-with-hint (assoc kw-item :hint "Drop :aria-live, or set it to \"assertive\"."))
(def ^:private form-item-with-hint (assoc form-item :hint "Drop :aria-live, or set it to \"assertive\"."))

(deftest report-renders-hints-test
  (testing "prints a finding's hint under the finding, on both section shapes"
    (let [kw-out (with-out-str (console/report {:unused-subs [kw-item-with-hint]}))
          form-out (with-out-str (console/report {:dynamic-sites [form-item-with-hint]}))]
      (is (str/includes? kw-out "Drop :aria-live, or set it to \"assertive\".")
          "kw-keyed sections render hints too, not only form-keyed ones")
      (is (str/includes? form-out "Drop :aria-live, or set it to \"assertive\".")
          "form-keyed sections render hints")))

  (testing "prints nothing extra for a finding with no hint"
    (let [out (with-out-str (console/report {:unused-subs [kw-item]}))]
      (is (not (str/includes? out "\u2192"))
          "no arrow line when the rule attached no hint")))

  (testing "prints hints in the baseline report as well"
    (let [out (with-out-str
                (console/report-with-baseline {:unused-subs [kw-item-with-hint]} #{}))]
      (is (str/includes? out "Drop :aria-live, or set it to \"assertive\".")
          "a baselined finding still says what to do about it"))))

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
  (let [new-item {:kw :app/new-sub
                  :type :sub
                  :file "src/new.cljs"
                  :row 1}
        old-item {:kw :app/old-sub
                  :type :sub
                  :file "src/old.cljs"
                  :row 2}
        new-ids #{{:rule :unused-subs
                   :key :app/new-sub}}
        result {:unused-subs [new-item old-item]}
        out (with-out-str (console/report-with-baseline result new-ids))]
    (is (str/includes? out "[NEW]")
        "tags new issues")
    (is (str/includes? out "[BASE]")
        "tags baseline issues")
    (is (str/includes? out ":app/new-sub"))
    (is (str/includes? out ":app/old-sub"))))

(deftest report-with-baseline-when-dynamic-test
  (let [new-dynamic {:form "(rf/dispatch [x])"
                     :file "a.cljs"
                     :row 1}
        old-dynamic {:form "(rf/subscribe [y])"
                     :file "b.cljs"
                     :row 2}
        new-ids #{{:rule :dynamic-sites
                   :form "(rf/dispatch [x])"
                   :file "a.cljs"
                   :line 1}}
        result {:dynamic-sites [new-dynamic old-dynamic]}
        out (with-out-str (console/report-with-baseline result new-ids))]
    (is (str/includes? out "[NEW]")
        "tags new dynamic site")
    (is (str/includes? out "[BASE]")
        "tags baseline dynamic site")))

(deftest report-with-baseline-when-quiet-test
  (let [new-item {:kw :app/new-sub
                  :type :sub
                  :file "src/new.cljs"
                  :row 1}
        old-item {:kw :app/old-sub
                  :type :sub
                  :file "src/old.cljs"
                  :row 2}
        new-ids #{{:rule :unused-subs
                   :key :app/new-sub}}
        result {:unused-subs [new-item old-item]}
        out (with-out-str (console/report-with-baseline result new-ids {:quiet? true}))]
    (is (str/includes? out ":app/new-sub")
        "shows new issues")
    (is (not (str/includes? out ":app/old-sub"))
        "suppresses baseline issues")))

(deftest report-with-baseline-when-quiet-all-baseline-test
  (let [old-item {:kw :app/old-sub
                  :type :sub
                  :file "src/old.cljs"
                  :row 2}
        result {:unused-subs [old-item]}
        out (with-out-str (console/report-with-baseline result #{} {:quiet? true}))]
    (is (= "" out)
        "no output when all issues are baseline and quiet")))

(deftest report-marks-blocking-sections-test
  (let [bug-item {:kw :app/dup
                  :file "subs.cljs"
                  :row 1}
        cleanup-item {:kw :app/unused
                      :file "subs.cljs"
                      :row 2}
        result {:duplicate-subs [bug-item]
                :unused-subs [cleanup-item]}
        fail-on-rules #{:duplicate-subs}
        out (with-out-str (console/report result fail-on-rules))]
    (is (str/includes? out "Duplicate subs (1) [BLOCKING]")
        "blocking rule's section is marked")
    (is (re-find #"Unused subs \(1\) ===" out)
        "non-blocking rule's section is unmarked")))

(deftest report-without-fail-on-omits-blocking-marker-test
  (let [item {:kw :app/dup
              :file "subs.cljs"
              :row 1}
        result {:duplicate-subs [item]}
        out (with-out-str (console/report result))]
    (is (not (str/includes? out "[BLOCKING]"))
        "no [BLOCKING] marker when fail-on-rules is unset")))

(deftest report-with-baseline-marks-blocking-test
  (let [item {:kw :app/dup
              :file "subs.cljs"
              :row 1}
        result {:duplicate-subs [item]}
        out (with-out-str
              (console/report-with-baseline result #{}
                                            {:fail-on-rules #{:duplicate-subs}}))]
    (is (str/includes? out "[BLOCKING]")
        "blocking marker applied in baseline mode too")))
