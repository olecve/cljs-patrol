(ns cljs-patrol.reporters.console-test
  (:require
   [cljs-patrol.reporters.console :as console]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private kw-item {:kw :my-ns/my-sub
                        :file "src/subs.cljs"
                        :row 5})
(def ^:private form-item {:form "(rf/dispatch [::my-event])"
                          :file "src/views.cljs"
                          :row 12})

(defn- report-lines
  ([findings] (report-lines findings {}))
  ([findings options] (str/split-lines (with-out-str (console/print-report findings options)))))

(deftest finding-message-test
  (is (= ":my-ns/my-sub" (:message (console/finding :unused-subs "error" kw-item)))
      "name without hint shows name")
  (is (= "(rf/dispatch [::my-event])" (:message (console/finding :dynamic-sites "info" form-item)))
      "form without hint shows form")
  (is (= "Use a keyword." (:message (console/finding :dynamic-sites "info" (assoc form-item :hint "Use a keyword."))))
      "form's hint replaces form, since its line shows it")
  (is (= ":my-ns/my-sub: Remove it." (:message (console/finding :unused-subs "error" (assoc kw-item :hint "Remove it."))))
      "name's hint keeps name, since its line may not show it")
  (is (= "(is (some? (get-by-text \"x\")))"
         (:message (console/finding :dynamic-sites "info" (assoc form-item :form "(is (some?\n    (get-by-text \"x\")))"))))
      "form collapses onto one line")
  (is (= 80 (count (:message (console/finding :dynamic-sites "info" (assoc form-item :form (apply str (repeat 200 "x")))))))
      "long form truncated"))

(deftest print-report-groups-by-file-test
  (let [lines (report-lines [(console/finding :dynamic-sites "info" form-item)
                             (console/finding :unused-subs "error" (assoc kw-item :row 40))
                             (console/finding :unused-subs "error" kw-item)])]
    (is (= ["src/subs.cljs"
            "   5  error  :my-ns/my-sub  unused-subs"
            "  40  error  :my-ns/my-sub  unused-subs"]
           (->> lines (drop-while #(not= "src/subs.cljs" %)) (take 3)))
        "file header, then rows by line with aligned columns and rule id last")
    (is (= ["src/subs.cljs" "src/views.cljs"] (filter #{"src/subs.cljs" "src/views.cljs"} lines))
        "files in path order")))

(deftest print-report-aligns-severity-test
  (is (= ["  1  error    :a  unused-subs"
          "  2  warning  :b  phantom-subs"]
         (->> (report-lines [(console/finding :unused-subs "error" {:kw :a
                                                                    :file "a.cljs"
                                                                    :row 1})
                             (console/finding :phantom-subs "warning" {:kw :b
                                                                       :file "a.cljs"
                                                                       :row 2})])
              (drop-while #(not= "a.cljs" %))
              rest
              (take 2)))
      "message column starts at same place whatever severity"))

(deftest print-report-shows-column-test
  (is (some #{"  3:7  warning  (inc x)  plus-minus-one"}
            (report-lines [(console/finding :plus-minus-one "warning" {:form "(inc x)"
                                                                       :file "a.cljs"
                                                                       :row 3
                                                                       :col 7})]))
      "line:col when rule knows column"))

(deftest print-report-totals-test
  (is (some #{"\u2716 2 problems (1 error, 1 warning)"}
            (report-lines [(console/finding :unused-subs "error" kw-item)
                           (console/finding :unused-subs "warning" kw-item)]))
      "counts problems by severity")
  (is (some #{"\u2716 1 problem (0 errors, 0 warnings, 1 info)"}
            (report-lines [(console/finding :dynamic-sites "info" form-item)]))
      "info counted only when present")
  (is (some #{"\u2714 No problems"} (report-lines []))
      "clean run says so")
  (is (some #{"\u2716 1 problem (1 error, 0 warnings) \u00b7 1 in baseline, 0 fixed"}
            (report-lines [(console/finding :unused-subs "error" kw-item)
                           (console/finding :unused-subs "baseline" kw-item)]
                          {:baseline {:present 1
                                      :fixed 0}}))
      "baselined findings shown but not counted as problems")
  (is (some #{"\u2714 No new problems \u00b7 3 in baseline, 0 fixed"}
            (report-lines [] {:baseline {:present 3
                                         :fixed 0}}))
      "clean baseline run says so")
  (is (some #(str/starts-with? % "2 baseline issues no longer present")
            (report-lines [] {:baseline {:present 0
                                         :fixed 2}}))
      "fixed issues prompt baseline refresh"))

(deftest print-report-docs-test
  (let [lines (report-lines [(console/finding :unused-subs "error" kw-item)
                             (console/finding :dynamic-sites "info" form-item)]
                            {:docs {:unused-subs "https://example.com/re-frame.md#unused-subs"
                                    :phantom-events "https://example.com/re-frame.md#phantom-events"}})]
    (is (= ["Rule docs:"
            "  unused-subs  https://example.com/re-frame.md#unused-subs"]
           (drop-while #(not= "Rule docs:" %) lines))
        "one link per rule that fired, none for rules without docs or findings")))
