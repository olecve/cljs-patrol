(ns cljs-patrol.groups.comments-test
  (:require
   [cljs-patrol.groups.comments :as comments]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- commented-out-code
  "Analyze `lines` as the only file of a project, and return the [row form] of each finding."
  [lines]
  (->> (temp-project/run-group comments/group {"app/core.cljs" (str/join "\n" (cons "(ns app.core)" lines))})
       :results
       :commented-out-code
       (mapv (juxt :row :form))))

(deftest code-in-comment-is-flagged
  (is (= [[3 "(rf/dispatch [::load id])"]
          [4 "(js/console.log \"debug\" x) [:span (tr :title)]"]
          [7 "(defn old-view [] [:div \"old\"])"]
          [11 "(when open? [modal {:on-close close}])"]]
         (commented-out-code ["(defn view []"
                              "  ;; (rf/dispatch [::load id])"
                              "   ;(js/console.log \"debug\" x)"
                              "   ;; [:span (tr :title)]"
                              "  [:div])"
                              ";; (defn old-view []"
                              ";;   [:div \"old\"])"
                              ""
                              "(def a"
                              "  ;; (when open?"
                              "  ;;   [modal {:on-close close}])"
                              "  1)"]))
      "reads call with nested form or qualified symbol, and block of lines at one column"))

(deftest finding-points-at-first-form-of-block
  (is (= [[3 "(prn (x))"]]
         (commented-out-code [";; ;; Prose kept from an older comment"
                              ";; (prn (x))"]))
      "skips leading line that is itself comment"))

(deftest blocks-opening-alike-stay-apart
  (is (= [[2 "(bench [] (render x))"] [4 "(bench [] (render y))"]]
         (commented-out-code [";; (bench [] (render x))"
                              ""
                              ";; (bench [] (render y))"]))
      "keeps whole block as form"))

(defn- separate [comments] (interpose "" comments))

(deftest each-strict-reading-condition-leaves-prose-alone
  (is (empty? (commented-out-code (separate [";; (see the docs at app.core)"
                                             ";; (refresh)"
                                             ";; (PSE-598) — the launch stays blocked"
                                             ";; Usage: (my-fn {:a 1})"
                                             ";; [:div [:span \"x\"]]"
                                             ";; ([:x] [:y])"
                                             ";; (1 [:x])"
                                             ";; then call (save {:a 1})"
                                             ";; [:dispatch [::kw args...]]"
                                             ";; (save … (now))"
                                             ";; {:ms N :dispatch (dispatch [::kw])}"
                                             ";; (dispatch [::kw] ...)"
                                             ";; (foo (bar)) ;=> 2"
                                             ";; (unbalanced (x)"
                                             "(defn f [x] (inc x)) ; (dec x (y))"])))
      "word outside form, no call, non-symbol head, placeholder, REPL result, unparsable, or trailing"))

(deftest prose-in-parentheses-is-left-alone
  (is (empty? (commented-out-code (separate [";; (see section 3)"
                                             ";; (default 300)"
                                             ";; (e.g. \"foo\")"
                                             ";; (i.e. :keyword args)"
                                             ";; (only :dev)"
                                             ";; (TODO :perf)"
                                             ";; (RFC 7231)"
                                             ";; (Hello \"world\")"
                                             ";; (see foo/bar)"
                                             ";; (cf. app.core/save)"
                                             ";; (called by (render))"
                                             ";; (as of (now))"
                                             ";; (only :app/dev)"
                                             ";; (RFC 7231 (section 4))"])))
      "keyword, string or number alone, prose word, or capitalized word"))

(deftest example-a-comment-introduces-is-left-alone
  (is (empty? (commented-out-code [";; Example:"
                                   ""
                                   ";;   (my-fn {:a 1})"]))
      "skips example after comment ending in colon")
  (is (= [[5 "(prn (x))"]]
         (commented-out-code [";; Steps:"
                              ""
                              ""
                              ";; (prn (x))"]))
      "reads code further from colon"))

(deftest each-code-signal-is-read
  (is (= [[2 "(api/save)"]
          [4 "(save (now))"]
          [6 "(save [:a])"]
          [8 "(save {:a 1})"]]
         (commented-out-code (separate [";; (api/save)"
                                        ";; (save (now))"
                                        ";; (save [:a])"
                                        ";; (save {:a 1})"])))
      "qualified symbol or nested list, vector or map"))

(deftest comment-opening-file-is-read
  (is (= [[1 "(prn (x))"]]
         (->> (temp-project/run-group comments/group {"app/core.cljs" ";; (prn (x))\n(ns app.core)"})
              :results
              :commented-out-code
              (mapv (juxt :row :form))))
      "reads comment nothing precedes"))

(deftest repl-code-in-comment-form-is-left-alone
  (is (empty? (commented-out-code ["(comment"
                                   "  ;; (reset! state {:a 1})"
                                   "  (prn 1))"]))
      "comment form holds REPL code on purpose"))
