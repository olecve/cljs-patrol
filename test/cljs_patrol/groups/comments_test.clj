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
          [4 "(js/console.log \"debug\" x)"]
          [6 "(defn old-view []"]
          [9 ";; (prn 1)"]
          [12 "(when open?"]]
         (commented-out-code ["(defn view []"
                              "  ;; (rf/dispatch [::load id])"
                              "   ;(js/console.log \"debug\" x)"
                              "  [:div])"
                              ";; (defn old-view []"
                              ";;   [:div \"old\"])"
                              ""
                              ";; ;; (prn 1)"
                              ";; (inc 1)"
                              "(def a"
                              "  ;; (when open?"
                              "  ;;   [modal {:on-close close}])"
                              "  1)"]))
      "reads call with code signal, block of lines at one column, and commented comment"))

(defn- separate [comments] (interpose "" comments))

(deftest each-strict-reading-condition-leaves-prose-alone
  (is (empty? (commented-out-code (separate [";; (see the docs at app.core)"
                                             ";; (refresh)"
                                             ";; (PSE-598) — the launch stays blocked"
                                             ";; Usage: (my-fn {:a 1})"
                                             ";; [:div \"x\"]"
                                             ";; ([:a] :b)"
                                             ";; (1 :a)"
                                             ";; then call (save {:a 1})"
                                             ";; [:dispatch [::kw args...]]"
                                             ";; (save … :now)"
                                             ";; {:ms N :dispatch (dispatch [::kw])}"
                                             ";; (dispatch [::kw] ...)"
                                             ";; (foo 1) ;=> 2"
                                             ";; (unbalanced :a"
                                             "(defn f [x] (inc x)) ; (dec x 1)"])))
      "no code signal, word outside form, no call, non-symbol head, placeholder, REPL result, unparsable, or trailing"))

(deftest each-code-signal-is-read
  (is (= [[2 "(save :now)"]
          [4 "(save \"now\")"]
          [6 "(save 1)"]
          [8 "(api/save)"]
          [10 "(save (now))"]]
         (commented-out-code (separate [";; (save :now)"
                                        ";; (save \"now\")"
                                        ";; (save 1)"
                                        ";; (api/save)"
                                        ";; (save (now))"])))
      "keyword, string, number, qualified symbol, or nested form"))

(deftest comment-opening-file-is-read
  (is (= [[1 "(prn :x)"]]
         (->> (temp-project/run-group comments/group {"app/core.cljs" ";; (prn :x)\n(ns app.core)"})
              :results
              :commented-out-code
              (mapv (juxt :row :form))))
      "reads comment nothing precedes"))

(deftest repl-code-in-comment-form-is-left-alone
  (is (empty? (commented-out-code ["(comment"
                                   "  ;; (reset! state {:a 1})"
                                   "  (prn 1))"]))
      "comment form holds REPL code on purpose"))
