(ns cljs-patrol.groups.comments-test
  (:require
   [cljs-patrol.groups.comments :as comments]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- findings-in [source]
  (->> (temp-project/run-group comments/group {"app/core.cljs" source})
       :results
       :commented-out-code
       (mapv (juxt :row :form))))

(defn- commented-out-code
  "Analyze `lines` after an ns form requiring `api` and `rf`, and return the [row form] of each finding."
  [lines]
  (->> lines
       (cons "(ns app.core (:require [api.core :as api] [re-frame.core :as rf]))")
       (str/join "\n")
       findings-in))

(defn- separate [comments] (interpose "" comments))

(deftest code-in-comment-is-flagged
  (is (= [[3 "(rf/dispatch [::load id])"]
          [4 "(js/console.log \"debug\" x) [:span (tr :title)]"]
          [7 "(defn old-view [] [:div \"old\"])"]
          [11 "(when (open? x) [modal {:on-close close}])"]]
         (commented-out-code ["(defn view []"
                              "  ;; (rf/dispatch [::load id])"
                              "   ;(js/console.log \"debug\" x)"
                              "   ;; [:span (tr :title)]"
                              "  [:div])"
                              ";; (defn old-view []"
                              ";;   [:div \"old\"])"
                              ""
                              "(def x"
                              "  ;; (when (open? x)"
                              "  ;;   [modal {:on-close close}])"
                              "  1)"]))
      "reads block of lines at one column holding code call"))

(deftest each-code-signal-is-read
  (is (= [[2 "(swap! state f)"]
          [4 "(when (nil? x) (render))"]
          [6 "(-> x (render))"]
          [8 "(= 3 x)"]
          [10 "(let [x 1] (render x))"]
          [12 "(api/save x)"]
          [14 "(js/alert x)"]
          [16 "(map #(render %) xs)"]
          [18 "(matches #\"x\" s)"]
          [20 "(render ::x)"]]
         (commented-out-code (separate [";; (swap! state f)"
                                        ";; (when (nil? x) (render))"
                                        ";; (-> x (render))"
                                        ";; (= 3 x)"
                                        ";; (let [x 1] (render x))"
                                        ";; (api/save x)"
                                        ";; (js/alert x)"
                                        ";; (map #(render %) xs)"
                                        ";; (matches #\"x\" s)"
                                        ";; (render ::x)"])))
      "bang or predicate head with arguments, operator head, code head, alias call, fn literal, regex, auto-resolved keyword"))

(deftest finding-points-at-first-form-of-block
  (is (= [[3 "(prn (x))"]]
         (commented-out-code [";; ;; Prose kept from an older comment"
                              ";; (prn (x))"]))
      "skips leading line that is itself comment"))

(deftest blocks-opening-alike-stay-apart
  (is (= [[2 "(bench [] (render! x))"] [4 "(bench [] (render! y))"]]
         (commented-out-code [";; (bench [] (render! x))"
                              ""
                              ";; (bench [] (render! y))"]))
      "keeps whole block as form"))

(deftest prose-in-parentheses-is-left-alone
  (is (empty? (commented-out-code (separate [";; (returns (nil) when empty)"
                                             ";; (deprecated (use new-fn))"
                                             ";; (calls rf/dispatch twice)"
                                             ";; (wraps js/setTimeout)"
                                             ";; (read/write (access))"
                                             ";; (yes/no)"
                                             ";; (follow-up (later))"
                                             ";; (built-in {:x 1})"
                                             ";; (default 300)"
                                             ";; (only :app/dev)"
                                             ";; (unknown/alias x)"
                                             ";; (should we cache this?)"
                                             ";; (works for now!)"
                                             ";; (hack!)"
                                             ";; (draft -> review -> done)"
                                             ";; (n > 0)"
                                             ";; (calls `save` twice)"
                                             ";; (it's 'quoted' text)"
                                             ";; (cc @bob)"
                                             ";; (render @state)"
                                             ";; (render #{x})"
                                             ";; (api/first ,,,)"
                                             ";; (use let here (later))"
                                             ";; (try (again))"
                                             ";; [root]"
                                             ";; (child [leaf])"])))
      "nested form, slash pair, unknown alias, or qualified symbol off call head"))

(deftest each-strict-reading-condition-leaves-prose-alone
  (is (empty? (commented-out-code (separate [";; then call (save! x)"
                                             ";; [:div @x]"
                                             ";; (1 @x)"
                                             ";; [:dispatch [::kw args...]]"
                                             ";; (save! … x)"
                                             ";; {:ms N :dispatch (swap! x)}"
                                             ";; (swap! x) ;=> 2"
                                             ";; (see (swap! x))"
                                             ";; (e.g (swap! x))"
                                             ";; (cf. (swap! x))"
                                             ";; (RFC 7231 (swap! x))"
                                             ";; (unbalanced (swap! x)"
                                             "(defn f [x] (inc x)) ; (swap! x f)"])))
      "word outside form, no call, placeholder, REPL result, prose word, capital, unparsable, or trailing"))

(deftest example-a-comment-introduces-is-left-alone
  (is (empty? (commented-out-code [";; Example:"
                                   ""
                                   ";;   (swap! state f)"]))
      "skips example after comment ending in colon")
  (is (= [[5 "(swap! state f)"]]
         (commented-out-code [";; Steps:"
                              ""
                              ""
                              ";; (swap! state f)"]))
      "reads code further from colon"))

(deftest comment-opening-file-is-read
  (is (= [[1 "(prn (x))"]]
         (findings-in ";; (prn (x))\n(ns app.core)"))
      "reads comment nothing precedes"))

(deftest repl-code-in-comment-form-is-left-alone
  (is (empty? (commented-out-code ["(comment"
                                   "  ;; (reset! state {:x 1})"
                                   "  (prn 1))"]))
      "comment form holds REPL code on purpose"))
