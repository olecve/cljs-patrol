(ns cljs-patrol.groups.idioms-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.idioms :as idioms]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.zip :as z]))

(def ^:private ns-info
  {:ns-name "app.views"
   :aliases {"str" "clojure.string"
             "lib" "some.lib"
             "util" "app.util"}
   :refers {"helper" "app.util"
            "subscribe" "re-frame.core"}})

(defn- parse
  "Walk `lines` the way the parser walks a file, and return the group's result map."
  ([lines] (parse idioms/group lines))
  ([rule-group lines]
   (let [{:keys [handle-list]} (group/parse-handlers rule-group)
         declarations (loop [loc (z/of-string (str/join "\n" lines) {:track-position? true})
                             found []]
                        (if (z/end? loc)
                          found
                          (recur (z/next loc)
                                 (if (#{:list :fn} (z/tag loc))
                                   (into found (:decls (handle-list loc ns-info "views.cljs")))
                                   found))))]
     (group/analyze rule-group {:declarations declarations}))))

(defn- rewrites
  ([lines] (rewrites idioms/group lines))
  ([rule-group lines] (mapv :rewrite (:nested-call-chain (parse rule-group lines)))))

(deftest chain-of-three-core-calls-is-flagged
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(vec (sort (distinct xs)))"]))))

(deftest chain-of-two-calls-is-left-alone
  (is (empty? (rewrites ["(vec (sort xs))"]))))

(deftest keyword-interop-and-string-heads-count
  (is (= ["(-> state :value first :text)"
          "(-> props .-table .getFilteredRowModel .-rows)"
          "(-> e .-target .-textContent str/blank?)"
          "(-> xs first str java.util.UUID/fromString)"
          "(-> options clj->js Auth0Client. js/Promise.resolve)"
          "(-> xs distinct sort clojure.core/vec)"]
         (rewrites ["(:text (first (:value state)))"
                    "(.-rows (.getFilteredRowModel (.-table props)))"
                    "(str/blank? (.-textContent (.-target e)))"
                    "(java.util.UUID/fromString (str (first xs)))"
                    "(js/Promise.resolve (Auth0Client. (clj->js options)))"
                    "(clojure.core/vec (sort (distinct xs)))"]))))

(deftest project-function-counts-and-project-macro-does-not
  (is (= ["(-> x prepare prepare prepare)"]
         (rewrites ["(defn prepare [x] x)"
                    "(defmacro wrap [x] x)"
                    "(prepare (prepare (prepare x)))"
                    "(wrap (wrap (wrap x)))"]))))

(deftest project-definition-shadowing-core-decides
  (is (empty? (rewrites ["(defmacro first [x] x)"
                         "(first (vec (sort xs)))"]))
      "a namespace's own first is a macro, so only vec and sort are left"))

(deftest def-like-call-inside-a-body-defines-nothing
  (is (= ["(-> xs first first first)"]
         (rewrites ["(defn view [] (default-value first))"
                    "(first (first (first xs)))"]))))

(deftest unknown-and-library-heads-end-chain
  (doseq [[line description] [["(f (g (h x)))" "a local or an undefined name may be a macro"]
                              ["(lib/f (lib/g (lib/h x)))" "a library the scan cannot read may define a macro"]
                              ["(some.lib/f (some.lib/g (some.lib/h x)))" "a lower-case qualifier is a namespace, not a class"]
                              ["(subscribe (subscribe (subscribe x)))" "a referred library var is a library var"]
                              ["(helper (helper (helper x)))" "a referred project var counts only once its defn is seen"]]]
    (is (empty? (rewrites [line]))
        description)))

(deftest call-with-more-than-one-argument-ends-chain
  (is (empty? (rewrites ["(vec (sort (map f xs)))"
                         "(vec (sort-by f (distinct xs)))"]))))

(deftest wrapper-around-chain-is-not-counted
  (is (empty? (rewrites ["(not (str/blank? (:description m)))"]))
      "not leaves two calls")
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(not (vec (sort (distinct xs))))"]))
      "the chain inside the wrapper is still reported, without it"))

(deftest call-on-literal-stays-the-starting-value
  (is (empty? (rewrites ["(:question (:params (request-to \"/qa\")))"
                         "(dec (dec (count \"_unstem\")))"
                         "(:id (first (vals {:a 1})))"
                         "(:id (first (first [:plan :items])))"]))
      "two steps are left above the call")
  (is (= ["(-> (str/lower-case \"A\") str/trim keyword name)"]
         (rewrites ["(name (keyword (str/trim (str/lower-case \"A\"))))"]))))

(deftest threaded-step-takes-an-argument-it-does-not-show
  (is (empty? (rewrites ["(doto cell (.setCellValue (double (inc row-idx))))"
                         "(-> x (vec (sort (distinct ys))))"])))
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(-> (vec (sort (distinct xs))) count)"]))
      "the first argument of a thread is not a step"))

(deftest reader-conditional-list-is-not-a-call
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["#?(:clj (vec (sort (distinct xs))))"]))))

(deftest comment-or-discard-in-chain-ends-it
  (is (empty? (rewrites ["(vec (sort ; why"
                         "           (distinct xs)))"
                         "(vec (sort #_old (distinct xs)))"]))))

(deftest unevaluated-forms-are-left-alone
  (is (empty? (rewrites ["'(vec (sort (distinct xs)))"
                         "`(vec (sort (distinct xs)))"
                         "(quote (vec (sort (distinct xs))))"
                         "(comment (vec (sort (distinct xs))))"]))))

(deftest only-longest-chain-is-reported
  (is (= ["(-> xs vec vec vec vec)"] (rewrites ["(vec (vec (vec (vec xs))))"]))))

(deftest min-calls-is-configurable
  (is (= ["(-> xs sort vec)"] (rewrites (idioms/make-group {:min-calls 2}) ["(vec (sort xs))"])))
  (doseq [written [1 "3" nil]]
    (is (empty? (rewrites (idioms/make-group {:min-calls written}) ["(vec (sort xs))"]))
        (str (pr-str written) " is not a usable minimum, so the default of 3 applies"))))

(deftest finding-names-its-definition-and-shortens-its-hint
  (let [finding (first (:nested-call-chain
                        (parse ["(defn view []"
                                "  (vec (sort (distinct (concat a-collection-with-a-very-long-name and-another-one)))))"])))]
    (is (= "view" (:test finding)))
    (is (= 2 (:row finding)))
    (is (str/ends-with? (:hint finding) "... distinct sort vec)")
        "a long seed is shortened in the hint")
    (is (str/includes? (:rewrite finding) "and-another-one")
        "the rewrite carries the seed in full")))

(deftest identity-reads-back-from-edn
  (let [finding (first (:nested-call-chain (parse ["(vec (sort (distinct xs)))"])))
        identity (baseline/issue->identity :nested-call-chain finding)]
    (is (= identity (edn/read-string (pr-str identity))))))
