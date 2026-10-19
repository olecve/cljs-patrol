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
             "util" "app.util"
             "M" "some.lib"}
   :refers {"helper" "app.util"
            "subscribe" "re-frame.core"
            "lower-case" "clojure.string"
            "trim" "clojure.string"}})

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
          "(-> e .-target .-textContent str/trim)"
          "(-> xs first str java.util.UUID/fromString)"
          "(-> options clj->js Auth0Client. js/Promise.resolve)"
          "(-> xs distinct sort clojure.core/vec)"]
         (rewrites ["(:text (first (:value state)))"
                    "(.-rows (.getFilteredRowModel (.-table props)))"
                    "(str/trim (.-textContent (.-target e)))"
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
                              ["(M/f (M/g (M/h x)))" "a capitalized alias is still an alias, not a class"]
                              ["(subscribe (subscribe (subscribe x)))" "a referred library var is a library var"]
                              ["(helper (helper (helper x)))" "a referred project var counts only once its defn is seen"]]]
    (is (empty? (rewrites [line]))
        description)))

(deftest referred-string-function-counts
  (is (= ["(-> x trim trim lower-case)"] (rewrites ["(lower-case (trim (trim x)))"]))))

(deftest multimethod-counts-and-its-methods-define-nothing
  (is (= ["(-> x prepare prepare prepare)" "(-> x render render render)"]
         (rewrites ["(defmulti prepare identity)"
                    "(defmethod prepare :a [x] x)"
                    "(defn render [x] x)"
                    "(defmethod util/render :b [x] x)"
                    "(prepare (prepare (prepare x)))"
                    "(render (render (render x)))"]))))

(deftest var-defined-as-a-macro-anywhere-does-not-count
  (is (empty? (rewrites ["(defmacro twice [x] x)"
                         "(defn twice [x] x)"
                         "(twice (twice (twice x)))"]))
      "a .clj macro and a .cljs function of one name read together, in either order"))

(deftest comment-or-discard-in-place-of-the-argument-is-not-a-call
  (is (empty? (rewrites ["(defn f [] (reset-all! ; clears everything"
                         "))"
                         "(vec (sort (distinct #_xs)))"]))))

(deftest rewrite-keeps-the-starting-value-as-written
  (let [[spaced commented spanning] (:nested-call-chain
                                     (parse ["(vec (sort (distinct (str/split \"a  ( b\" #\"  \"))))"
                                             "(vec (sort (distinct (concat a ; why"
                                             "                             b))))"
                                             "(vec (sort (distinct (f a"
                                             "                        b))))"]))]
    (is (= "(-> (str/split \"a  ( b\" #\"  \") distinct sort vec)" (:rewrite spaced))
        "strings and regexes keep their spaces")
    (is (str/includes? (:hint spaced) "\"a  ( b\"")
        "the hint shows the same text")
    (is (str/includes? (:rewrite commented) "; why\n")
        "a comment keeps the line break that ends it")
    (is (str/includes? (:hint spanning) "(-> ... distinct sort vec)")
        "a starting value spanning lines is not squeezed onto the hint's one line")))

(deftest data-last-call-threads-with-thread-last
  (is (= ["(->> xs (map g) (filter f) vec)"
          "(->> m keys (map name) (str/join \", \"))"]
         (rewrites ["(vec (filter f (map g xs)))"
                    "(str/join \", \" (map name (keys m)))"]))))

(deftest data-first-call-threads-with-thread-first
  (is (= ["(-> m (get-in [:a]) first (select-keys [:b]))"
          "(-> document .createParagraph .createRun (.setText text))"]
         (rewrites ["(select-keys (first (get-in m [:a])) [:b])"
                    "(.setText (.createRun (.createParagraph document)) text)"]))))

(deftest call-outside-the-data-slot-ends-chain
  (is (= ["(-> ks distinct sort vec)"] (rewrites ["(get-in db (vec (sort (distinct ks))))"]))
      "the nested call is the path, not the map get-in reads")
  (is (empty? (rewrites ["(vec (sort (map f xs ys)))"
                         "(vec (distinct (map f)))"
                         "(vec (sort (js/Math.max (count xs) 1)))"]))
      "two collections, a transducer, and a host call that is not a method have no data slot"))

(deftest transducer-argument-is-not-the-data
  (is (= ["(->> xs distinct sort (into [] (map f)))"] (rewrites ["(into [] (map f) (sort (distinct xs)))"]))))

(deftest change-of-direction-reports-the-longest-part-once
  (is (= ["(->> xs (remove h) (filter g) (map f) sort first)"]
         (rewrites ["(get (first (sort (map f (filter g (remove h xs))))) :k)"]))
      "one-argument steps at the turn join the part below, and the part above is too short")
  (is (= ["(-> m (get-in [:a]) (assoc :b 1) (dissoc :c))"]
         (rewrites ["(map f (dissoc (assoc (get-in m [:a]) :b 1) :c))"]))
      "only the longer part is reported, so two threads are never suggested for one form")
  (is (= ["(->> xs (map h) (filter g) sort vec)"]
         (rewrites ["(select-keys (dissoc (assoc (vec (sort (filter g (map h xs)))) :a 1) :b) [:c])"]))
      "the longer part wins wherever it sits")
  (is (= ["(->> xs (map h) (filter g) vec)"]
         (rewrites ["(select-keys (dissoc (assoc (vec (filter g (map h xs))) :a 1) :b) [:c])"]))
      "on a tie the innermost part wins"))

(deftest predicate-at-the-top-is-not-counted
  (is (empty? (rewrites ["(contains? (set (map :id xs)) id)"
                         "(pos? (count (filter f xs)))"
                         "(some #{x} (map f (vals m)))"])))
  (is (= ["(->> xs (filter f) (map :id) set)"]
         (rewrites ["(contains? (set (map :id (filter f xs))) id)"]))
      "the part below the predicate still counts"))

(deftest local-named-like-core-gets-no-data-slot
  (is (empty? (rewrites ["(defn f [{:keys [filter]}] (vec (filter g (map h xs))))"
                         "(let [update (fn [m k v] m)] (count (update (vec (sort xs)) :a inc)))"
                         "(letfn [(merge [a b] a)] (count (merge (vec (sort xs)) b)))"]))
      "a destructured, let-bound or letfn-bound name is a local")
  (is (= ["(->> xs (filter g) (map h) vec)"]
         (rewrites ["(defn f [filtered] (vec (map h (filter g xs))))"]))
      "a local with a different name changes nothing")
  (is (empty? (rewrites ["(for [x xs :let [filter f]] (vec (filter g (map h x))))"]))
      "a :let modifier binds too")
  (is (= ["(->> a (map h) (filter g) vec)"]
         (rewrites ["(let [a (filter f xs)] (vec (filter g (map h a))))"]))
      "a name used in a binding's value is not bound by it")
  (is (= ["(->> xs (filter g) (map h) vec)" "(->> ys (map f) distinct sort vec)"]
         (rewrites ["(defn view [x] [:div (filter x)] (vec (map h (filter g xs))))"
                    "(defn build [x] (let [a (conj x 1)] (vec (sort (distinct (map f ys))))))"]))
      "a vector in a body and a let below are not parameter lists")
  (is (empty? (rewrites ["(defn many ([filter] (vec (filter g (map h xs)))) ([a b] a))"
                         "(defmethod render :x [filter] (vec (filter g (map h xs))))"
                         "(defmethod render [:a :b] [filter] (vec (filter g (map h xs))))"]))
      "each arity's parameters, and a method's after its dispatch value, are read"))

(deftest member-access-macro-is-not-a-method
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(.. (vec (sort (distinct xs))) getA getB)"]))))

(deftest quoted-and-tagged-literals-stay-the-starting-value
  (is (empty? (rewrites ["(vec (sort (distinct '(3 1 2))))"
                         "(vec (sort (distinct #js [1 2])))"]))))

(deftest collection-literal-starts-a-thread-last
  (is (= ["(->> [3 1 2] (map inc) sort vec)"
          "(->> '(3 1 2) (map inc) sort vec)"]
         (rewrites ["(vec (sort (map inc [3 1 2])))"
                    "(vec (sort (map inc '(3 1 2))))"]))))

(deftest finding-names-the-operator-it-suggests
  (is (= ['->> '->]
         (map :kw (:nested-call-chain (parse ["(vec (filter f (map g xs)))"
                                              "(vec (sort (distinct xs)))"]))))))

(deftest call-of-more-arguments-counts-only-for-a-known-slot
  (is (= ["(-> xs distinct sort first)"]
         (rewrites ["(defn pick [m k] (get m k))"
                    "(pick (first (sort (distinct xs))) :a)"]))
      "a project function's data slot is not known")
  (is (empty? (rewrites ["(defn map [f xs] xs)"
                         "(vec (map f (filter g xs)))"]))
      "a namespace's own map is not core map"))

(deftest long-arguments-are-shortened-in-the-hint
  (let [finding (first (:nested-call-chain
                        (parse ["(vec (filter some-very-long-predicate-name-that-goes-past-the-limit (map g xs)))"])))]
    (is (str/includes? (:hint finding) "(filter ...)"))
    (is (str/includes? (:rewrite finding) "some-very-long-predicate-name-that-goes-past-the-limit"))))

(deftest wrapper-below-a-macro-is-not-counted
  (is (empty? (rewrites ["(is (nil? (:id (first xs))))"]))
      "is ends the chain, and nil? still tops what would be threaded")
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(lib/m (seq (vec (sort (distinct xs)))))"]))))

(deftest wrapper-around-chain-is-not-counted
  (is (empty? (rewrites ["(not (str/blank? (:description m)))"]))
      "not leaves two calls")
  (is (= ["(-> xs distinct sort vec)"] (rewrites ["(not (vec (sort (distinct xs))))"]))
      "the chain inside the wrapper is still reported, without it"))

(deftest call-on-literal-stays-the-starting-value
  (is (empty? (rewrites ["(defn request-to [path] path)"
                         "(:question (:params (request-to \"/qa\")))"
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
      "the first argument of a thread is not a step")
  (is (= ["(-> $ distinct sort vec)"] (rewrites ["(as-> x $ (vec (sort (distinct $))))"]))
      "an as-> step writes its argument out")
  (is (= ["(-> xs seq next rest)"] (rewrites ["(cond-> m (empty? (rest (next (seq xs)))) (vec (sort (distinct ys))))"]))
      "a cond-> test is not threaded, and its step is")
  (is (= ["(-> xs next rest count)"] (rewrites ["(cond-> m a? inc (count (rest (next xs))) dec)"]))
      "every test is read, not only the first"))

(deftest function-literal-chain-is-flagged
  (is (= ["#(-> % distinct sort vec)" "#(-> % distinct sort vec)"]
         (rewrites ["#(vec (sort (distinct %)))"
                    "(g #(vec (sort (distinct %))))"]))))

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

(deftest identity-separates-chains-differing-past-their-start
  (let [found (:nested-call-chain
               (parse ["(defn view []"
                       "  [(str/lower-case (str/trim (name (:some-long-keyword-name entity-aaaaaaa))))"
                       "   (str/lower-case (str/trim (name (:some-long-keyword-name entity-bbbbbbb))))])"]))]
    (is (= 2 (->> found (map #(baseline/issue->identity :nested-call-chain %)) set count)))))

(deftest suggestion-names-the-configured-minimum
  (is (str/starts-with? (:nested-call-chain (group/suggestions (idioms/make-group {:min-calls 2}))) "2 or more")))

(deftest identity-reads-back-from-edn
  (let [finding (first (:nested-call-chain (parse ["(vec (sort (distinct xs)))"])))
        identity (baseline/issue->identity :nested-call-chain finding)]
    (is (= identity (edn/read-string (pr-str identity))))))

(defn- else-nils [lines]
  (->> lines parse :cond-else-nil (mapv :form)))

(deftest cond-closing-on-else-nil-is-flagged
  (is (= ["(cond (pos? n) :up :else nil)"
          "(cond (pos? n) :up true nil)"
          "(cond (pos? n) :up :default nil)"
          "(clojure.core/cond (pos? n) :up :else nil)"
          "(cond (pos? n) :up (neg? n) :down :else nil)"
          "#(cond (pos? %) :up :else nil)"
          "(cond (pos? n) :up ;; falls through :else nil)"
          "(cond (pos? n) :up #_(neg? n) :else nil)"]
         (else-nils ["(cond (pos? n) :up :else nil)"
                     "(cond (pos? n) :up true nil)"
                     "(cond (pos? n) :up :default nil)"
                     "(clojure.core/cond (pos? n) :up :else nil)"
                     "(cond (pos? n) :up\n      (neg? n) :down\n      :else nil)"
                     "#(cond (pos? %) :up :else nil)"
                     "(cond (pos? n) :up\n      ;; falls through\n      :else nil)"
                     "(cond (pos? n) :up #_(neg? n) :else nil)"]))))

(deftest else-nil-finding-names-the-clause-and-where-it-is
  (let [[finding] (:cond-else-nil (parse ["(defn sign [n]" "  (cond (pos? n) :up :default nil))"]))]
    (is (= {:kw 'cond
            :type :cond-else-nil
            :file "views.cljs"
            :row 2
            :col 3
            :test "sign"
            :hint "Drop the :default nil clause: a cond no clause matches returns nil"}
           (dissoc finding :form)))))

(deftest cond-with-a-real-default-is-left-alone
  (is (empty? (else-nils ["(cond (pos? n) :up :else :flat)"
                          "(cond (pos? n) :up :else (nil-default))"
                          "(cond (pos? n) :up flag nil)"
                          "(cond :else nil)"
                          "(cond (pos? n) :up :else nil :odd)"
                          "(cond (pos? n) :up (neg? n) :else nil)"
                          "(cond (pos? n) :up #?(:clj :else) nil)"
                          "(cond (pos? n) :up :else #?(:clj nil))"]))))

(deftest empty-list-is-left-alone
  (is (empty? (else-nils ["()" "#()" "[()]"]))))

(deftest unreadable-last-clause-is-left-alone
  (is (empty? (else-nils ["(cond (pos? n) :up ^:m :else nil)"
                          "(cond (pos? n) :up :else ^:m nil)"]))))

(deftest condp-and-case-are-left-alone
  (is (empty? (else-nils ["(condp = n 1 :one nil)"
                          "(case n 1 :one :else nil)"]))))

(deftest cond-from-a-library-is-left-alone
  (is (empty? (else-nils ["(lib/cond (pos? n) :up :else nil)"]))))

(deftest locally-bound-cond-is-left-alone
  (is (empty? (else-nils ["(let [cond pick] (cond (pos? n) :up :else nil))"]))))

(deftest cond-the-namespace-defines-is-left-alone
  (is (empty? (else-nils ["(defmacro cond [& clauses] clauses)"
                          "(cond (pos? n) :up :else nil)"]))))

(deftest unevaluated-cond-is-left-alone
  (is (empty? (else-nils ["'(cond (pos? n) :up :else nil)"
                          "(comment (cond (pos? n) :up :else nil))"
                          "#_(cond (pos? n) :up :else nil)"]))))
