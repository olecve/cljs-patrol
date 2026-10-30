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
  ([rule-group lines] (parse rule-group ns-info lines))
  ([rule-group file-ns-info lines] (parse rule-group file-ns-info "views.cljs" lines))
  ([rule-group file-ns-info file lines]
   (let [{:keys [handle-list]} (group/parse-handlers rule-group)
         declarations (loop [loc (z/of-string (str/join "\n" lines) {:track-position? true})
                             found []]
                        (if (z/end? loc)
                          found
                          (recur (z/next loc)
                                 (if (#{:list :fn} (z/tag loc))
                                   (into found (:decls (handle-list loc file-ns-info file)))
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

(defn- else-nils
  ([lines] (else-nils ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :cond-else-nil (mapv :form))))

(deftest cond-closing-on-else-nil-is-flagged
  (is (= ["(cond (pos? n) :up :else nil)"
          "(cond (pos? n) :up true nil)"
          "(cond (pos? n) :up :default nil)"
          "(clojure.core/cond (pos? n) :up :else nil)"
          "(cond (pos? n) :up ...1 more... :else nil)"
          "#(cond (pos? %) :up :else nil)"
          "(cond (pos? n) :up :else nil)"
          "(cond (pos? n) :up :else nil)"]
         (else-nils ["(cond (pos? n) :up :else nil)"
                     "(cond (pos? n) :up true nil)"
                     "(cond (pos? n) :up :default nil)"
                     "(clojure.core/cond (pos? n) :up :else nil)"
                     "(cond (pos? n) :up\n      (neg? n) :down\n      :else nil)"
                     "#(cond (pos? %) :up :else nil)"
                     "(cond (pos? n) :up\n      ;; falls through\n      :else nil)"
                     "(cond (pos? n) :up #_(neg? n) :else nil)"]))))

(deftest else-nil-form-keeps-what-tells-two-apart
  (is (= ["(cond (= mode :search) ... ...2 more... :else nil)"
          "(cond (= mode :search) [smart-filters-toolbar] :else nil)"]
         (else-nils ["(cond (= mode :search) (let [props {:disabled true}]\n (toolbar props))\n a :a\n b :b\n :else nil)"
                     "(cond (= mode :search) [smart-filters-toolbar] :else nil)"]))
      "a value too long to show is left out, and the clauses between are counted"))

(deftest else-nil-form-survives-wrapping-first-value
  (is (= ["(cond (pos? n) (str \"up \" n) :else nil)"
          "(cond (pos? n) (str \"up \" n) :else nil)"]
         (else-nils ["(cond (pos? n) (str \"up \" n) :else nil)"
                     "(cond (pos? n) (str \"up \"\n                   n)\n      :else nil)"]))))

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
  (is (empty? (else-nils ["(lib/cond (pos? n) :up :else nil)"])))
  (is (empty? (else-nils (assoc ns-info :refers {"cond" "better-cond.core"})
                         ["(cond (pos? n) :up :else nil)"]))))

(deftest cond-through-an-alias-to-core-is-flagged
  (is (= ["(c/cond (pos? n) :up :else nil)"]
         (else-nils (assoc ns-info :aliases {"c" "cljs.core"})
                    ["(c/cond (pos? n) :up :else nil)"]))))

(deftest cond-the-ns-form-may-shadow-is-left-alone
  (is (empty? (else-nils (assoc ns-info :shadowed-core #{"cond"})
                         ["(cond (pos? n) :up :else nil)"])))
  (is (empty? (else-nils (assoc ns-info :shadowed-core :all)
                         ["(cond (pos? n) :up :else nil)"])))
  (is (= ["(clojure.core/cond (pos? n) :up :else nil)"]
         (else-nils (assoc ns-info :shadowed-core :all)
                    ["(clojure.core/cond (pos? n) :up :else nil)"]))
      "qualified cond is core's whatever the ns form shadows"))

(deftest locally-bound-cond-is-left-alone
  (is (empty? (else-nils ["(let [cond pick] (cond (pos? n) :up :else nil))"
                          "(defn f [{:keys [:cond]}] (cond (pos? n) :up :else nil))"
                          "(defn f [{:keys [opts/cond]}] (cond (pos? n) :up :else nil))"
                          "(fn cond [n] (cond (pos? n) :up :else nil))"
                          "(defrecord R [cond] Object (toString [_] (cond 1 :up :else nil)))"
                          "(reify P (pick [_ cond] (cond 1 :up :else nil)))"
                          "(reify P (pick ([_] 1) ([_ cond] (cond 1 :up :else nil))))"]))))

(deftest local-from-destructuring-is-left-alone
  (is (empty? (else-nils ["(defn f [{:keys [:cond]}] (cond (pos? n) :up :else nil))"
                          "(defn f [{:app/keys [cond]}] (cond (pos? n) :up :else nil))"
                          "(defn f [{:syms [app/cond]}] (cond (pos? n) :up :else nil))"
                          "(defn f [{cond :c}] (cond (pos? n) :up :else nil))"
                          "(defn f [#:app{:keys [cond]}] (cond (pos? n) :up :else nil))"
                          "(with-local-vars [cond 1] (cond (pos? n) :up :else nil))"]))))

(deftest name-outside-what-binds-binds-nothing
  (is (= ["(->> xs first vals (map inc) (filter even?))"
          "(->> xs first vals (map inc) (filter even?))"
          "(->> xs first vals (map inc) (filter even?))"]
         (rewrites ["(defn g [{f :filter}] (filter even? (map inc (vals (first xs)))))"
                    "(defn k [x :- [:map [:filter :int]]] (filter even? (map inc (vals (first xs)))))"
                    "(defn s [x :- schema/filter] (filter even? (map inc (vals (first xs)))))"]))))

(deftest qualified-cond-under-a-local-cond-is-flagged
  (is (= ["(clojure.core/cond (pos? n) :up :else nil)"]
         (else-nils ["(let [cond pick] (clojure.core/cond (pos? n) :up :else nil))"]))))

(deftest list-named-cond-that-is-no-call-is-left-alone
  (is (empty? (else-nils ["(reify ICond (cond [this] a :else nil))"
                          "(extend-protocol ICond String (cond [this] a :else nil))"
                          "(.. o (cond a b :else nil))"
                          "(. o (cond a b :else nil))"
                          "(case x (cond a b :else nil) :hit :miss)"
                          "(deftype T [] P (cond [_ a] a :else nil))"
                          "(defrecord R [] P (cond [_ a] a :else nil))"
                          "(extend-type T P (cond [_ a] a :else nil))"
                          "(proxy [Base] [] (cond [a] a :else nil))"
                          "(specify o P (cond [_ a] a :else nil))"
                          "(specify! o P (cond [_ a] a :else nil))"
                          "(definterface I (cond [a b] :else nil))"
                          "(defprotocol P (cond [a b] :else nil))"]))))

(deftest wrapped-list-named-cond-that-is-no-call-is-left-alone
  (is (empty? (else-nils ["(reify P ^:foo (cond [_ a] a :else nil))"
                          "(reify P #?(:cljs (cond [_ a] a :else nil)))"
                          "(.. o ^:m (cond a b :else nil))"
                          "(case x #?(:clj (cond a b :else nil)) :hit :miss)"]))))

(deftest cond-as-a-case-result-or-member-target-is-flagged
  (is (= ["(cond a b :else nil)"
          "(cond a b :else nil)"
          "(cond a b :else nil)"]
         (else-nils ["(case x 1 (cond a b :else nil))"
                     "(case x 1 :one (cond a b :else nil))"
                     "(.. (cond a b :else nil) toString)"]))))

(deftest cond-the-namespace-defines-is-left-alone
  (is (empty? (else-nils ["(defmacro cond [& clauses] clauses)"
                          "(cond (pos? n) :up :else nil)"]))))

(deftest unevaluated-cond-is-left-alone
  (is (empty? (else-nils ["'(cond (pos? n) :up :else nil)"
                          "(comment (cond (pos? n) :up :else nil))"
                          "#_(cond (pos? n) :up :else nil)"]))))

(defn- negated-tests
  ([lines] (negated-tests ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :negated-test (mapv :form))))

(deftest if-or-when-testing-not-is-flagged
  (is (= ["(if (not ready?) :wait :go)"
          "(if (not ready?) :wait)"
          "(when (not ready?) (log) (retry))"
          "(when (not ready?) (log) (retry) ...1 more...)"
          "(clojure.core/when (clojure.core/not ready?) :wait)"
          "#(when (not %) :wait)"
          "(if (not #_x (seq xs)) :empty :full)"]
         (negated-tests ["(if (not ready?) :wait :go)"
                         "(if (not ready?) :wait)"
                         "(when (not ready?)\n  (log)\n  (retry))"
                         "(when (not ready?) (log) (retry) (done))"
                         "(clojure.core/when (clojure.core/not ready?) :wait)"
                         "#(when (not %) :wait)"
                         "(if ;; why\n (not #_x (seq xs)) :empty :full)"])))
  (is (= ["(if (not x) :a :b)"]
         (negated-tests ["(if #_y (not x) :a :b)"]))
      "a discarded form before the test is no test"))

(deftest negated-test-finding-names-the-fold-and-where-it-is
  (let [[finding] (:negated-test (parse ["(defn f [x]" "  (when (not x) :wait))"]))]
    (is (= {:kw 'when
            :type :negated-test
            :file "views.cljs"
            :row 2
            :col 3
            :test "f"
            :hint "Write (when-not x ...)"}
           (dissoc finding :form)))))

(deftest negated-test-hint-leaves-out-a-long-test
  (is (= ["Write (when-not ... ...)"]
         (->> ["(when (not (contains? selected-document-ids document-id)) :a)"]
              parse
              :negated-test
              (mapv :hint)))))

(deftest negated-test-form-leaves-out-a-long-branch
  (is (= ["(if (not ready?) ... :go)"]
         (negated-tests ["(if (not ready?)\n  (let [attempts (count retries)]\n    (inc attempts))\n  :go)"]))))

(deftest negated-test-form-leaves-out-a-branch-one-line-would-misquote
  (is (= ["(if (not ready?) ... :go)"
          "(if (not ready?) ... :go)"]
         (negated-tests ["(if (not ready?)\n  (let [a 1] ; why\n    (inc a))\n  :go)"
                         "(if (not ready?)\n  (str \"line one\nline two\")\n  :go)"]))
      "a comment or a string spanning lines"))

(deftest negated-test-form-survives-wrapping-a-branch
  (is (= ["(if (not ready?) (let [a 1] (inc a)) :go)"
          "(if (not ready?) (let [a 1] (inc a)) :go)"]
         (negated-tests ["(if (not ready?) (let [a 1] (inc a)) :go)"
                         "(if (not ready?)\n  (let [a 1]\n    (inc a))\n  :go)"]))))

(deftest test-that-is-no-single-not-is-left-alone
  (is (empty? (negated-tests ["(if (not= a b) :x :y)"
                              "(if (not a b) :x :y)"
                              "(if (not) :x :y)"
                              "(if not :x :y)"
                              "(if ready? :x :y)"
                              "(when-not (not ready?) :x)"
                              "(if-not (not ready?) :x)"
                              "(when)"]))))

(deftest if-of-wrong-arity-is-left-alone
  (is (empty? (negated-tests ["(if (not ready?))"
                              "(if (not ready?) :a :b :c)"]))))

(deftest not-holding-a-reader-conditional-is-left-alone
  (is (empty? (negated-tests ["(when (not #?(:clj x)) :a)"
                              "(when (not #?@(:clj [x])) :a)"]))))

(deftest threaded-if-or-when-is-left-alone
  (is (empty? (negated-tests ["(-> v (when (not ready?)))"
                              "(cond-> m flag (if (not ready?) :a))"
                              "(doto o (when (not ready?) :a))"]))))

(deftest if-or-when-threaded-through-a-nested-or-wrapped-step-is-left-alone
  (is (empty? (negated-tests ["(-> m (cond-> flag (if (not x) :a)))"
                              "(-> v (doto (when (not x) :a)))"
                              "(-> v (some-> (when (not x))))"
                              "(-> v (-> (when (not x))))"
                              "(cond-> m true (-> (when (not x))))"
                              "(-> v ^:m (when (not ready?)))"
                              "(-> v #?(:cljs (when (not ready?))))"
                              "(-> x #?@(:clj [(when (not x))]))"]))))

(deftest if-or-when-that-a-thread-only-starts-from-is-flagged
  (is (= ["(when (not x) :a)"
          "(when (not x) :a)"
          "(when (not x) :a)"
          "(when (not x) :a)"]
         (negated-tests ["(->> m (-> (when (not x) :a)))"
                         "(-> (when (not x) :a) f)"
                         "(-> (-> (when (not x) :a)) f)"
                         "(f #?(:clj [(when (not x) :a)]))"]))))

(deftest chain-in-a-nested-or-wrapped-step-is-left-alone
  (is (empty? (rewrites ["(-> m (-> (vec (sort (distinct xs)))))"
                         "(-> m ^:m (vec (sort (distinct xs))))"
                         "(-> m #?(:cljs (vec (sort (distinct xs)))))"]))))

(deftest not-or-fold-that-is-not-core-is-left-alone
  (is (empty? (negated-tests ["(lib/when (not ready?) :a)"
                              "(when (lib/not ready?) :a)"
                              "(let [not some?] (when (not ready?) :a))"
                              "(let [when-not f] (when (not ready?) :a))"
                              "(let [when f] (when (not ready?) :a))"])))
  (is (empty? (negated-tests (assoc ns-info :shadowed-core #{"when-not"})
                             ["(when (not ready?) :a)"]))
      "a when-not the ns form may take from elsewhere is no fold to ask for")
  (is (empty? (negated-tests (assoc ns-info :shadowed-core #{"not"})
                             ["(when (not ready?) :a)"]))))

(deftest fold-or-not-the-namespace-defines-is-left-alone
  (is (empty? (negated-tests ["(defmacro when-not [& body] body)"
                              "(when (not ready?) :a)"])))
  (is (empty? (negated-tests ["(defn not [x] x)"
                              "(when (not ready?) :a)"])))
  (is (empty? (negated-tests ["(defmacro when [test & body] body)"
                              "(when (not ready?) :a)"]))))

(deftest unevaluated-or-no-call-is-left-alone
  (is (empty? (negated-tests ["'(when (not ready?) :a)"
                              "(comment (when (not ready?) :a))"
                              "(case k (when (not x) :a) :hit :miss)"
                              "(. o (when (not x) :a))"
                              "(.. o (when (not x) :a))"]))))

(deftest local-from-other-binding-forms-is-left-alone
  (is (empty? (negated-tests ["(defui panel [{:keys [when]}] (when (not ready?) :a))"
                              "(rum/defc panel [not] (when (not ready?) :a))"
                              "(areduce xs i not 0 (if (not x) 1 2))"
                              "(amap xs i not (if (not x) 1 2))"
                              "(let [not some? ;; why\n ] (when (not ready?) :a))"
                              "(let [#_x not some?] (when (not ready?) :a))"
                              "(let ;; why\n [not some?] (when (not ready?) :a))"
                              "(letfn [(step [not x] (when (not x) :a))] (step f 1))"
                              "(this-as not (when (not ready?) :a))"
                              "(are [not x] (when (not x) :a) f 1)"
                              "(-> x (as-> not (when (not ready?) :a)))"]))))

(deftest if-or-when-in-a-thread-written-as-a-fn-literal-is-left-alone
  (is (empty? (negated-tests ["(map #(-> % (when (not y) 1)) xs)"
                              "(map #(cond-> % t (if (not y) 1 2)) xs)"]))))

(defn- into-empties
  ([lines] (into-empties ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :into-empty (mapv :form))))

(deftest into-empty-set-from-a-fresh-collection-is-flagged
  (is (= ["(into #{} (map :id rows))"
          "(into #{} (keys m))"
          "(clojure.core/into #{} (concat a b))"
          "#(into #{} (map :id %))"
          "(into #{} [1 2])"
          "(into #{} #{1 2})"]
         (into-empties ["(into #{} (map :id rows))"
                        "(into #{} (keys m))"
                        "(clojure.core/into #{} (concat a b))"
                        "#(into #{} (map :id %))"
                        "(into #{} [1 2])"
                        "(into #{} #{1 2})"]))))

(deftest into-empty-set-from-what-may-be-a-set-or-reducible-is-left-alone
  (is (empty? (into-empties ["(into #{} ids)"
                             "(into #{} (sorted-set 3 1 2))"
                             "(into #{} (r/map inc xs))"
                             "(into #{} (set xs))"
                             "(into #{} (conj s 1))"
                             "(into #{} (merge s))"
                             "(into #{} (.getArray v))"
                             "(into #{} #?(:clj xs))"
                             "(into #{} #?@(:clj [(map inc) xs] :cljs [xs]))"]))))

(deftest into-empty-vector-from-what-is-no-array-is-flagged
  (is (= ["(into [] (map :id rows))"
          "(into [] (keys m))"
          "(into [] (clojure.core/filter odd? xs))"
          "(into [] [1 2])"
          "(into [] {:a 1})"
          "(into [] '(1 2))"
          "(into [] (concat a b))"
          "(into [] #{1 2})"]
         (into-empties ["(into [] (map :id rows))"
                        "(into [] (keys m))"
                        "(into [] (clojure.core/filter odd? xs))"
                        "(into [] [1 2])"
                        "(into [] {:a 1})"
                        "(into [] '(1 2))"
                        "(into [] (concat a b))"
                        "(into [] #{1 2})"]))))

(deftest into-empty-vector-from-what-may-be-an-array-is-left-alone
  (is (empty? (into-empties ["(into [] ids)"
                             "(into [] (.getArray v))"
                             "(into [] (js/Object.keys o))"
                             "(into [] (to-array xs))"
                             "(into [] (lib/map f xs))"
                             "(into [] (helper xs))"
                             "(into [] (vec arr))"
                             "(into [] (conj arr))"
                             "(into [] (merge arr))"
                             "(into [] (conj (subvec v 1) 4))"
                             "(into [] #(f %))"
                             "(into [] ())"]))))

(deftest into-empty-finding-names-the-builder-and-where-it-is
  (let [[finding] (:into-empty (parse ["(defn f [rows]" "  (into #{} (map :id rows)))"]))]
    (is (= {:kw 'into
            :type :into-empty
            :form "(into #{} (map :id rows))"
            :file "views.cljs"
            :row 2
            :col 3
            :test "f"
            :hint "Write (set (map :id rows))"}
           finding))))

(deftest into-empty-finding-shortens-a-long-source-in-the-hint-only
  (is (= [["(into #{} (map (comp keyword :value) available-permission-options))" "Write (set ...)"]]
         (->> ["(into #{} (map (comp keyword :value) available-permission-options))"]
              parse
              :into-empty
              (mapv (juxt :form :hint))))))

(deftest into-that-is-no-pour-into-an-empty-literal-is-left-alone
  (is (empty? (into-empties ["(into [] (map inc) xs)"
                             "(into #{} (map :id) rows)"
                             "(into [1] (map inc xs))"
                             "(into #{:a} (keys m))"
                             "(into {} (map f xs))"
                             "(into () (map inc xs))"
                             "(into ^:m [] (map inc xs))"
                             "(into v (map inc xs))"
                             "(into [])"]))))

(deftest into-empty-with-comments-inside-is-flagged
  (is (= ["(into [;; nothing yet] (map inc xs))"
          "(into #{} (keys m))"]
         (into-empties ["(into [ ;; nothing yet\n] (map inc xs))"
                        "(into #{} #_old (keys m))"]))))

(deftest threaded-into-is-left-alone
  (is (empty? (into-empties ["(->> ys (into [] (map inc xs)))"
                             "(->> ys (into #{} (keys m)))"]))))

(deftest into-or-builder-that-is-not-core-is-left-alone
  (is (empty? (into-empties ["(lib/into #{} (keys m))"
                             "(let [set sorted-set] (into #{} (keys m)))"
                             "(let [vec f] (into [] (map inc xs)))"
                             "(let [map f] (into [] (map inc xs)))"
                             "(let [into f] (into #{} (keys m)))"])))
  (is (empty? (into-empties (assoc ns-info :shadowed-core #{"set"})
                            ["(into #{} (keys m))"]))
      "a set the ns form may take from elsewhere is no builder to ask for")
  (is (empty? (into-empties ["(defn vec [xs] xs)"
                             "(into [] (map inc xs))"])))
  (is (empty? (into-empties ["(defn map [f xs] xs)"
                             "(into [] (map inc xs))"]))
      "a map the namespace defines may return an array")
  (is (empty? (into-empties ["(defmacro into [& xs] xs)"
                             "(into #{} (keys m))"]))))

(deftest unevaluated-into-is-left-alone
  (is (empty? (into-empties ["'(into #{} (keys m))"
                             "(comment (into #{} (keys m)))"
                             "#_(into #{} (keys m))"]))))

(defn- plus-minus-ones
  ([lines] (plus-minus-ones ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :plus-minus-one (mapv :form))))

(deftest plus-or-minus-trailing-one-is-flagged
  (is (= ["(+ page 1)"
          "(- (count items) 1)"
          "#(+ % 1)"
          "(cljs.core/+ n 1)"
          "(- ^long n 1)"]
         (plus-minus-ones ["(+ page 1)"
                           "(- (count items) 1)"
                           "#(+ % 1)"
                           "(cljs.core/+ n 1)"
                           "(- ^long n 1)"]))))

(deftest plus-minus-one-finding-names-the-step-and-where-it-is
  (let [[finding] (:plus-minus-one (parse ["(defn f [page]" "  (+ page 1))"]))]
    (is (= {:kw '+
            :type :plus-minus-one
            :form "(+ page 1)"
            :file "views.cljs"
            :row 2
            :col 3
            :test "f"
            :hint "Write (inc page)"}
           finding))))

(deftest plus-minus-one-hint-asks-for-a-call-in-a-clojure-fn-literal
  (is (= ["Write #(inc %)" "Write #(dec %1)"]
         (->> ["#(+ % 1)" "#(- %1 1)"]
              (parse idioms/group ns-info "core.clj")
              :plus-minus-one
              (mapv :hint)))
      "unchecked math wraps (+ % 1) and (inc %) alike, where inc as a value checks")
  (is (= ["Write #(inc %)"]
         (->> ["#(+ % 1)"]
              (parse idioms/group ns-info "core.cljc")
              :plus-minus-one
              (mapv :hint)))))

(deftest plus-minus-one-hint-asks-for-the-bare-function-in-a-fn-literal
  (is (= [['+ "#(+ % 1)" "Write inc"]
          ['- "#(- %1 1)" "Write dec"]
          ['+ "#(+ (:n %) 1)" "Write #(inc (:n %))"]
          ['- "#(- (count (filter :selected? (:permission-options %))) 1)" "Write #(dec ...)"]]
         (->> ["#(+ % 1)" "#(- %1 1)" "#(+ (:n %) 1)" "#(- (count (filter :selected? (:permission-options %))) 1)"]
              parse
              :plus-minus-one
              (mapv (juxt :kw :form :hint))))))

(deftest plus-minus-one-finding-shortens-a-long-operand-in-the-hint-only
  (is (= [["(- (count (filter :selected? available-permission-options)) 1)" "Write (dec ...)"]]
         (->> ["(- (count (filter :selected? available-permission-options)) 1)"]
              parse
              :plus-minus-one
              (mapv (juxt :form :hint))))))

(deftest plus-or-minus-what-is-no-trailing-one-is-left-alone
  (is (empty? (plus-minus-ones ["(+ 1 page)"
                                "(- 1 n)"
                                "(+ x 1.0)"
                                "(+ x 1N)"
                                "(+ x 1M)"
                                "(+ x 2)"
                                "(+ x +1)"
                                "(+ x y 1)"
                                "(+ x 1 1)"
                                "(+ 30 1)"
                                "(- 0.5 1)"
                                "(+ x)"
                                "(+ x one)"
                                "(* x 1)"
                                "(+ x #?(:cljs 1))"]))))

(deftest plus-minus-one-with-comments-inside-is-flagged
  (is (= ["(+ n 1)"
          "(- n 1)"]
         (plus-minus-ones ["(+ n ;; next\n 1)"
                           "(- n #_2 1)"]))))

(deftest plus-minus-one-of-a-reader-conditional-is-left-alone
  (is (empty? (plus-minus-ones ["(+ #?(:clj n) 1)"
                                "(+ #?@(:clj [a b]) 1)"]))))

(deftest threaded-plus-minus-one-is-left-alone
  (is (empty? (plus-minus-ones ["(-> n (+ m 1))"
                                "(->> n (- m 1))"
                                "(cond-> n flag (+ m 1))"
                                "(-> n (inc) #?(:cljs (+ m 1)))"
                                "(-> m (->> (+ n 1)))"
                                "(some-> n (+ m 1))"
                                "(some->> n (+ m 1))"
                                "(doto n (+ m 1))"
                                "(cond->> n flag (- m 1))"
                                "#(-> % (+ off 1))"
                                "#(->> % (- total 1))"
                                "#(cond-> % x (+ y 1))"
                                "#(doto % (- y 1))"
                                "#(some-> % (+ y 1))"])))
  (is (= ["(+ v 1)" "(+ n 1)"]
         (plus-minus-ones ["(as-> n v (+ v 1))"
                           "(as-> (+ n 1) v (str v))"]))
      "an as-> step writes its operand out, and its value binds nothing"))

(deftest plus-minus-one-or-step-that-is-not-core-is-left-alone
  (is (empty? (plus-minus-ones ["(lib/+ n 1)"
                                "(let [inc f] (+ n 1))"
                                "(let [dec f] (- n 1))"
                                "(let [+ f] (+ n 1))"
                                "(fn [- n] (- n 1))"
                                "(letfn [(step [inc x] (+ x 1))] (step f 1))"
                                "(letfn [(step ([x] x) ([inc x] (+ x 1)))] (step f 1))"
                                "(this-as inc (+ x 1))"
                                "(try (f) (catch :default inc (+ x 1)))"
                                "(are [inc x] (= 2 (+ x 1)) f 1)"
                                "(-> x (as-> inc (+ y 1)))"])))
  (is (empty? (plus-minus-ones (assoc ns-info :shadowed-core #{"inc"})
                               ["(+ n 1)"]))
      "an inc the ns form may take from elsewhere is no step to ask for")
  (is (empty? (plus-minus-ones ["(defn dec [n] n)"
                                "(- n 1)"])))
  (is (empty? (plus-minus-ones ["(defmacro + [& xs] xs)"
                                "(+ n 1)"]))))

(deftest unevaluated-plus-minus-one-is-left-alone
  (is (empty? (plus-minus-ones ["'(+ n 1)"
                                "(comment (+ n 1))"
                                "#_(+ n 1)"
                                "(case x (+ n 1) :a :b)"
                                "(. o (- n 1))"]))))

(defn- negated-equalities
  ([lines] (negated-equalities ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :negated-equality (mapv :form))))

(deftest not-of-an-equality-is-flagged
  (is (= ["(not (= before after))"
          "(not (= a b c))"
          "#(not (= % :done))"
          "(cljs.core/not (clojure.core/= a b))"
          "(not (= a #?(:clj b :cljs c)))"
          "(not (= ;; why a b))"]
         (negated-equalities ["(not (= before after))"
                              "(not (= a b c))"
                              "#(not (= % :done))"
                              "(cljs.core/not (clojure.core/= a b))"
                              "(not (= a #?(:clj b :cljs c)))"
                              "(not #_old (= ;; why\n a b))"]))))

(deftest negated-equality-finding-names-not-equal-and-where-it-is
  (let [[finding] (:negated-equality (parse ["(defn f [a b]" "  (not (= a b)))"]))]
    (is (= {:kw 'not
            :type :negated-equality
            :form "(not (= a b))"
            :file "views.cljs"
            :row 2
            :col 3
            :test "f"
            :hint "Write (not= a b)"}
           finding))))

(deftest negated-equality-hint-keeps-a-fn-literal-and-shortens-a-long-argument
  (is (= ["Write #(not= % :done)"
          "Write (not= ... :done)"]
         (->> ["#(not (= % :done))"
               "(not (= (get-in response [:body :results :status]) :done))"]
              parse
              :negated-equality
              (mapv :hint)))))

(deftest negated-equality-tested-by-if-or-when-is-not-negated-test
  (let [result (parse ["(if (not (= a b)) :x :y)"
                       "(when (not (clojure.core/= a b)) :x)"])]
    (is (= ["(not (= a b))" "(not (clojure.core/= a b))"]
           (mapv :form (:negated-equality result))))
    (is (empty? (:negated-test result))))
  (is (= ["(when (not (:= m)) :x)"]
         (negated-tests ["(when (not (:= m)) :x)"]))
      "a keyword named = is no equality"))

(deftest not-of-what-is-no-equality-is-left-alone
  (is (empty? (negated-equalities ["(not (== a b))"
                                   "(not (not= a b))"
                                   "(not (= a))"
                                   "(not (=))"
                                   "(not (= a b) c)"
                                   "(not ^:m (= a b))"
                                   "(not #?(:clj (= a b) :cljs (= a c)))"
                                   "(not eq)"
                                   "(not)"
                                   "(not [= a b])"]))))

(deftest threaded-negated-equality-is-left-alone
  (is (empty? (negated-equalities ["(-> ok? (not (= a b)))"
                                   "(->> ok? (not (= a b)))"
                                   "#(-> % (not (= a b)))"]))))

(deftest negated-equality-that-is-not-core-is-left-alone
  (is (empty? (negated-equalities ["(lib/not (= a b))"
                                   "(not (lib/= a b))"
                                   "(let [= f] (not (= a b)))"
                                   "(let [not f] (not (= a b)))"
                                   "(let [not= f] (not (= a b)))"])))
  (is (empty? (negated-equalities (assoc ns-info :shadowed-core #{"not="})
                                  ["(not (= a b))"]))
      "a not= the ns form may take from elsewhere is no fix to ask for")
  (is (empty? (negated-equalities ["(defn not= [a b] a)"
                                   "(not (= a b))"])))
  (is (empty? (negated-equalities ["(defn = [a b] a)"
                                   "(not (= a b))"])))
  (is (empty? (negated-equalities ["(defmacro not [x] x)"
                                   "(not (= a b))"]))))

(deftest unevaluated-negated-equality-is-left-alone
  (is (empty? (negated-equalities ["'(not (= a b))"
                                   "(comment (not (= a b)))"
                                   "#_(not (= a b))"
                                   "(case x (not (= a b)) :a :b)"]))))

(deftest call-inside-a-case-constant-is-left-alone
  (let [result (parse ["(case x [(not (= a b))] 1 2)"
                       "(case x ((not (= a b)) foo) 1 2)"
                       "(case x [(if (not (pos? a)) 1 2)] 1 2)"
                       "(case x [(+ n 1)] 1 2)"
                       "(case x [(into [] (map inc xs))] 1 2)"
                       "(case x [(cond a 1 :else nil)] 1 2)"])]
    (is (every? empty? (vals (dissoc result :nested-call-chain))))))

(deftest call-in-a-case-result-is-flagged
  (is (= ["(not (= a b))" "(not (= c d))"]
         (negated-equalities ["(case x :k (not (= a b)) (not (= c d)))"]))))

(deftest negated-test-reports-a-not-of-an-equality-negated-equality-cannot
  (is (= ["(if (not (lib/= a b)) 1 2)"
          "(when (not (= a)) 1)"
          "(when (not (= a b)) 1)"
          "(when (not (= a b)) 1)"]
         (negated-tests ["(if (not (lib/= a b)) 1 2)"
                         "(when (not (= a)) 1)"
                         "(fn [= a b] (when (not (= a b)) 1))"
                         "(fn [not= a b] (when (not (= a b)) 1))"])))
  (is (= ["(if (not (= a b)) 1 2)"]
         (negated-tests (assoc ns-info :shadowed-core #{"not="})
                        ["(if (not (= a b)) 1 2)"]))
      "a not= the ns form may take from elsewhere leaves the fold to negated-test")
  (is (= ["(if (not (= a b)) 1 2)"]
         (negated-tests ["(defn not= [a b] a)"
                         "(if (not (= a b)) 1 2)"]))
      "a not= the namespace defines leaves the fold to negated-test"))

(defn- true-clauses
  ([lines] (true-clauses ns-info lines))
  ([file-ns-info lines]
   (->> lines (parse idioms/group file-ns-info) :unconditional-cond-threading (mapv :form))))

(deftest conditional-thread-with-true-clause-is-flagged
  (is (= ["(cond-> db true (assoc :loading? true) ...2 more...)"
          "(cond->> xs true (map inc) ...1 more...)"
          "(cond-> db ...1 before... true (update :counter inc))"
          "(cond-> db true (assoc :a 1))"
          "#(cond-> % true inc)"
          "(clojure.core/cond-> db true (assoc :a 1))"
          "(cljs.core/cond->> xs true (map inc))"
          "(cond-> db true (assoc :a 1))"]
         (true-clauses ["(cond-> db\n  true (assoc :loading? true)\n  reset? (dissoc :cache)\n  true (update :counter inc))"
                        "(cond->> xs\n  true (map inc)\n  needs-filter? (filter pos?))"
                        "(cond-> db reset? (dissoc :cache) true (update :counter inc))"
                        "(cond-> db true (assoc :a 1))"
                        "#(cond-> % true inc)"
                        "(clojure.core/cond-> db true (assoc :a 1))"
                        "(cljs.core/cond->> xs true (map inc))"
                        "(cond-> db ;; always\n  #_flag true (assoc :a 1))"]))))

(deftest true-clause-finding-names-the-thread-and-where-it-is
  (let [[finding] (:unconditional-cond-threading (parse ["(defn load [db]" "  (cond->> db true (map inc)))"]))]
    (is (= {:kw 'cond->>
            :type :unconditional-cond-threading
            :form "(cond->> db true (map inc))"
            :file "views.cljs"
            :row 2
            :col 3
            :test "load"
            :hint "Thread (map inc) with ->>: its test is true, so it always runs"}
           finding))))

(deftest true-clause-hint-keeps-steps-in-order
  (is (= ["Thread the 2 steps tested true with ->: they always run"
          "Thread the step tested true with ->: its test is true, so it always runs"
          "Thread the cond-> into (assoc :cache {}) with ->: its test is true, so it always runs after the clauses above it"
          "Thread the cond->> into the 2 steps tested true with ->>, keeping their order: they always run"]
         (->> ["(cond-> db true (assoc :a 1) true (assoc :c 2) flag (dissoc :b))"
               "(cond-> db true (assoc-in db [:some :very :long :path :here] {:value 1}))"
               "(cond-> db reset? (dissoc :cache) true (assoc :cache {}))"
               "(cond->> xs true (map inc) flag (filter pos?) true (take 2))"]
              parse
              :unconditional-cond-threading
              (mapv :hint)))
      "a step after a conditional one stays after it"))

(deftest true-clause-form-leaves-out-a-long-expression-or-step
  (is (= ["(cond-> ... true ...)"]
         (true-clauses [(str "(cond-> (get-in db [:search-panel :results :items])"
                             " true (assoc-in [:search-panel :loading?] true))")]))))

(deftest true-clause-form-survives-wrapping-expression-or-step
  (is (= ["(cond-> (get db :x) true (assoc :a 1))"
          "(cond-> (get db :x) true (assoc :a 1))"]
         (true-clauses ["(cond-> (get db :x) true (assoc :a 1))"
                        "(cond-> (get db\n             :x)\n  true (assoc\n         :a 1))"]))))

(deftest test-that-is-no-literal-true-is-left-alone
  (is (empty? (true-clauses ["(cond-> db (always-true? ctx) (assoc :a 1) reset? (dissoc :cache))"
                             "(cond-> db false (assoc :a 1))"
                             "(cond-> db nil (assoc :a 1))"
                             "(cond-> db :always (assoc :a 1))"
                             "(cond-> db 'true (assoc :a 1))"
                             "(cond-> db ^:x true (assoc :a 1))"
                             "(cond-> db truthy (assoc :a 1))"
                             "(cond-> true flag not)"
                             "(cond-> db flag true)"]))))

(deftest plain-thread-is-left-alone
  (is (empty? (true-clauses ["(-> db (assoc :a 1) (update :counter inc))"
                             "(->> xs (map inc) (filter true?))"
                             "(some-> db true (assoc :a 1))"
                             "(cond db true (assoc :a 1))"]))))

(deftest conditional-thread-of-odd-shape-is-left-alone
  (is (empty? (true-clauses ["(cond-> db true)"
                             "(cond-> db true (assoc :a 1) flag)"
                             "(cond->)"]))
      "a clause without its step does not compile, so it is no thread to read"))

(deftest conditional-thread-holding-other-reader-macro-is-flagged
  (is (= ["(cond-> #js {} true (doto (aset \"a\" 1)))"
          "(cond-> #inst \"2020\" true str)"]
         (true-clauses ["(cond-> #js {} true (doto (aset \"a\" 1)))"
                        "(cond-> #inst \"2020\" true str)"]))))

(deftest conditional-thread-holding-reader-conditional-is-left-alone
  (is (empty? (true-clauses ["(cond-> db #?@(:cljs [flag (assoc :a 1)]) true (assoc :b 2))"
                             "(cond-> db #?(:cljs true :clj false) (assoc :a 1))"
                             "(cond-> #?(:cljs db :clj m) true (assoc :a 1))"]))))

(deftest threaded-conditional-thread-is-left-alone
  (is (empty? (true-clauses ["(-> db (cond-> true (assoc :a 1)))"
                             "(->> xs (cond->> true (map inc)))"
                             "(-> db (cond-> flag true))"
                             "(-> db (cond-> true (assoc :a 1) flag (dissoc :b)))"]))
      "a step takes its expression from the thread, so it writes whole pairs"))

(deftest conditional-thread-that-is-not-core-is-left-alone
  (is (empty? (true-clauses ["(lib/cond-> db true (assoc :a 1))"
                             "(fn [cond->] (cond-> db true (assoc :a 1)))"
                             "(let [-> identity] (cond-> db true (assoc :a 1)))"
                             "(defn cond->> [x] x)"
                             "(cond->> xs true (map inc))"])))
  (is (empty? (true-clauses (assoc ns-info :shadowed-core #{"cond->"})
                            ["(cond-> db true (assoc :a 1))"]))
      "a cond-> the ns form may take from elsewhere")
  (is (empty? (true-clauses (assoc ns-info :shadowed-core #{"->>"})
                            ["(cond->> xs true (map inc))"]))
      "a ->> the fix would ask for that may not be core's"))

(deftest unevaluated-conditional-thread-is-left-alone
  (is (empty? (true-clauses ["'(cond-> db true (assoc :a 1))"
                             "(comment (cond-> db true (assoc :a 1)))"
                             "#_(cond-> db true (assoc :a 1))"
                             "(case x (cond-> db true (assoc :a 1)) :a :b)"
                             "(. o (cond-> db true (assoc :a 1)))"]))))
