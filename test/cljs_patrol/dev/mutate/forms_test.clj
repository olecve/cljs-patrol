(ns cljs-patrol.dev.mutate.forms-test
  (:require
   [cljs-patrol.dev.mutate.forms :as forms]
   [clojure.test :refer [deftest is]]))

(def ^:private source
  (str "(ns app.core)\n"
       "(defn ^:private visible? [x] (when (seq x) true))\n"
       "(defn count-up [n] (comment (when n 1)) (inc n))\n"
       "(comment (when x y))\n"
       "#_(defn gone [] (when x y))\n"
       "(def quoted '(when x y))\n"
       "(def plain {:a 1})\n"
       "(def ^{:private true :added 2} hidden 1)\n"
       "(defonce cache (when x y))\n"
       "(extend-type Foo P (m [x] (when x 1)))\n"
       "(defn hinted [xs] ^String (first xs))\n"
       "(defn attrs {:private true} [] (if x 1 2))\n"
       "(defn platform [] #?(:clj (when x 1) :cljs (when y 2)))\n"
       "(defn body-map [] {:a (if x 1 2)})\n"
       "(defn two-lines [x]\n"
       "  (when x 1))\n"))

(defn- without-bodies [mutants] (remove #(= :body (:swap %)) mutants))

(defn- swaps-of [mutants] (mapv (juxt :def :row :from #(pr-str (:to %))) (without-bodies mutants)))

(deftest mutants-swap-each-operator-of-each-definition
  (is (= [["visible?" 2 "when" "when-not"]
          ["visible?" 2 "seq" "empty?"]
          ["visible?" 2 "true" "false"]
          ["count-up" 3 "inc" "dec"]
          ["hinted" 11 "first" "last"]
          ["attrs" 12 "if" "if-not"]
          ["attrs" 12 "1" "0"]
          ["attrs" 12 "2" "3"]
          ["platform" 13 "when" "when-not"]
          ["platform" 13 "1" "0"]
          ["body-map" 14 "if" "if-not"]
          ["body-map" 14 "1" "0"]
          ["body-map" 14 "2" "3"]
          ["two-lines" 16 "when" "when-not"]
          ["two-lines" 16 "1" "0"]]
         (swaps-of (forms/mutants (forms/top-level-forms source))))
      (str "ns form, comment, discarded, quoted, metadata, attr map and cljs-branch code skipped; a hinted form"
           " mutated; forms loaded once and other forms left out; numbers in def values left out")))

(deftest mutants-change-plain-integer-literals
  (let [source (str "(ns app.n)\n"
                    "(defn numbers [] [0 1 7 -1 -5 9223372036854775807"
                    " 0x10 +1 007 -0 2r101 1N 1M 1.5 1/2 \\1 \"1\" :1])\n")]
    (is (= [["0" "1"] ["1" "0"] ["7" "8"] ["-1" "0"] ["-5" "-4"] ["9223372036854775807" "9223372036854775808N"]]
           (map (juxt :from #(pr-str (:to %))) (without-bodies (forms/mutants (forms/top-level-forms source)))))
        "0 and 1 swap, other integers go up by one; other notations, numbers that are no integer and characters kept")))

(deftest scope-skips-number-mutant-named-in-marker
  (let [{:keys [selected ignored unused-markers]}
        (forms/scope "(ns app.x)\n(defn f [x] (max x 0 -1)) ; mutate: skip 0: zero is the floor\n" {})]
    (is (= [["0" "zero is the floor"]] (map (juxt :from :skip-reason) ignored)))
    (is (= ["-1"] (map :from (without-bodies selected))))
    (is (= [] unused-markers))))

(deftest scope-names-what-is-not-mutated-and-unknown-definitions
  (let [{:keys [loaded-once other-forms]} (forms/scope source {})]
    (is (= ["cache (defonce)"] loaded-once))
    (is (= ["extend-type@10"] other-forms)))
  (is (= ["count-up"] (:defs-in-scope (forms/scope source {:lines #{3}})))
      "changed lines keep only definitions they touch")
  (let [{:keys [defs-in-scope selected]} (forms/scope source {:lines #{15}})]
    (is (= ["two-lines"] defs-in-scope)
        "one changed line of several brings definition in")
    (is (empty? selected)
        "mutant on unchanged line of changed definition is left out"))
  (is (= [[16 "body"] [16 "when"] [16 "1"]] (map (juxt :row :from) (:selected (forms/scope source {:lines #{16}}))))
      "mutants on changed line are kept")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No top-level definition named nope"
                        (forms/scope source {:defs ["nope"]}))))

(def ^:private wrapped-source
  (str "(ns app.wrapped)\n"
       "^:foo (defn tagged [] (when q 1))\n"
       "#?(:clj (defn on-clj [] (when r 1)))\n"
       "#?(:cljs (defn on-cljs [] (when r 1)))\n"
       "(defn spliced [] #?@(:cljs [(when y 1)] :clj [(if z 1 2)]))\n"
       "(defn defaulted [] #?(:clj (when a 1) :default (when b 2)))\n"
       "(def table {:a (when x 1)})\n"
       "#?@(:cljs [(defn cljs-only [] (when x 1))] :clj [(defn spliced-a [] (when x 1)) (defn spliced-b [] (if x 1 2))])\n"))

(deftest mutants-read-forms-as-clojure-loads-them
  (is (= [["tagged" 2 "when" "when-not"]
          ["tagged" 2 "1" "0"]
          ["on-clj" 3 "when" "when-not"]
          ["on-clj" 3 "1" "0"]
          ["spliced" 5 "if" "if-not"]
          ["spliced" 5 "1" "0"]
          ["spliced" 5 "2" "3"]
          ["defaulted" 6 "when" "when-not"]
          ["defaulted" 6 "1" "0"]
          ["table" 7 "when" "when-not"]
          ["spliced-a" 8 "when" "when-not"]
          ["spliced-a" 8 "1" "0"]
          ["spliced-b" 8 "if" "if-not"]
          ["spliced-b" 8 "1" "0"]
          ["spliced-b" 8 "2" "3"]]
         (swaps-of (forms/mutants (forms/top-level-forms wrapped-source))))
      (str "definition under metadata or in a clj branch kept; only branch Clojure reads mutated; a def's value"
           " map is no attribute map")))

(def ^:private marked-source
  (str "(ns app.marked)\n"
       "(defn pick [xs] (first xs)) ; mutate: skip first: xs holds one item\n"
       "(defn both [x]\n"
       "  ; mutate: skip when: x is never nil here\n"
       "  ; mutate: skip =: two checks for one case\n"
       "\n"
       "  (when (= x :a) :one))\n"
       "(defn ^{:mutate/skip \"wraps a JDK call\"} wrapped [x] (if x :a :b))\n"
       "(defn stale [x] (if x :a :b)) ; mutate: skip first: no first here any more\n"
       "(defn told [x]\n"
       "  ; mutate: skip when: r1\n"
       "  (when (str \"; mutate: skip only words\") :a) ; mutate: skip str: r2\n"
       "  (when x :b))\n"
       "; mutate: skip if: nothing follows\n"))

(deftest skip-markers-read-trailing-and-own-line-comments
  (is (= [[2 2 "first" "xs holds one item"]
          [4 7 "when" "x is never nil here"]
          [5 7 "=" "two checks for one case"]
          [9 9 "first" "no first here any more"]
          [11 12 "when" "r1"]
          [12 12 "str" "r2"]
          [14 nil "if" "nothing follows"]]
         (map (juxt :line :row :operator :reason) (forms/skip-markers marked-source)))
      "own line skips next code past blank lines and comments, also next to trailing marker; string is no marker")
  (doseq [text ["(inc 1) ; mutate: skip because\n" "(inc 1) ; mutate:skip inc: r\n" "(inc 1) ; Mutate: skip inc: r\n"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"line 1 reads like a skip marker"
                          (forms/skip-markers text))
        text)))

(deftest scope-leaves-skipped-mutants-out-with-reason
  (let [{:keys [selected ignored unused-markers]} (forms/scope marked-source {})]
    (is (= [["pick" "first" "xs holds one item"]
            ["both" "when" "x is never nil here"]
            ["both" "=" "two checks for one case"]
            ["wrapped" "body" "wraps a JDK call"]
            ["wrapped" "if" "wraps a JDK call"]
            ["told" "when" "r1"]]
           (map (juxt :def :from :skip-reason) ignored)))
    (is (= [["stale" "if"]
            ["told" "when"]]
           (map (juxt :def :from) (without-bodies selected)))
        "only unmarked mutants run")
    (is (= ["line 9 (first)" "line 12 (str)" "line 14 (if)"] unused-markers)
        "marker naming no mutant on its line, or with no code after it, is reported"))
  (is (= [] (:unused-markers (forms/scope marked-source {:defs ["pick"]})))
      "markers outside scope are not checked")
  (is (= [7 7] (map :row (:ignored (forms/scope marked-source {:lines #{7}}))))
      "changed lines limit ignored mutants too")
  (doseq [metadata ["^:mutate/skip" "^{:mutate/skip false}" "^{:mutate/skip \"\"}"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on f needs a reason"
                          (forms/scope (str "(ns app.x)\n(defn " metadata " f [x] (when x 1))\n") {}))
        metadata))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on first needs a reason"
                        (forms/scope (str "(ns app.x)\n(defn ^:mutate/skip first [x] (when x 1))\n"
                                          "(defn ^:mutate/skip second [x] (when x 1))\n")
                                     {}))
      "first bad definition named"))

(deftest skip-markers-ignore-prose-that-mentions-them
  (is (= []
         (forms/skip-markers (str "(ns app.x)\n"
                                  ";; see ^{:mutate/skip ...} on wrapped\n"
                                  "; never mutate; skip the copy\n"
                                  "(defn f [x] (when x 1))\n"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"line 1 reads like a skip marker"
                        (forms/skip-markers "; mutate:skip first: x\n(ns app.x)\n"))
      "comment before first form is read"))

(deftest skip-markers-stay-inside-their-form
  (is (= [[3 nil] [6 8]]
         (map (juxt :line :row)
              (forms/skip-markers (str "(defn f [x]\n"
                                       "  (g x)\n"
                                       "  ; mutate: skip when: last in its form\n"
                                       "  )\n"
                                       "(defn h [x]\n"
                                       "  ; mutate: skip when: past a #_ form\n"
                                       "  #_(old-call x)\n"
                                       "  (when x 1))\n"))))
      "no code after marker in its form gives no row; #_ form is no code"))

(deftest scope-reads-skip-reason-from-attribute-map
  (is (= [["f" "body" "wraps a JDK call"] ["f" "if" "wraps a JDK call"]]
         (map (juxt :def :from :skip-reason)
              (:ignored (forms/scope "(ns app.x)\n(defn f \"Doc.\" {:mutate/skip \"wraps a JDK call\"} [x] (if x :a :b))\n"
                                     {})))))
  (is (= ["if"]
         (map :from (:selected (forms/scope "(ns app.x)\n(def m {:mutate/skip \"value\" :a (if x :b :c)})\n" {}))))
      "map value of def is no attribute map"))

(deftest scope-checks-skip-reason-of-every-definition
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on helper needs a reason"
                        (forms/scope "(ns app.x)\n(defn ^:mutate/skip helper [x] (str x))\n" {}))
      "definition with no mutants")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on helper needs a reason"
                        (forms/scope "(ns app.x)\n(defn ^:mutate/skip helper [x] (when x 1))\n(defn g [x] (if x 1 2))\n"
                                     {:defs ["g"]}))
      "definition outside -d")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata on f needs a reason as a string"
                        (forms/scope (str "(ns app.x)\n"
                                          "(defn ^{:mutate/skip #?(:clj \"jdk\" :cljs \"js\")} f [x] (when x 1))\n")
                                     {}))
      "reader conditional is no reason string"))

(deftest scope-reports-markers-in-forms-that-are-not-mutated
  (is (= ["line 2 (when)" "line 4 (if)"]
         (:unused-markers (forms/scope (str "(ns app.x)\n"
                                            "(:require [a]) ; mutate: skip when: stale\n"
                                            "(comment\n"
                                            "  (if x 1 2)) ; mutate: skip if: stale\n"
                                            "(defn f [x] (when x 1))\n")
                                       {})))))

(deftest skip-markers-leave-other-mutate-comments-alone
  (is (= []
         (forms/skip-markers "(ns app.x)\n;; Mutate the atom in place for speed\n; mutate! is only for tests\n")))
  (is (= [] (forms/skip-markers ""))
      "empty source"))

(deftest scope-reads-attribute-map-only-before-parameters-or-after-arities
  (is (= ["body" "if"]
         (map :from (:selected (forms/scope "(ns app.x)\n(defn f ^String [x] {:mutate/skip (if x \"a\" \"b\")})\n" {}))))
      "map after hinted parameter vector is body")
  (is (= [["f" "body" "wraps a JDK call"]
          ["f" "body" "wraps a JDK call"]
          ["f" "when" "wraps a JDK call"]
          ["f" "if" "wraps a JDK call"]]
         (map (juxt :def :from :skip-reason)
              (:ignored (forms/scope (str "(ns app.x)\n"
                                          "(defn f ([x] (when x :a)) ([x y] (if x y :b))"
                                          " {:mutate/skip \"wraps a JDK call\"})\n")
                                     {}))))
      "trailing attribute map after arities")
  (is (= ["r" "r"]
         (map :skip-reason
              (:ignored (forms/scope "(ns app.x)\n(defn ^:private f {:mutate/skip \"r\"} [x] (if x :a :b))\n" {}))))
      "name under metadata is no parameter vector"))

(deftest scope-rejects-skip-metadata-that-does-nothing-or-cannot-be-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"m is a defmacro, which the runner does not mutate"
                        (forms/scope "(ns app.x)\n(defmacro ^{:mutate/skip \"r\"} m [x] x)\n" {})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metadata of f cannot be read: No value supplied"
                        (forms/scope "(ns app.x)\n(defn ^{:mutate/skip} f [x] (when x 1))\n" {}))))

(deftest mutants-change-numbers-only-in-function-code
  (let [mutants (forms/mutants (forms/top-level-forms (str "(ns app.x)\n"
                                                           "(defmethod area 1 [_] (if true 2 3))\n"
                                                           "(defmethod area [0 true] [_] 4)\n"
                                                           "(def limit 129)\n"
                                                           "(def ^:const width 80)\n"
                                                           "(defn f [x] (+ x 1))\n"
                                                           "(def g (fn [x] (+ x 5)))\n"
                                                           "(def h #(* % 6))\n"
                                                           "(def table {:fn (fn [] 7) :size 8})\n"
                                                           "(def built (let [f (fn [] 9)] (f)))\n")))]
    (is (= [["area" 2 "body"] ["area" 2 "if"] ["area" 2 "true"] ["area" 2 "2"] ["area" 2 "3"] ["area" 3 "body"]
            ["area" 3 "4"] ["f" 6 "body"] ["f" 6 "1"] ["g" 7 "body"] ["g" 7 "5"] ["h" 8 "6"]]
           (map (juxt :def :row :from) mutants))
        (str "dispatch value, def data, fn inside def data and const def left out; def whose value is fn or #() kept;"
             " #() gets no body mutant"))
    (is (= [:body :operator :operator :number :number :body :number :body :number :body :number :number]
           (map :swap mutants))
        "body replaced by nil marked body, integer literals number, other swaps operator")))

(deftest mutants-leave-definition-names-alone
  (is (= [["last" "first"]]
         (map (juxt :from #(pr-str (:to %)))
              (without-bodies (forms/mutants (forms/top-level-forms (str "(ns app.x (:refer-clojure :exclude [first]))\n"
                                                                         "(defn first [xs] (last xs))\n"
                                                                         "(defmethod first :a [xs] xs)\n"
                                                                         "(defn ^:private last [xs] xs)\n"))))))
      "only body swap; names of defn, defmethod and metadata-wrapped name kept"))

(deftest scope-lists-const-defs-apart
  (is (= ["width"] (:const-defs (forms/scope "(ns app.x)\n(def ^:const width 80)\n(def ^:private height 9)\n" {})))
      "other metadata makes no const def")
  (is (= [] (:loaded-once (forms/scope "(ns app.x)\n(def ^:const width 80)\n" {}))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"width is a \^:const def, whose value callers copy, so the runner does not mutate it, and its"
                        (forms/scope "(ns app.x)\n(def ^{:const true :mutate/skip \"fixed\"} width 80)\n" {}))))

(def ^:private bodies-source
  (str "(ns app.b)\n"
       "(defn long-body \"Doc.\" {:added 1} [x]\n"
       "  ; first\n"
       "  (prn x) #_(gone)\n"
       "  (inc x) #_(after))\n"
       "(defn arities \"Doc.\" ([x] (arities x 1)) ([x y] (+ x y)) {:added 1})\n"
       "(defmethod area [:a :b] [x] (str x))\n"
       "(def named (fn named [x] {:pre [(pos? x)]} (* 2 x)))\n"
       "(defn ^String hinted ^String [x] x)\n"
       "(defn already-nil [x] nil)\n"
       "(defn empty-body [x])\n"
       "(defn nil-first [x] nil (prn x))\n"
       "(def short-fn #(prn %))\n"
       "(defn only-map [x] {:a x})\n"
       "(defn checked-nil [x] {:pre [x]} nil)\n"))

(deftest mutants-replace-each-arity-body-with-nil
  (let [bodies (filter #(= :body (:swap %)) (forms/mutants (forms/top-level-forms bodies-source)))]
    (is (= [["long-body" 4 3 5 10 "(prn x) #_(gone)\n  (inc x)"]
            ["arities" 6 27 6 40 "(arities x 1)"]
            ["arities" 6 49 6 56 "(+ x y)"]
            ["area" 7 29 7 36 "(str x)"]
            ["named" 8 44 8 51 "(* 2 x)"]
            ["hinted" 9 34 9 35 "x"]
            ["nil-first" 12 21 12 32 "nil (prn x)"]
            ["only-map" 14 20 14 26 "{:a x}"]]
           (map (juxt :def :row :col :end-row :end-col forms/replaced-text) bodies))
        (str "docstring, attribute maps, name, dispatch value, hint and condition map left out; one per arity;"
             " lone nil, also after condition map, empty body and #() give none; lone map is body"))
    (is (= [["odd-attrs" "(inc x)"]]
           (->> (forms/top-level-forms "(ns app.b)\n(defn odd-attrs ([x] (inc x)) {[:key] :value})\n")
                forms/mutants
                (filter #(= :body (:swap %)))
                (map (juxt :def forms/replaced-text))))
        "attribute map after arities is no arity, even with vector key")
    (is (= [["body" nil]] (distinct (map (juxt :from :to) bodies)))
        "body mutant reads as body to nil")
    (is (= ["(defn long-body \"Doc.\" {:added 1} [x]\n  ; first\n  nil #_(after))"
            "(defn arities \"Doc.\" ([x] nil) ([x y] (+ x y)) {:added 1})"
            "(defn arities \"Doc.\" ([x] (arities x 1)) ([x y] nil) {:added 1})"
            "(defmethod area [:a :b] [x] nil)"
            "(def named (fn named [x] {:pre [(pos? x)]} nil))"
            "(defn ^String hinted ^String [x] nil)"
            "(defn nil-first [x] nil)"
            "(defn only-map [x] nil)"]
           (map forms/mutated-text bodies))
        "every form of body gives way to one nil, up to its last form")))

(deftest scope-skips-body-mutant-named-in-marker
  (let [{:keys [selected ignored unused-markers]}
        (forms/scope (str "(ns app.x)\n"
                          "(defn log! [x]\n"
                          "  ; mutate: skip body: only prints\n"
                          "  (prn x))\n"
                          "(defn g [x] (inc x)) ; mutate: skip body: stale\n"
                          "(defn h [x]\n"
                          "  (prn x)\n"
                          "  (inc x)) ; mutate: skip body: past first line\n")
                     {})]
    (is (= [["log!" 4 "only prints"] ["g" 5 "stale"]] (map (juxt :def :row :skip-reason) ignored)))
    (is (= [["g" "inc"] ["h" "body"] ["h" "inc"]] (map (juxt :def :from) selected)))
    (is (= ["line 8 (body)"] unused-markers)
        "marker names body only on line where body starts")))

(deftest scope-selects-body-mutant-when-any-of-its-lines-changed
  (let [source (str "(ns app.x)\n"
                    "(defn log! [x]\n"
                    "  ; mutate: skip body: only prints\n"
                    "  (prn x)\n"
                    "  (prn x))\n"
                    "(defn h [x]\n"
                    "  (prn x)\n"
                    "  (inc x))\n")
        {:keys [selected ignored]} (forms/scope source {:lines #{5 8}})]
    (is (= [["h" 7 "body"] ["h" 8 "inc"]] (map (juxt :def :row :from) selected))
        "body selected by change on its last line")
    (is (= [["log!" 4 "body"]] (map (juxt :def :row :from) ignored))
        "marked body ignored by change on its last line")))
