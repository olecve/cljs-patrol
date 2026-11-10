(ns cljs-patrol.heads-test
  (:require
   [cljs-patrol.heads :as heads]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.zip :as z]))

(def ^:private ns-info {:aliases {"r" "reagent.core"}
                        :refers {"defn" "schema.core"}
                        :shadowed-core #{"defn" "fn"}})

(defn- resolve-in
  "Resolve the head of the list `path` steps into `source` reaches."
  [file-ns-info source path]
  (heads/resolve-head (reduce #(%2 %1) (z/of-string source) path) file-ns-info))

(defn- resolve-top [source] (resolve-in ns-info source []))

(deftest heads-resolve-through-ns-form
  (is (= 'cljs.core/when (resolve-top "(when x y)"))
      "reads bare unshadowed name as core")
  (is (= 'cljs.core/when (resolve-top "(clojure.core/when x y)"))
      "reads clojure.core as cljs.core")
  (is (= 'cljs.core/when (resolve-top "(cljs.core/when x y)"))
      "keeps cljs.core")
  (is (= 'reagent.core/with-let (resolve-top "(r/with-let [a 1] a)"))
      "resolves alias")
  (is (= 'schema.core/defn (resolve-top "(defn f :- [s/Str] [x] x)"))
      "resolves refer over core")
  (is (= 'fn (resolve-top "(fn :- [s/Int] [x] x)"))
      "keeps name ns form excludes as written")
  (is (= 'x/when (resolve-top "(x/when a b)"))
      "keeps unknown alias as written")
  (is (nil? (resolve-top "(:k m)"))
      "gives nil for keyword head")
  (is (nil? (resolve-top "()"))
      "gives nil for empty list"))

(deftest special-forms-resolve-to-core-whatever-shadows-them
  (is (= 'cljs.core/do (resolve-in {:shadowed-core :all} "(do x y)" []))
      "reads special form when ns form may shadow any name")
  (is (= 'when (resolve-in {:shadowed-core :all} "(when x y)" []))
      "keeps other name when ns form may shadow any")
  (is (= 'x/do (resolve-in {} "(x/do a b)" []))
      "keeps special form name under alias as written")
  (is (= 'cljs.core/if (resolve-in {} "(let [if f] (if x y))" [z/down z/right z/right]))
      "reads special form under local of its name"))

(deftest locals-shadow-core-heads
  (is (= 'when (resolve-in {} "(let [when f] (when x y))" [z/down z/right z/right]))
      "keeps head a let binds as written")
  (is (= 'when (resolve-in {} "(fn [when] (when x y))" [z/down z/right z/right]))
      "keeps head a parameter binds as written")
  (is (= 'cljs.core/when (resolve-in {} "(let [other f] (when x y))" [z/down z/right z/right]))
      "reads head no local binds as core"))
