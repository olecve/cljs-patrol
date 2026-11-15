(ns cljs-patrol.integration.spade-attrs-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.spade :as spade]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test-projects/spade-attrs-app/src/webapp")

(defn- flagged-names []
  (->> (core/run fixture-dir [spade/group]) :group-results first :defattrs-as-data (map (comp name :kw))))

(deftest defattrs-as-data-test
  (let [flagged (flagged-names)
        flagged? (set flagged)]

    (testing "flags a defattrs whose call a core function changes or merges"
      (is (every? flagged? ["merge-attrs" "assoc-attrs" "assoc-in-attrs" "update-attrs" "dissoc-attrs" "conj-attrs"
                            "into-attrs"]))
      (is (flagged? "class-text-attrs")
          "a lookup that is an argument of another call is no thread step"))

    (testing "flags a defattrs whose call is the target of a key lookup"
      (is (every? flagged? ["keyword-attrs" "get-attrs" "get-in-attrs"])))

    (testing "flags a defattrs whose call a `->` hands to a lookup or a change"
      (is (every? flagged? ["thread-lookup-attrs" "thread-assoc-attrs" "thread-step-attrs"])
          "the call may be the value threaded or a step whose result the next step reads"))

    (testing "flags a lookup that opens a thread-first form"
      (is (flagged? "thread-head-attrs")
          "the first argument of a `->` is no step, so nothing is threaded into the lookup"))

    (testing "flags a merge however its arguments arrive"
      (is (every? flagged? ["threaded-merge-attrs" "fn-literal-attrs" "reader-conditional-attrs"])
          "a merge reads every argument, threaded or not, in a fn literal or a reader conditional"))

    (testing "reports a defattrs once for several calls read as data"
      (is (= 1 (count (filter #{"repeated-attrs"} flagged)))))

    (testing "does not flag a call passed on whole"
      (is (not-any? flagged? ["whole-props-attrs" "component-props-attrs" "passed-attrs" "variant-attrs" "prop-attrs"
                              "value-attrs" "fallback-attrs" "reader-conditional-props-attrs"])
          "the props slot of a tag or a component, another function, a prop, a stored value, a lookup default"))

    (testing "does not flag a call a thread-first step takes after its threaded value"
      (is (not-any? flagged? ["cache-key-attrs" "some-cache-key-attrs" "cond-cache-key-attrs"
                              "reader-conditional-step-key-attrs"])))

    (testing "does not flag a `->` with no step reading a map"
      (is (not-any? flagged? ["map-step-key-attrs" "lone-thread-attrs"])
          "a map as a step looks the value up as a key"))

    (testing "does not flag a function named like a core one"
      (is (not-any? flagged? ["local-merge-attrs" "local-assoc-attrs" "util-merge-attrs" "referred-assoc-attrs"])
          "a local, an alias of another namespace, a refer from another namespace"))

    (testing "does not flag a defclass"
      (is (not (flagged? "merged-class"))))

    (testing "flags nothing else"
      (is (= 19 (count flagged))))))
