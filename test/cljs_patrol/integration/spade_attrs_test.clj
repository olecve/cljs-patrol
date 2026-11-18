(ns cljs-patrol.integration.spade-attrs-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.reagent :as reagent]
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

    (testing "flags a call read as data past a `#_` form"
      (is (every? flagged? ["discard-before-attrs" "discard-step-attrs"])
          "a `#_` form is no argument and no thread step"))

    (testing "flags a lookup in the test of a `cond->`"
      (is (every? flagged? ["cond-test-keyword-attrs" "cond-test-get-attrs"])
          "a test is no step, so nothing is threaded into it"))

    (testing "flags a defattrs called through a refer"
      (is (flagged? "referred-merge-attrs")))

    (testing "flags a defattrs that is also passed whole"
      (is (flagged? "icon-attrs")))

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

    (testing "does not flag a local that shadows the style name"
      (is (not-any? flagged? ["shadow-param-attrs" "shadow-let-attrs" "shadow-letfn-attrs"])
          "a parameter, a let binding, a letfn binding"))

    (testing "does not flag a call in a step spliced into a thread-first form"
      (is (not-any? flagged? ["spliced-step-attrs" "spliced-cond-step-attrs" "nested-spliced-step-attrs"
                              "spliced-cond-test-attrs"])
          "a `#?@` splice hides which argument the step takes, and whether a form is a test or a step"))

    (testing "does not flag a call spliced into an argument of a function that reads one position"
      (is (not (flagged? "spliced-key-attrs"))
          "a `#?@` splice hides the argument position")
      (is (not (flagged? "conditional-element-attrs"))
          "a `#?` keeps its vector, so the call is an element of it"))

    (testing "flags a merge however a `#?@` splices it"
      (is (every? flagged? ["spliced-merge-step-attrs" "spliced-merge-argument-attrs"])
          "a merge reads every argument, so its position does not matter"))

    (testing "does not flag a call inside a `#_` form"
      (is (not (flagged? "discarded-attrs"))))

    (testing "does not flag a defclass"
      (is (not (flagged? "merged-class"))))

    (testing "flags nothing else"
      (is (= 29 (count flagged))))))

(deftest defattrs-as-data-fix-test
  (let [[spade-result reagent-result] (:group-results (core/run fixture-dir [spade/group reagent/group]))]
    (is (some #(= "icon-attrs" (name (:kw %))) (:defattrs-as-data spade-result))
        "defattrs used whole and with other props")
    (is (not-any? #(= "icon-style" (name (:kw %))) (:defclass-as-sole-attr reagent-result))
        "defclass fix used whole and with other props")
    (is (not-any? #(contains? #{"merged-fix-class" "assoc-fix-class"} (name (:kw %)))
                  (:defclass-as-sole-attr reagent-result))
        "defclass fix in a class map that is merged or changed")
    (is (not-any? #(= "discarded-class" (name (:kw %))) (:defclass-as-sole-attr reagent-result))
        "defclass used only inside a discarded form")
    (is (not-any? #(= "merge-props-class" (name (:kw %))) (:defclass-as-sole-attr reagent-result))
        "defclass fix in a class map that merge-props merges")
    (is (not-any? #(= "js-object-class" (name (:kw %))) (:defclass-as-sole-attr reagent-result))
        "defclass in a #js object, which needs the class string")
    (is (some #(= "reader-conditional-class" (name (:kw %))) (:defclass-as-sole-attr reagent-result))
        "defclass used whole inside a reader conditional")
    (is (every? (->> reagent-result :defclass-as-sole-attr (map (comp name :kw)) set)
                ["live-and-discarded-class" "discarded-other-use-class"])
        "defclass used whole, also inside a discarded form")))

(deftest defattrs-as-data-prefers-file-declaration-test
  (let [flagged (->> (core/run fixture-dir [spade/group]) :group-results first :defattrs-as-data (map :kw) set)]
    (is (contains? flagged :webapp.redeclared-names/redeclared-attrs)
        "defattrs declared in the file")
    (is (not (contains? flagged :webapp.attrs-styles/redeclared-attrs))
        "referred defattrs of the same name")
    (is (contains? flagged :webapp.conditional-names/conditional-attrs)
        "defattrs declared in a reader conditional")
    (is (not (contains? flagged :webapp.attrs-styles/conditional-attrs))
        "referred defattrs of the same name as one in a reader conditional")))
