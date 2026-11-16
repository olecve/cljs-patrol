(ns cljs-patrol.integration.data-def-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.forbid-props :as forbid-props]
   [cljs-patrol.groups.layout :as layout]
   [cljs-patrol.groups.react-testing-library-cljs :as react-testing-library-cljs]
   [cljs-patrol.groups.reagent :as reagent]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private fixture-dir "test-projects/data-def-app/src/datadefapp")

(def ^:private defs-file (str fixture-dir "/defs.cljs"))

(def ^:private referred-file (str fixture-dir "/referred.cljs"))

(def ^:private schema-file (str fixture-dir "/schema.cljc"))

(def ^:private mention-fixture-dir "test-projects/data-def-app/src/datadefmention")

(defn- def-names-in
  "Name the fixture definition each finding in a file sits in, by the last def above its row."
  [file findings]
  (let [def-rows (->> (str/split-lines (slurp file))
                      (keep-indexed (fn [index line]
                                      (when-let [[_ def-name]
                                                 (re-find #"^(?:#\?\(:cljs )?\((?:defn-?|defonce|def) (?:\^\S+ )*(\S+)"
                                                          line)]
                                        [(inc index) def-name]))))]
    (->> findings
         (filter #(= file (:file %)))
         (map (fn [{:keys [row]}]
                (->> def-rows
                     (take-while #(<= (first %) row))
                     last
                     second)))
         set)))

(defn- result [rule-group]
  (-> (core/run fixture-dir [rule-group]) :group-results first))

(deftest data-def-a11y-fixture-test
  (let [{:keys [img-alt-missing label-not-associated]} (result a11y/group)]
    (is (= #{"bad-hiccup-image"
             "bad-toast"
             "bad-routes"
             "bad-actions"
             "bad-as-element-in-schema"
             "bad-full-as-element-in-schema"
             "bad-thread-as-element-in-schema"
             "bad-thread-last-as-element-in-schema"
             "bad-some-thread-as-element-in-schema"
             "bad-thread-call-as-element-in-schema"
             "bad-apply-as-element-in-schema"
             "bad-partial-as-element-in-schema"
             "bad-nested-thread-as-element-in-schema"
             "bad-composed-as-element-in-schema"
             "bad-look-alike-as-element-in-schema"
             "bad-unresolved-as-element-in-schema"
             "bad-thread-look-alike-as-element-in-schema"
             "bad-thread-call-look-alike-as-element-in-schema"
             "bad-apply-look-alike-as-element-in-schema"
             "bad-fn-literal-in-schema"
             "bad-garden-rule"
             "bad-cells"
             "bad-path-in-meta"
             "bad-path-in-reader-conditional"
             "bad-path-in-function"}
           (def-names-in defs-file img-alt-missing))
        "Hiccup outside data defs, and under #() or form holding symbol named as-element inside one, keeps its findings")
    (is (= #{"bad-referred-as-element-in-schema"}
           (def-names-in referred-file img-alt-missing))
        "referred as-element ends data def too")
    (is (= #{"bad-cljc-as-element-in-schema" "bad-reader-conditional-thread-step-in-schema"}
           (def-names-in schema-file img-alt-missing))
        "as-element required inside reader conditional ends data def")
    (is (= 2 (count (filter #(= #{"bad-cells"} (def-names-in defs-file [%])) img-alt-missing)))
        "both cells of keyword-only list stay Hiccup")
    (is (empty? label-not-associated)
        "Malli label entries in data defs are no labels")))

(deftest data-def-separator-no-longer-counts-toward-run
  (is (empty? (:missing-accessible-name (result a11y/group)))
      "Malli entry is no second focusable separator, so lone real separator is left alone"))

(deftest data-def-reagent-fixture-test
  (is (= #{"bad-empty-fragment-props"}
         (def-names-in defs-file (:empty-fragment-props (result reagent/group))))
      "fragment entry of Malli schema is data"))

(deftest data-def-forbid-props-fixture-test
  (is (= #{"bad-hiccup-button"}
         (def-names-in defs-file (:forbidden-prop (result (forbid-props/make-group [:aria-disabled])))))
      "forbidden prop counts in Hiccup def, not in Malli entry properties"))

(deftest data-def-react-testing-library-cljs-fixture-test
  (is (= #{"bad-save-button-by-test-id"}
         (->> react-testing-library-cljs/group result :test-id-query-on-role (map :test) set))
      "vector in data def is no element that shares role and name"))

(deftest data-def-entry-hides-no-test-id-query-on-role
  (is (some #(= "bad-save-button-by-test-id" (:test %))
            (:test-id-query-on-role (result react-testing-library-cljs/group)))
      "Malli entry with same role and name and no test id hides no test-id-query-on-role finding"))

(deftest data-def-test-id-mention-counts-with-its-vector
  (is (= ["bad-save-button-by-test-id"]
         (->> (core/run mention-fixture-dir [react-testing-library-cljs/group])
              :group-results
              first
              :test-id-query-on-role
              (map :test)))
      "test id mention in data def is left out like its vector, so element and mention counts agree"))

(defn- test-id-query-on-role-tests [dir]
  (->> (core/run dir [react-testing-library-cljs/group]) :group-results first :test-id-query-on-role (map :test)))

(deftest data-def-other-test-id-strings-count-as-mentions
  (is (empty? (test-id-query-on-role-tests "test-projects/data-def-app/src/datadefstrings"))
      "enum value and default in data defs are mentions, so their lookups are left alone"))

(deftest data-def-built-test-id-is-left-out-with-its-vector
  (is (= ["bad-save-button-by-test-id"] (test-id-query-on-role-tests "test-projects/data-def-app/src/datadefbuilt"))
      "built or quoted test id on data def vector is no mention"))

(deftest data-def-layout-fixture-test
  (is (= #{"bad-split-label-schema"}
         (def-names-in defs-file (:form-after-split-map (result layout/group))))
      "layout still reads Malli def"))

(def ^:private mixed-fixture-dir "test-projects/data-def-app/src/datadefmixed")

(def ^:private mixed-file (str mixed-fixture-dir "/views.cljs"))

(defn- mixed-result [rule-group]
  (-> (core/run mixed-fixture-dir [rule-group]) :group-results first))

(defn- noted-defs
  "Name the fixture definitions whose findings carry the mixed schema note."
  [findings]
  (def-names-in mixed-file (filter #(some-> (:hint %) (str/includes? "Malli schema")) findings)))

(deftest mixed-schema-note-on-each-group
  (is (= #{"bad-label-schema" "bad-alias-schema"} (-> a11y/group mixed-result :label-not-associated noted-defs))
      "a11y")
  (is (= #{"bad-button-schema"}
         (-> (forbid-props/make-group [:aria-disabled]) mixed-result :forbidden-prop noted-defs)))
  (is (= #{"bad-fragment-schema"} (-> reagent/group mixed-result :empty-fragment-props noted-defs))
      "reagent")
  (is (= [(str "Query it by role \"button\" and its name. Its element sits in a Malli schema at " mixed-file
               ":20, which reads as markup because of (fn [_ _] \"Bad\"). Move that part into its own def or function,"
               " so the schema reads as data.")
          "Query it by role \"button\" and its name."]
         (->> react-testing-library-cljs/group mixed-result :test-id-query-on-role (map :hint)))
      "react-testing-library-cljs"))

(deftest mixed-schema-note-leaves-schemas-that-a-cut-keeps-from-data
  (is (not-any? #{"cut-as-element-schema" "cut-fn-literal-schema" "cut-sibling-as-element-schema"}
                (-> a11y/group mixed-result :label-not-associated noted-defs))
      "as-element or fn literal in the value"))

(deftest mixed-schema-note-leaves-test-id-shared-with-real-element
  (is (= 1 (->> react-testing-library-cljs/group mixed-result :test-id-query-on-role (filter :hint)
                (filter #(str/includes? (:hint %) "Malli schema")) count))
      "only the save button lookup, whose every element sits in a schema"))

(deftest mixed-schema-note-on-run-level-a11y-findings
  (is (= #{"bad-notes-schema" "bad-separator-schema"}
         (-> a11y/group mixed-result :missing-accessible-name noted-defs))
      "unreferenced id and unnamed separator"))

(deftest mixed-schema-note-quotes-the-source
  (is (some #(some-> (:hint %) (str/includes? "[:> ui/button {:on-click identity} \"x\"]"))
            (-> (a11y/make-group {:component-aliases {'datadefmixed.ui/button :button}})
                mixed-result
                :label-not-associated))
      "component alias"))

(deftest mixed-schema-note-leaves-schemas-with-more-parts-and-the-part-itself
  (let [findings (:label-not-associated (mixed-result a11y/group))]
    (is (= #{"bad-label-schema" "bad-two-parts-schema" "bad-render-schema" "bad-alias-schema" "cut-as-element-schema"
             "cut-fn-literal-schema" "cut-sibling-as-element-schema"}
           (def-names-in mixed-file findings))
        "every label stays reported")
    (is (not-any? #{"bad-two-parts-schema" "bad-render-schema"} (noted-defs findings))
        "two parts, and a label inside the part")))
