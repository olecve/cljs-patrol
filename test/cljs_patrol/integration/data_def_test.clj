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
             "bad-fn-literal-in-schema"
             "bad-garden-rule"
             "bad-cells"
             "bad-path-in-meta"
             "bad-path-in-reader-conditional"
             "bad-path-in-function"}
           (def-names-in defs-file img-alt-missing))
        "Hiccup outside data defs, and under #() or as-element inside one, keeps its findings")
    (is (= #{"bad-referred-as-element-in-schema"}
           (def-names-in referred-file img-alt-missing))
        "referred as-element ends data def too")
    (is (= 2 (count (filter #(= #{"bad-cells"} (def-names-in defs-file [%])) img-alt-missing)))
        "both cells of keyword-only list stay Hiccup")
    (is (empty? label-not-associated)
        "Malli label entries in data defs are no labels")))

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

(deftest data-def-layout-fixture-test
  (is (= #{"bad-split-label-schema"}
         (def-names-in defs-file (:form-after-split-map (result layout/group))))
      "layout still reads Malli def"))
