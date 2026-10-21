(ns cljs-patrol.group-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(deftest docs-ref-test
  (is (= "v0.0.38" (group/docs-ref "0.0.38"))
      "release links its own tag")
  (is (= "main" (group/docs-ref "dev"))
      "build from source links main"))

(deftest docs-url-test
  (is (str/ends-with? (group/docs-url a11y/group :img-alt-missing)
                      (str "/blob/" (group/docs-ref group/tool-version) "/docs/rules/a11y.md#img-missing-alt"))
      "url names version, group page and rule anchor")
  (is (nil? (group/docs-url a11y/group :no-such-rule))
      "rule without section has no url"))

(deftest guides-test
  (is (= {:text (:img-alt-missing (group/suggestions a11y/group))
          :docs (group/docs-url a11y/group :img-alt-missing)}
         (:img-alt-missing (group/guides a11y/group)))
      "guide pairs suggestion with its docs url"))
