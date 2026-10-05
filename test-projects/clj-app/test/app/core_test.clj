(ns app.core-test
  (:require
   [app.core :as core]
   [clojure.test :refer [deftest is]]))

(deftest summarize-test
  (is (= 1 (core/summarize 1)) "the message shares the line"))
