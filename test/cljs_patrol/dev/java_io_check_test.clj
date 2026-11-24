(ns cljs-patrol.dev.java-io-check-test
  (:require
   [cljs-patrol.dev.java-io-check :as java-io-check]
   [clojure.test :refer [deftest is]]))

(defn- class-usage [class row]
  {:class class
   :filename "src/a.clj"
   :row row
   :col 1})

(deftest hits-finds-file-classes-and-to-file-calls
  (let [analysis {:java-class-usages [(class-usage "java.io.FileReader" 3)
                                      (class-usage "java.io.File" 2)
                                      (class-usage "java.io.File" 2)
                                      (class-usage "java.io.FileInputStream" 4)
                                      (class-usage "java.io.BufferedWriter" 5)
                                      (class-usage "java.nio.file.Files" 6)
                                      (class-usage "my.java.io.File" 7)]
                  :instance-invocations [{:method-name "toFile"
                                          :filename "src/a.clj"
                                          :name-row 8
                                          :name-col 2}
                                         {:method-name "toPath"
                                          :filename "src/a.clj"
                                          :name-row 9
                                          :name-col 2}]}]
    (is (= ["src/a.clj:2:1: java.io.File"
            "src/a.clj:3:1: java.io.FileReader"
            "src/a.clj:4:1: java.io.FileInputStream"
            "src/a.clj:8:2: .toFile"]
           (map java-io-check/hit-line (java-io-check/hits analysis)))
        "File classes and .toFile, once per place, sorted; other classes and methods left alone")))

(deftest hits-gives-nothing-for-empty-analysis
  (is (empty? (java-io-check/hits {}))))
