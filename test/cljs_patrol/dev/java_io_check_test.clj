(ns cljs-patrol.dev.java-io-check-test
  (:require
   [cljs-patrol.dev.java-io-check :as java-io-check]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.edn :as edn]
   [clojure.string :as str]
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
                                      (class-usage "java.io.RandomAccessFile" 5)
                                      (class-usage "java.io.BufferedWriter" 6)
                                      (class-usage "java.io.PrintWriter" 6)
                                      (class-usage "java.io.PrintStream" 6)
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
            "src/a.clj:5:1: java.io.RandomAccessFile"
            "src/a.clj:8:2: .toFile"]
           (map java-io-check/hit-line (java-io-check/hits analysis)))
        "File classes, RandomAccessFile and .toFile, once per place, sorted; other classes and methods left alone")))

(deftest hits-gives-nothing-for-empty-analysis
  (is (empty? (java-io-check/hits {}))))

(deftest hits-drops-use-with-no-row-when-same-class-has-row
  (let [rowless {:class "java.io.FileFilter"
                 :filename "src/a.clj"}]
    (is (= ["src/a.clj:1:26: java.io.FileFilter"]
           (map java-io-check/hit-line
                (java-io-check/hits {:java-class-usages [rowless (assoc rowless :row 1 :col 26)]})))
        "reify gives one use with a place and one without")
    (is (= ["src/a.clj: java.io.FileFilter"]
           (map java-io-check/hit-line (java-io-check/hits {:java-class-usages [rowless]})))
        "use with no row stays when it is the only one")))

(defn- lint-string [code]
  (with-in-str code
    (java-io-check/lint {:lint ["-"]
                         :filename "src/a.clj"})))

(deftest lint-gives-hits-with-row-and-column
  (let [result (lint-string (str "(ns a)\n"
                                 "(defn f [x p]\n"
                                 "  [(java.io.File. x)\n"
                                 "   (.toFile p)])\n"))]
    (is (empty? (java-io-check/lint-problems result)))
    (is (= ["src/a.clj:3:4: java.io.File"
            "src/a.clj:4:5: .toFile"]
           (map java-io-check/hit-line (java-io-check/hits (:analysis result)))))))

(deftest lint-problems-names-error-findings
  (is (= ["src/a.clj:1:8: clj-kondo error: Found an opening ( with no matching )"
          "src/a.clj:1:9: clj-kondo error: Expected a ) to match ( from line 1"]
         (java-io-check/lint-problems (lint-string "(ns a) (")))))

(deftest lint-problems-names-run-with-no-file
  (is (= ["clj-kondo linted no file."]
         (java-io-check/lint-problems {:findings []
                                       :summary {:files 0}}))))

(defn- run-cli [paths]
  (let [exit (atom nil)
        out (with-out-str (reset! exit (java-io-check/run-cli paths)))]
    [@exit (str/split-lines out)]))

(defn- run-cli-on-code [code]
  (temp-dir/with-dir "java-io-check-"
    (fn [dir]
      (spit (str dir "/a.clj") code)
      (run-cli [(str dir)]))))

(deftest run-cli-gives-1-on-hit
  (let [[exit lines] (run-cli-on-code "(ns a)\n(defn f [x] (java.io.File. x))\n")]
    (is (= 1 exit))
    (is (str/ends-with? (first lines) "a.clj:2:13: java.io.File"))))

(deftest run-cli-gives-0-on-clean-code
  (is (= [0 [""]] (run-cli-on-code "(ns a)\n(defn f [x] x)\n"))))

(deftest run-cli-gives-2-on-clj-kondo-error
  (let [[exit lines] (run-cli-on-code "(ns a) (")]
    (is (= 2 exit))
    (is (= "The check cannot run. Fix the clj-kondo errors above." (last lines)))))

(deftest run-cli-gives-2-on-missing-path
  (is (= [2 ["nosuch:0:0: clj-kondo error: file does not exist"
             "clj-kondo linted no file."
             "The check cannot run. Fix the clj-kondo errors above."]]
         (run-cli ["nosuch"]))))

(deftest run-cli-gives-2-on-folder-with-no-file
  (is (= [2 ["clj-kondo linted no file."
             "The check cannot run. Fix the clj-kondo errors above."]]
         (temp-dir/with-dir "java-io-check-" #(run-cli [(str %)])))))

(deftest run-cli-gives-2-with-no-path
  (is (= [2 ["Give the paths to check, such as src."]]
         (run-cli []))))

(deftest test-alias-pins-clj-kondo-version-of-clj-kondo-alias
  (let [aliases (:aliases (edn/read-string (slurp "deps.edn")))
        version-in (fn [alias] (get-in aliases [alias :extra-deps 'clj-kondo/clj-kondo :mvn/version]))]
    (is (some? (version-in :clj-kondo)))
    (is (= (version-in :clj-kondo) (version-in :test))
        "tests run the java-io check on the clj-kondo that CI lints with")))
