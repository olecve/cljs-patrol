(ns cljs-patrol.dev.java-io-check-test
  (:require
   [cljs-patrol.dev.java-io-check :as java-io-check]
   [cljs-patrol.temp-dir :as temp-dir]
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

(def ^:private fixture
  "Code with two hits, an unresolved symbol and, on its last line, a syntax error."
  (str "(ns a)\n"
       "(defn f [x p]\n"
       "  [(java.io.File. x)\n"
       "   (.toFile p)\n"
       "   (unknown x)])\n"
       "("))

(def ^:private fixture-result
  "The one lint of [[fixture]] by real clj-kondo, with a caller `:config` that tries to turn the analysis off."
  (delay (with-in-str fixture
           (java-io-check/lint {:lint ["-"]
                                :filename "src/a.clj"
                                :config {:analysis {:java-class-usages false
                                                    :instance-invocations false}}}))))

(defn- without-syntax-findings [result]
  (update result :findings #(remove (comp #{:syntax} :type) %)))

(defn- report [result]
  (let [exit (atom nil)
        out (with-out-str (reset! exit (java-io-check/report result)))]
    [@exit (str/split-lines out)]))

(deftest lint-keeps-analysis-that-caller-config-turns-off
  (is (= ["src/a.clj:3:4: java.io.File"
          "src/a.clj:4:5: .toFile"]
         (map java-io-check/hit-line (java-io-check/hits (:analysis @fixture-result))))))

(deftest lint-problems-names-syntax-findings-only
  (let [{:keys [findings]} @fixture-result
        problems (java-io-check/lint-problems @fixture-result)]
    (is (some #(= :unresolved-symbol (:type %)) findings)
        "fixture has an error finding that is not about syntax")
    (is (= (count (filter (comp #{:syntax} :type) findings)) (count problems)))
    (is (every? #(re-find #"^src/a\.clj:6:\d+: clj-kondo error: " %) problems))))

(deftest lint-problems-names-missing-path-without-place
  (is (= ["nosuch: clj-kondo error: file does not exist"
          "clj-kondo linted no file."]
         (java-io-check/lint-problems {:findings [{:filename "nosuch"
                                                   :type :file
                                                   :level :error
                                                   :row 0
                                                   :col 0
                                                   :message "file does not exist"}]
                                       :summary {:files 0}}))))

(deftest lint-problems-names-run-with-no-file
  (is (= ["clj-kondo linted no file."]
         (java-io-check/lint-problems {:findings []
                                       :summary {:files 0}}))))

(deftest lint-problems-reads-missing-summary-as-no-file
  (is (= ["clj-kondo linted no file."]
         (java-io-check/lint-problems {}))))

(deftest report-gives-2-on-syntax-error
  (let [[exit lines] (report @fixture-result)]
    (is (= 2 exit))
    (is (= "The check cannot run. Fix the clj-kondo errors above." (last lines)))))

(deftest report-gives-1-on-hit-next-to-other-error-finding
  (let [[exit lines] (report (without-syntax-findings @fixture-result))]
    (is (= 1 exit))
    (is (= ["src/a.clj:3:4: java.io.File"
            "src/a.clj:4:5: .toFile"
            "Found java.io file access. Use the Path functions of cljs-patrol.fs."]
           lines))))

(deftest report-gives-0-on-clean-result
  (is (= [0 [""]] (report {:summary {:files 1}}))))

(defn- run-cli [paths]
  (let [exit (atom nil)
        out (with-out-str (reset! exit (java-io-check/run-cli paths)))]
    [@exit (str/split-lines out)]))

(deftest run-cli-gives-1-on-hit-in-file
  (let [[exit lines] (temp-dir/with-dir "java-io-check-"
                       (fn [dir]
                         (spit (str dir "/a.clj") "(ns a)\n(defn f [x] (java.io.File. x))\n")
                         (run-cli [(str dir)])))]
    (is (= 1 exit))
    (is (str/ends-with? (first lines) "a.clj:2:13: java.io.File"))))

(deftest run-cli-gives-2-with-no-path
  (is (= [2 ["Give the paths to check, such as src."]]
         (run-cli []))))
