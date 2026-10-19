(ns cljs-patrol.docs-examples-test
  "Run every example in docs/rules/ through the rule it illustrates.

  A rule's section opens with its key, then a block whose `;; flagged` part holds code the
  rule reports and whose `;; fix` part holds the same code rewritten, and a `**Left alone:**`
  block of code it does not report. Each blank-line-separated chunk is analyzed as a file of
  its own, so an example cannot lean on another, and the docs cannot drift from the tool."
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private assemble-groups #'core/assemble-groups)

(def ^:private default-requires
  "Aliases an example may use without writing its own ns form."
  (str "(:require [re-frame.core :as rf]"
       " [spade.core :refer [defclass defattrs]]"
       " [clojure.string :as str]"
       " [clojure.test :refer [deftest is testing]])"))

(defn- chunks
  "Split code into the blank-line-separated chunks it holds."
  [code]
  (->> (str/split code #"\n\s*\n")
       (map str/trim)
       (remove str/blank?)))

(defn- code-blocks [text]
  (map second (re-seq #"(?s)```clojure\n(.*?)```" text)))

(defn- rule-sections
  "Return {:rule :flagged :fix :left-alone} for every rule section of a docs page."
  [text]
  (for [section (rest (str/split text #"(?m)^### "))
        :let [rule (some->> section (re-find #"(?m)^`([^`]+)` ·") second keyword)
              example (first (code-blocks section))
              left-alone (some->> (str/split section #"(?m)^\*\*Left alone:\*\*$" 2) second code-blocks first)
              [_ flagged fix] (when example (re-find #"(?s);; flagged\n(.*?)(?:;; fix\n(.*))?$" example))]
        :when rule]
    {:rule rule
     :flagged (some-> flagged chunks)
     :fix (some-> fix chunks)
     :left-alone (some-> left-alone chunks)}))

(defn- findings
  "Analyze code as the only file of a project and return how many findings rule-group gives for rule."
  [rule-group rule code]
  (let [test-ns? (= :tests (group/group-id rule-group))
        ns-name (if test-ns? "example.core-test" "example.core")
        source (if (str/starts-with? code "(ns ")
                 code
                 (str "(ns " ns-name " " default-requires ")\n\n" code "\n"))
        dir (.toFile (java.nio.file.Files/createTempDirectory "docs-example" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file dir (if test-ns? "core_test.cljs" "core.cljs"))]
    (try
      (spit file source)
      (let [result (binding [*err* (java.io.StringWriter.)]
                     (-> (core/run (str dir) [rule-group]) :group-results first))]
        (count (get result rule)))
      (finally
        (.delete file)
        (.delete dir)))))

(deftest rule-examples-match-the-tool
  (let [groups (into {} (map (juxt group/group-id identity)) (assemble-groups {} false))
        pages (->> (io/file "docs/rules") .listFiles (filter #(str/ends-with? (.getName ^java.io.File %) ".md")))]
    (is (seq pages))
    (doseq [^java.io.File page pages
            :let [group-id (-> page .getName (str/replace #"\.md$" "") keyword)
                  rule-group (get groups group-id)
                  sections (rule-sections (slurp page))]]
      (testing (.getName page)
        (is (some? rule-group)
            "every page is named after a group")
        (is (seq sections)
            "every page shows its rules by example")
        (doseq [{:keys [rule flagged fix left-alone]} sections]
          (testing (name rule)
            (is (seq flagged)
                "every rule shows code it flags")
            (doseq [code flagged]
              (is (pos? (findings rule-group rule code))
                  (str "flagged example is not flagged:\n" code)))
            (doseq [code (concat fix left-alone)]
              (is (zero? (findings rule-group rule code))
                  (str "fix or left-alone example is flagged:\n" code)))))))))
