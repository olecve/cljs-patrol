(ns cljs-patrol.dev.mutate.apply-test
  (:require
   [cljs-patrol.dev.mutate.apply :as apply]
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [clojure.java.io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(deftest relative-path-starts-from-the-working-directory
  (is (= "src/app.clj" (apply/relative-path "src/app.clj")))
  (is (= (.getCanonicalPath (clojure.java.io/file "/tmp/app.clj")) (apply/relative-path "/tmp/app.clj"))
      "file outside the working directory keeps its whole path"))

(deftest apply-mutant-writes-one-swap-and-restore-puts-code-back
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")
            kept (str (clojure.java.io/file dir "applied.edn"))
            text "(ns app.core)\n(defn label [x]\n  (if (> x 0) :up :down))\n; end\n"]
        (spit file text)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"takes ROW:COL:OPERATOR, such as '490:43:remove', not 3:8"
                              (apply/apply-mutant (str file) "3:8" kept)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No mutant at 3:5\. Line 3 has 3:4 if, 3:8 >, 3:12 0\. Run the report"
                              (apply/apply-mutant (str file) "3:5:if" kept))
            "names the mutants of that line")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No mutant at 9:1\. Run the report"
                              (apply/apply-mutant (str file) "9:1:if" kept)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"At 3:8 the operator is now >, not <"
                              (apply/apply-mutant (str file) "3:8:<" kept))
            "position from older report")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No mutant is applied"
                              (apply/restore-mutant kept)))
        (is (str/includes? (apply/apply-mutant (str file) "3:8:>" kept)
                           (str "Applied 3:8  > -> >=  in label to " (apply/relative-path file) ".\n"
                                "Run the tests and watch the new test fail, then put the code back with:\n"
                                "  clojure -M:test:mutate --restore")))
        (is (= "(ns app.core)\n(defn label [x]\n  (if (>= x 0) :up :down))\n; end\n" (slurp file))
            "only that token changes")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already applied to .*core.clj\. Put the code back with --restore"
                              (apply/apply-mutant (str file) "3:4:if" kept)))
        (is (= (str "Restored " (apply/relative-path file) ".") (apply/restore-mutant kept)))
        (is (= text (slurp file)))
        (is (not (.exists (clojure.java.io/file kept)))
            "kept text is removed once restored")
        (apply/apply-mutant (str file) "3:4:if" kept)
        (spit file (str (slurp file) "(defn new [])\n"))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"changed since the mutant was applied, so --restore leaves it alone"
                              (apply/restore-mutant kept)))
        (is (str/includes? (slurp file) "(defn new [])")
            "later edit kept")))))

(deftest apply-mutant-writes-mutant-under-metadata-like-a-run
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")
            kept (str (clojure.java.io/file dir "applied.edn"))]
        (spit file "(ns app.core)\n^:checked\n(defn f [x] (when x 1))\n")
        (apply/apply-mutant (str file) "3:14:when" kept)
        (is (= "(ns app.core)\n^:checked\n(defn f [x] (when-not x 1))\n" (slurp file)))))))

(deftest apply-mutant-keeps-line-endings-and-recovers-from-mishaps
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")
            kept (str (clojure.java.io/file dir "applied.edn"))
            text "(ns app.core)\r\n(defn label [x]\r\n  (if (> x 0) :up :down))\r\n"]
        (spit file text)
        (apply/apply-mutant (str file) "3:8:>" kept)
        (is (= "(ns app.core)\r\n(defn label [x]\r\n  (if (>= x 0) :up :down))\r\n" (slurp file))
            "crlf line endings kept")
        (spit file text)
        (is (str/ends-with? (apply/restore-mutant kept) "was already back as before the apply.")
            "file put back by hand clears kept text")
        (is (not (.exists (clojure.java.io/file kept))))
        (spit kept "")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold what --apply keeps\. If no mutant is applied, delete it"
                              (apply/apply-mutant (str file) "3:8:>" kept)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold what --apply keeps"
                              (apply/restore-mutant kept)))
        (spit kept (pr-str {:file (str file)}))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold what --apply keeps"
                              (apply/restore-mutant kept))
            "kept text missing its parts")
        (clojure.java.io/delete-file kept)
        (spit file "(ns app.core)\r(defn label [x] (when x 1))\n")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The text at 2:18 of .* is not when, so nothing was written"
                              (apply/apply-mutant (str file) "2:18:when" kept))
            "lone carriage return shifts lines")
        (is (not (.exists (clojure.java.io/file kept))))
        (spit file text)
        (with-redefs [apply/write-whole (fn [_ _] (throw (java.io.IOException. "disk full")))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Could not write .*, which is left as it was: disk full"
                                (apply/apply-mutant (str file) "3:8:>" kept))))
        (is (= text (slurp file)))
        (is (not (.exists (clojure.java.io/file kept)))
            "failed write leaves no kept text")))))

(deftest write-whole-replaces-file-and-leaves-no-temporary-file
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")]
        (spit file "old")
        (apply/write-whole (str file) "new")
        (is (= "new" (slurp file)))
        (is (= ["core.clj"] (map #(.getName %) (.listFiles dir))))))))

(deftest restore-mutant-leaves-gone-file-to-user
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")
            kept (str (clojure.java.io/file dir "applied.edn"))]
        (spit file "(ns app.core)\n(defn f [x] (when x 1))\n")
        (apply/apply-mutant (str file) "2:14:when" kept)
        (clojure.java.io/delete-file file)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"core.clj is gone, so --restore cannot put it back\. The text from before the apply is in"
                              (apply/restore-mutant kept)))
        (is (.exists (clojure.java.io/file kept)))))))
