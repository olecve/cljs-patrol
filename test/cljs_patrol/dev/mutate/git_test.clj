(ns cljs-patrol.dev.mutate.git-test
  (:require
   [cljs-patrol.dev.mutate.git :as git]
   [clojure.java.io]
   [clojure.java.shell]
   [clojure.test :refer [deftest is]]))

(deftest changed-lines-read-git-diff-hunks
  (is (= #{3 7 8 9 11}
         (git/changed-lines (str "@@ -3 +3 @@\n-old\n+new\n"
                                 "@@ -10,0 +7,3 @@\n+a\n+b\n+c\n"
                                 "@@ -20,2 +11,0 @@\n-x\n-y\n")))
      "one changed line, three added lines, and the line a deletion follows"))

(defn- sh! [dir & args]
  (let [{:keys [exit err]} (apply clojure.java.shell/sh (concat args [:dir dir]))]
    (assert (zero? exit) err)))

(deftest lines-since-reads-the-branch-changes-from-the-merge-base
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "mutate" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (clojure.java.io/file dir "core.clj")]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "user.email" "t@example.com")
    (sh! dir "git" "config" "user.name" "t")
    (spit file "a\nb\nc\n")
    (sh! dir "git" "add" ".")
    (sh! dir "git" "commit" "-q" "-m" "base")
    (sh! dir "git" "switch" "-q" "-c" "topic")
    (spit file "a\nB\nc\n")
    (is (= #{2} (git/lines-since (str file) "main")))
    (spit (clojure.java.io/file dir "fresh.clj") "x\n")
    (is (= :all-lines (git/lines-since (str (clojure.java.io/file dir "fresh.clj")) "main"))
        "file the merge base lacks counts in full")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No git commit named nope"
                          (git/lines-since (str file) "nope")))))
