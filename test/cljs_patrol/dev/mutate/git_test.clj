(ns cljs-patrol.dev.mutate.git-test
  (:require
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.git :as git]
   [cljs-patrol.temp-dir :as temp-dir]
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
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (let [file (clojure.java.io/file dir "core.clj")]
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
                              (git/lines-since (str file) "nope")))))))

(deftest ignored-paths-name-ignored-folders-once-and-files-relative-to-dir
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/git-repo! dir "target/" "*.log")
      (doseq [path ["target/classes/a.clj" "sub/target/b.clj" "sub/c.log" "d.log" "src/e.clj" "logs/f.log"]]
        (fixtures/write! dir path "x"))
      (is (= #{"target/" "sub/target/" "sub/c.log" "d.log" "logs/f.log"} (git/ignored-paths dir))
          "logs/ holds only ignored files, but no rule ignores logs/ itself")
      (is (= #{"target/" "c.log"} (git/ignored-paths (clojure.java.io/file dir "sub")))
          "paths under subfolder, relative to it"))))

(deftest ignored-paths-in-git-with-nothing-ignored-are-empty
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/git-repo! dir)
      (fixtures/write! dir "src/a.clj" "x")
      (is (= #{} (git/ignored-paths dir))))))

(deftest ignored-paths-outside-git-are-nil
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/outside-git dir #(is (nil? (git/ignored-paths dir)))))))

(deftest ignored-paths-in-ignored-folder-are-nil
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/git-repo! dir "build/")
      (fixtures/write! dir "build/x/a.clj" "x")
      (fixtures/write! dir "build/t/kept.clj" "x")
      (sh! dir "git" "add" "-f" "build/t/kept.clj")
      (is (nil? (git/ignored-paths (clojure.java.io/file dir "build")))
          "git ignores dir itself")
      (is (nil? (git/ignored-paths (clojure.java.io/file dir "build/x")))
          "git ignores folder above dir")
      (is (nil? (git/ignored-paths (clojure.java.io/file dir "build/t")))
          "tracked file in ignored folder")
      (is (= #{"build/x/"} (git/ignored-paths dir))))))

(deftest ignored-among-matches-paths-against-ignore-rules
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/git-repo! dir "target/" "*.log")
      (fixtures/write! dir "kept.log" "x")
      (sh! dir "git" "add" "-f" "kept.log")
      (is (= #{"target/" "target/classes/app/stale.clj" "sub/new.log"}
             (git/ignored-among dir ["target/" "target/classes/app/stale.clj" "sub/new.log" "src/kept.clj" "kept.log"]))
          "paths need not exist, and tracked file is not ignored")
      (is (= #{} (git/ignored-among dir ["src/kept.clj"]))))))

(deftest ignored-among-outside-git-ignores-nothing
  (temp-dir/with-dir "mutate"
    (fn [dir]
      (fixtures/outside-git dir #(is (= #{} (git/ignored-among dir ["target/"])))))))
