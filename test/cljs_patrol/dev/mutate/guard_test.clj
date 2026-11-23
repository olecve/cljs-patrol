(ns cljs-patrol.dev.mutate.guard-test
  (:require
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.git :as git]
   [cljs-patrol.dev.mutate.guard :as guard]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- set-time! [dir path millis] (.setLastModified (io/file dir path) millis))

(defn- with-unreadable
  "Make `file` unreadable, call `f` when the JVM then cannot read it, and make it readable again.

  A JVM that runs as root can still read the file, so `f` does not run there."
  [^java.io.File file f]
  (.setReadable file false false)
  (try
    (when-not (.canRead file)
      (f))
    (finally (.setReadable file true false))))

(def ^:private editor-folders
  #{".lsp" ".clj-kondo/.cache" ".clj-kondo/imports" ".clj-kondo/inline-configs" ".idea" ".calva"})

(deftest skipped-dirs-name-root-git-and-cache-folder
  (fixtures/with-temp-dir
    (fn [dir]
      (is (= #{".git" "cache/runs"}
             (guard/skipped-dirs dir (io/file dir "cache/runs"))))
      (is (= #{".git"}
             (guard/skipped-dirs dir (.getParentFile dir)))
          "cache folder outside root is no part of snapshot")
      (is (= (guard/skipped-dirs dir nil) (guard/skipped-dirs dir dir))
          "root itself is never skipped"))))

(deftest with-ignored-adds-what-git-ignores-or-build-folders
  (is (= (into #{".git" "target" "a.log"} editor-folders)
         (guard/with-ignored #{".git"} #{"target/" "a.log"})))
  (is (= (into #{".git" ".cpcache" ".shadow-cljs"} editor-folders)
         (guard/with-ignored #{".git"} nil))
      "git cannot tell, so build folders are skipped by hand"))

(deftest tool-file-names-what-editors-and-tools-write
  (is (every? guard/tool-file? ["core.clj.swp" "core.clj.swo" ".#core.clj" "#core.clj#" "core.clj~" "4913" ".DS_Store"
                                ".nrepl-port"]))
  (is (not-any? guard/tool-file? ["core.clj" "swp" "4914" "14913" "a.#b" "#a" "a#" "~a" "x.DS_Store" ".nrepl-port.edn"
                                  "a.swpx" "#"])))

(deftest snapshot-maps-files-and-skips-what-skipped-dirs-name
  (fixtures/with-temp-dir
    (fn [dir]
      (doseq [path ["src/a.clj" ".git/HEAD" "runs/a.edn" ".lsp/db" ".clj-kondo/.cache/x" ".clj-kondo/config.edn"
                    "sub/.git/HEAD" "src/.a.clj.swp" "src/4913" ".idea/x.xml" ".calva/output.calva-repl"]]
        (fixtures/write! dir path "(ns a)"))
      (.mkdir (io/file dir "empty"))
      (let [snapshot (guard/snapshot dir (guard/with-ignored (guard/skipped-dirs dir (io/file dir "runs")) #{}))]
        (is (= #{"src/" "src/a.clj" ".clj-kondo/config.edn" "sub/" "sub/.git/" "sub/.git/HEAD" "empty/"}
               (set (keys snapshot)))
            "only root .git, what tools write in .clj-kondo, and files of tools are skipped")
        (is (= :folder (get snapshot "empty/"))
            "empty folder counts")
        (is (= 6 (first (get snapshot "src/a.clj")))
            "size comes first")))))

(deftest snapshot-marks-folder-it-cannot-read
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "locked/a.txt" "x")
      (with-unreadable (io/file dir "locked")
        #(is (= {"locked/" :unreadable} (guard/snapshot dir #{})))))))

(deftest file-attributes-of-missing-file-are-nil
  (fixtures/with-temp-dir
    (fn [dir]
      (is (nil? (guard/file-attributes (.toPath (io/file dir "gone.txt")))))
      (is (some? (guard/file-attributes (.toPath dir)))))))

(deftest changes-name-added-changed-and-deleted-files
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "kept.txt" "same")
      (fixtures/write! dir "edited.txt" "old")
      (fixtures/write! dir "gone.txt" "x")
      (set-time! dir "edited.txt" 1000000)
      (let [before (guard/snapshot dir #{})]
        (is (= [] (guard/changes dir before before {})))
        (fixtures/write! dir "edited.txt" "new")
        (set-time! dir "edited.txt" 2000000)
        (io/delete-file (io/file dir "gone.txt"))
        (fixtures/write! dir "new.txt" "x")
        (is (= [["edited.txt" :changed] ["gone.txt" :deleted] ["new.txt" :added]]
               (guard/changes dir before (guard/snapshot dir #{}) {})))))))

(deftest changes-compare-bytes-of-file-baseline-wrote
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "log.txt" "up")
      (set-time! dir "log.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            hashes (:hashes (guard/start dir #{} {} before false))]
        (set-time! dir "log.txt" 2000000)
        (is (empty? (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same bytes written again")
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) {}))
            "no hash, so new time alone is change")
        (fixtures/write! dir "log.txt" "dn")
        (set-time! dir "log.txt" 3000000)
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same size, other bytes")))))

(deftest start-hashes-files-baseline-added-or-changed
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "kept.txt" "x")
      (fixtures/write! dir "edited.txt" "x")
      (fixtures/write! dir "gone.txt" "x")
      (set-time! dir "edited.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            _ (fixtures/write! dir "left.txt" "x")
            _ (fixtures/write! dir "edited.txt" "y")
            _ (io/delete-file (io/file dir "gone.txt"))
            after (guard/snapshot dir #{})
            file-guard (guard/start dir #{".git"} before after false)]
        (is (= #{"left.txt" "edited.txt"} (-> file-guard :hashes keys set))
            "deleted file has no bytes to hash")
        (is (= after @(:files file-guard)))
        (is (= #{".git"} (:paths @(:rules file-guard))))))))

(deftest look-gives-changes-since-baseline
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "kept.txt" "x")
      (let [snapshot (guard/snapshot dir #{})
            file-guard (guard/start dir #{"skipped"} snapshot snapshot false)]
        (is (nil? (guard/look file-guard)))
        (fixtures/write! dir "skipped/a.txt" "x")
        (is (nil? (guard/look file-guard))
            "skipped folder stays out")
        (io/delete-file (io/file dir "kept.txt"))
        (fixtures/write! dir "new.txt" "x")
        (is (= [["kept.txt" :deleted] ["new.txt" :added]] (guard/look file-guard)))))))

(deftest describe-lists-ten-files-and-counts-rest
  (let [changed (mapv #(vector (str "f" %) :added) (range 12))
        text (guard/describe changed {})]
    (is (= "  added f0\n  deleted g" (guard/describe [["f0" :added] ["g" :deleted]] {})))
    (is (= 11 (count (str/split-lines text))))
    (is (str/ends-with? text "  added f9\n  and 2 more"))
    (is (not (str/includes? (guard/describe (take 10 changed) {}) "more"))
        "exactly ten files, nothing more to count")))

(deftest describe-tells-when-baseline-wrote-changed-file
  (is (= (str "  changed log.txt (the baseline wrote it too, and its bytes are not the same now)\n"
              "  changed other.txt\n"
              "  deleted gone.txt")
         (guard/describe [["log.txt" :changed] ["other.txt" :changed] ["gone.txt" :deleted]]
                         {"log.txt" "hash"
                          "gone.txt" "hash"}))))

(deftest folder-name-has-no-dot-parts
  (fixtures/with-temp-dir
    (fn [dir]
      (is (= (-> dir .getAbsoluteFile .toPath str) (guard/folder-name (str dir "/.")))))))

(deftest changes-name-new-and-deleted-folders-but-not-their-times
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "kept/a.txt" "x")
      (.mkdir (io/file dir "gone"))
      (let [before (guard/snapshot dir #{})]
        (fixtures/write! dir "kept/b.txt" "x")
        (.delete (io/file dir "gone"))
        (.mkdir (io/file dir "made"))
        (is (= [["gone/" :deleted] ["kept/b.txt" :added] ["made/" :added]]
               (guard/changes dir before (guard/snapshot dir #{}) {}))
            "time of kept/ changed, but folder compares by path")))))

(deftest changes-count-folder-that-turned-unreadable
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "locked/a.txt" "x")
      (let [before (guard/snapshot dir #{})]
        (with-unreadable (io/file dir "locked")
          #(is (= [["locked/" :changed] ["locked/a.txt" :deleted]]
                  (guard/changes dir before (guard/snapshot dir #{}) {"locked/" "hash"}))))))))

(deftest file-baseline-wrote-that-cannot-be-read-is-changed
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "log.txt" "up")
      (set-time! dir "log.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            hashes (:hashes (guard/start dir #{} {} before false))]
        (with-unreadable (io/file dir "log.txt")
          (fn []
            (set-time! dir "log.txt" 2000000)
            (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) hashes)))
            (is (empty? (:hashes (guard/start dir #{} {} (guard/snapshot dir #{}) false)))
                "no bytes, no hash")))))))

(deftest look-takes-snapshot-as-reference-when-nothing-changed
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "log.txt" "up")
      (set-time! dir "log.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            file-guard (guard/start dir #{} {} before false)]
        (set-time! dir "log.txt" 2000000)
        (is (nil? (guard/look file-guard)))
        (is (= (guard/snapshot dir #{}) @(:files file-guard))
            "same bytes again, new time is reference now")))))

(deftest describe-lists-files-before-folders
  (is (= "  added a.txt\n  added z.txt\n  added a/\n  deleted b/"
         (guard/describe [["a/" :added] ["a.txt" :added] ["b/" :deleted] ["z.txt" :added]] {}))))

(deftest look-skips-what-clj-kondo-writes-but-guards-its-config
  (fixtures/with-temp-dir
    (fn [dir]
      (let [skipped (guard/with-ignored (guard/skipped-dirs dir nil) #{})
            snapshot (guard/snapshot dir skipped)
            file-guard (guard/start dir skipped snapshot snapshot false)]
        (fixtures/write! dir ".clj-kondo/.cache/v1/x.transit.json" "x")
        (fixtures/write! dir ".clj-kondo/imports/lib/config.edn" "{}")
        (fixtures/write! dir ".clj-kondo/inline-configs/core.clj/config.edn" "{}")
        (is (nil? (guard/look file-guard))
            "clj-kondo and clojure-lsp made these while run went on, folder too")
        (fixtures/write! dir ".clj-kondo/config.edn" "{}")
        (is (= [[".clj-kondo/config.edn" :added]] (guard/look file-guard))
            "hand-written config counts")))))

(deftest snapshot-leaves-out-skipped-file-and-folder-that-holds-it
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/write! dir "src/a.clj" "x")
      (fixtures/write! dir "src/b.log" "x")
      (is (= #{"src/a.clj"} (set (keys (guard/snapshot dir #{"src/b.log"}))))))))

(defn- git-guard
  "Start a guard on `dir` that skips what git ignores, as a run does after its baseline."
  [dir]
  (let [skipped (guard/with-ignored (guard/skipped-dirs dir nil) (git/ignored-paths dir))
        snapshot (guard/snapshot dir skipped)]
    (guard/start dir skipped snapshot snapshot true)))

(deftest look-skips-ignored-folder-that-exists-at-start
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "target/")
      (fixtures/write! dir "target/classes/old.clj" "x")
      (let [file-guard (git-guard dir)]
        (is (not-any? #(str/starts-with? % "target") (keys @(:files file-guard))))
        (fixtures/write! dir "target/classes/app/stale.clj" "x")
        (is (nil? (guard/look file-guard)))))))

(deftest look-skips-new-folder-git-ignores-from-then-on
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "target/" "out")
      (fixtures/write! dir "src/a.clj" "x")
      (let [file-guard (git-guard dir)
            skipped (:paths @(:rules file-guard))]
        (fixtures/write! dir "target/classes/app/stale.clj" "x")
        (fixtures/write! dir "out/compiled.cljs" "x")
        (is (nil? (guard/look file-guard))
            "fresh clone builds target/ and out/")
        (is (= (conj skipped "target" "out") (:paths @(:rules file-guard)))
            "only top ignored folders join skipped paths")
        (fixtures/write! dir "target/classes/app/other.clj" "x")
        (is (nil? (guard/look file-guard))
            "reference keeps src/")
        (fixtures/write! dir "src/kept.clj" "x")
        (is (= [["src/kept.clj" :added]] (guard/look file-guard)))))))

(deftest look-names-only-new-paths-git-does-not-ignore
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "*.log")
      (let [file-guard (git-guard dir)
            reference @(:files file-guard)]
        (fixtures/write! dir "run.log" "x")
        (fixtures/write! dir "note.txt" "x")
        (is (= [["note.txt" :added]] (guard/look file-guard)))
        (is (= reference @(:files file-guard))
            "change keeps reference")))))

(deftest look-skips-new-folder-that-holds-only-ignored-paths
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "*.log")
      (let [file-guard (git-guard dir)]
        (fixtures/write! dir "logs/run.log" "x")
        (is (nil? (guard/look file-guard)))
        (fixtures/write! dir "logs/note.txt" "x")
        (is (= [["logs/note.txt" :added]] (guard/look file-guard))
            "folder that holds skipped path is no part of snapshot")))))

(deftest look-keeps-folder-out-once-it-holds-new-ignored-file
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "*.log")
      (fixtures/write! dir "src/a.clj" "x")
      (let [file-guard (git-guard dir)]
        (fixtures/write! dir "src/run.log" "x")
        (is (nil? (guard/look file-guard)))
        (is (nil? (guard/look file-guard))
            "reference drops src/ as later snapshots do")))))

(deftest look-outside-git-counts-what-git-would-ignore
  (fixtures/with-temp-dir
    (fn [dir]
      (spit (io/file dir ".gitignore") "target/\n")
      (fixtures/outside-git dir
                            (fn []
                              (let [skipped (guard/with-ignored (guard/skipped-dirs dir nil) (git/ignored-paths dir))
                                    snapshot (guard/snapshot dir skipped)
                                    file-guard (guard/start dir skipped snapshot snapshot false)]
                                (fixtures/write! dir ".cpcache/x.cp" "x")
                                (is (nil? (guard/look file-guard))
                                    "build folder on hand list")
                                (fixtures/write! dir "target/a.clj" "x")
                                (is (= [["target/" :added] ["target/a.clj" :added]] (guard/look file-guard)))))))))

(deftest look-asks-git-only-about-added-paths
  (fixtures/with-temp-dir
    (fn [dir]
      (fixtures/git-repo! dir "*.log")
      (fixtures/write! dir "kept.txt" "x")
      (let [file-guard (git-guard dir)
            asked (atom [])
            ignored-among git/ignored-among]
        (with-redefs [git/ignored-among (fn [root paths] (swap! asked conj paths) (ignored-among root paths))]
          (fixtures/write! dir "kept.txt" "xy")
          (is (= [["kept.txt" :changed]] (guard/look file-guard)))
          (is (empty? @asked)
              "changed file was not ignored at start")
          (fixtures/write! dir "run.log" "x")
          (is (= [["kept.txt" :changed]] (guard/look file-guard)))
          (is (= [["run.log"]] @asked)))))))

(deftest look-from-ignored-folder-asks-git-nothing
  (fixtures/with-temp-dir
    (fn [repo]
      (fixtures/git-repo! repo "build/")
      (let [dir (io/file repo "build" "x")
            ignored (git/ignored-paths dir)
            skipped (guard/with-ignored (guard/skipped-dirs dir nil) ignored)
            snapshot (guard/snapshot dir skipped)
            file-guard (guard/start dir skipped snapshot snapshot (some? ignored))]
        (is (nil? ignored))
        (fixtures/write! dir "kept.clj" "x")
        (is (= [["kept.clj" :added]] (guard/look file-guard))
            "git calls every new path here ignored")))))
