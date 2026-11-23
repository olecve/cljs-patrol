(ns cljs-patrol.dev.mutate.guard-test
  (:require
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.guard :as guard]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- write! [dir path text]
  (let [file (io/file dir path)]
    (io/make-parents file)
    (spit file text)))

(defn- set-time! [dir path millis] (.setLastModified (io/file dir path) millis))

(defn- at [seconds] (java.time.Instant/ofEpochSecond seconds))

(defn- tested [] nil)

(defn- started-guard
  "Start a guard on `dir` with the snapshot of now as its reference, and close its recording at once."
  [dir writers]
  (let [snapshot (guard/snapshot dir #{})
        file-guard (guard/start dir #{} snapshot snapshot writers)]
    (guard/close! file-guard)
    file-guard))

(defn- started-guard-from [dir before after]
  (let [file-guard (guard/start dir #{} before after #{})]
    (guard/close! file-guard)
    file-guard))

(defn- with-guard
  "Call `f` with a guard on `dir` whose reference is the snapshot of now, and close the guard after."
  [dir f]
  (let [snapshot (guard/snapshot dir #{})
        file-guard (guard/start dir #{} snapshot snapshot #{})]
    (try
      (f file-guard)
      (finally (guard/close! file-guard)))))

(deftest skipped-dirs-name-root-git-tool-caches-and-cache-folder
  (fixtures/with-temp-dir
    (fn [dir]
      (is (= #{".git" ".lsp" ".clj-kondo/.cache" ".cpcache" ".shadow-cljs" "cache/runs"}
             (guard/skipped-dirs dir (io/file dir "cache/runs"))))
      (is (= #{".git" ".lsp" ".clj-kondo/.cache" ".cpcache" ".shadow-cljs"}
             (guard/skipped-dirs dir (.getParentFile dir)))
          "cache folder outside root is no part of snapshot")
      (is (= (guard/skipped-dirs dir nil) (guard/skipped-dirs dir dir))
          "root itself is never skipped"))))

(deftest snapshot-maps-files-and-skips-what-skipped-dirs-name
  (fixtures/with-temp-dir
    (fn [dir]
      (doseq [path ["src/a.clj" ".git/HEAD" "runs/a.edn" ".lsp/db" ".clj-kondo/.cache/x" ".clj-kondo/config.edn"
                    "sub/.git/HEAD"]]
        (write! dir path "(ns a)"))
      (let [snapshot (guard/snapshot dir (guard/skipped-dirs dir (io/file dir "runs")))]
        (is (= #{"src/a.clj" ".clj-kondo/config.edn" "sub/.git/HEAD"} (set (keys snapshot)))
            "only root .git is skipped, and only cache of .clj-kondo")
        (is (= 6 (first (get snapshot "src/a.clj")))
            "size comes first")))))

(deftest snapshot-marks-folder-it-cannot-read
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "locked/a.txt" "x")
      (let [locked (io/file dir "locked")]
        (.setReadable locked false false)
        (try
          (is (= {"locked/" :unreadable} (guard/snapshot dir #{})))
          (finally (.setReadable locked true false)))))))

(deftest file-attributes-of-missing-file-are-nil
  (fixtures/with-temp-dir
    (fn [dir]
      (is (nil? (guard/file-attributes (.toPath (io/file dir "gone.txt")))))
      (is (some? (guard/file-attributes (.toPath dir)))))))

(deftest changes-name-added-changed-and-deleted-files
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "kept.txt" "same")
      (write! dir "edited.txt" "old")
      (write! dir "gone.txt" "x")
      (set-time! dir "edited.txt" 1000000)
      (let [before (guard/snapshot dir #{})]
        (is (= [] (guard/changes dir before before {})))
        (write! dir "edited.txt" "new")
        (set-time! dir "edited.txt" 2000000)
        (io/delete-file (io/file dir "gone.txt"))
        (write! dir "new.txt" "x")
        (is (= [["edited.txt" :changed] ["gone.txt" :deleted] ["new.txt" :added]]
               (guard/changes dir before (guard/snapshot dir #{}) {})))))))

(deftest changes-compare-bytes-of-file-baseline-wrote
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "log.txt" "up")
      (set-time! dir "log.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            hashes (:hashes (started-guard-from dir {} before))]
        (set-time! dir "log.txt" 2000000)
        (is (empty? (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same bytes written again")
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) {}))
            "no hash, so new time alone is change")
        (write! dir "log.txt" "dn")
        (set-time! dir "log.txt" 3000000)
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same size, other bytes")))))

(deftest start-looks-after-every-mutant-when-baseline-left-file-or-recorded-no-writers
  (fixtures/with-temp-dir
    (fn [dir]
      (let [before (guard/snapshot dir #{})
            _ (write! dir "left.txt" "x")
            after (guard/snapshot dir #{})
            start #(let [file-guard (guard/start dir #{} %1 %2 %3)] (guard/close! file-guard) file-guard)]
        (is (false? (:every-mutant? (start before before #{}))))
        (is (true? (:every-mutant? (start before before nil)))
            "plain baseline recorded no writers")
        (is (true? (:every-mutant? (start before after #{}))))
        (is (= ["left.txt"] (keys (:hashes (start before after #{})))))
        (is (empty? (:hashes (start after before #{})))
            "deleted file has no bytes to hash")))))

(deftest start-without-flight-recorder-says-so
  (fixtures/with-temp-dir
    (fn [dir]
      (with-redefs [guard/recorder-available? (constantly false)]
        (let [file-guard (started-guard dir nil)]
          (is (nil? (:recording file-guard)))
          (is (str/starts-with? (:note file-guard) "This JDK has no Java Flight Recorder")))
        (is (nil? (guard/start-recording))))
      (is (nil? (guard/baseline-writers {:test-runs []} [#'tested]))
          "no recording, no writers")
      (is (true? (guard/recorder-available?)))
      (is (nil? (:note (started-guard dir #{})))))))

(deftest check-looks-only-after-mutants-whose-tests-could-write
  (let [file-guard {:every-mutant? false
                    :writers #{:writer}}]
    (is (true? (guard/check? file-guard {:kind :subset
                                         :tests [:other :writer]})))
    (is (false? (guard/check? file-guard {:kind :subset
                                          :tests [:other]})))
    (is (true? (guard/check? file-guard {:kind :every}))
        "every test runs, writer too")
    (is (false? (guard/check? (assoc file-guard :writers #{}) {:kind :every})))
    (is (true? (guard/check? (assoc file-guard :every-mutant? true) {:kind :subset
                                                                     :tests [:other]})))))

(deftest look-names-mutant-whose-tests-wrote-file
  (fixtures/with-temp-dir
    (fn [dir]
      (with-guard dir
        (fn [file-guard]
          (is (nil? (guard/look file-guard "m0")))
          (let [started (java.time.Instant/now)]
            (write! dir "early.txt" "x")
            (guard/mark-mutant! file-guard "m0" started (java.time.Instant/now)))
          (Thread/sleep 20)
          (let [started (java.time.Instant/now)]
            (write! dir "late.txt" "x")
            (guard/mark-mutant! file-guard "m1" started (java.time.Instant/now)))
          (is (= {:changed [["early.txt" :added] ["late.txt" :added]]
                  :by "m1"}
                 (guard/look file-guard "m1"))
              "current mutant comes first")
          (is (= "m0" (:by (guard/look file-guard "m2")))
              "else first mutant whose window holds write")
          (is (= "m0" (:by (guard/look file-guard nil)))))))))

(deftest look-cannot-name-mutant-for-write-outside-every-mutant
  (fixtures/with-temp-dir
    (fn [dir]
      (with-guard dir
        (fn [file-guard]
          (write! dir "between.txt" "x")
          (is (= {:changed [["between.txt" :added]]
                  :by nil}
                 (guard/look file-guard "m1"))))))))

(deftest look-warns-once-for-file-another-process-wrote
  (fixtures/with-temp-dir
    (fn [dir]
      (with-guard dir
        (fn [file-guard]
          (shell/sh "sh" "-c" "echo x > saved.txt" :dir dir)
          (let [output (with-out-str (is (nil? (guard/look file-guard "m1"))))]
            (is (= "Warning: saved.txt changed during the run, but not by this run's tests.\n" output)))
          (is (= "" (with-out-str (is (nil? (guard/look file-guard "m2")))))
              "new state is reference now"))))))

(deftest look-counts-deleted-file-as-current-mutant
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "kept.txt" "x")
      (with-guard dir
        (fn [file-guard]
          (io/delete-file (io/file dir "kept.txt"))
          (is (= {:changed [["kept.txt" :deleted]]
                  :by "m1"}
                 (guard/look file-guard "m1")))
          (is (= {:changed [["kept.txt" :deleted]]
                  :by nil}
                 (guard/look file-guard nil))
              "no current mutant at end of run"))))))

(deftest look-without-recording-counts-every-change-as-current-mutant
  (fixtures/with-temp-dir
    (fn [dir]
      (with-redefs [guard/recorder-available? (constantly false)]
        (with-guard dir
          (fn [file-guard]
            (shell/sh "sh" "-c" "echo x > saved.txt" :dir dir)
            (is (= {:changed [["saved.txt" :added]]
                    :by "m1"}
                   (guard/look file-guard "m1")))))))))

(deftest entries-name-what-is-directly-in-root
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "a.txt" "x")
      (write! dir "sub/b.txt" "x")
      (is (= #{"a.txt" "sub"} (guard/entries dir))
          "names in subfolder are not read"))))

(deftest recording-gives-path-and-time-of-each-file-write
  (fixtures/with-temp-dir
    (fn [dir]
      (let [recording (guard/start-recording)
            file (io/file dir "a.txt")]
        (try
          (let [started (java.time.Instant/now)
                _ (spit file "x")
                ended (java.time.Instant/now)
                writes (filter #(= (str file) (:path %)) (guard/recorded-writes recording))]
            (is (= 1 (count writes)))
            (is (= #{#'tested} (guard/writers writes [[#'tested started ended false]] [#'tested #'at]))))
          (finally (guard/close-recording recording)))))))

(deftest writers-are-tests-with-write-in-their-time-or-changed-entries
  (let [writes [{:at (at 10)} {:at (at 30)}]]
    (is (= #{:during :at-start :at-end :entries}
           (guard/writers writes
                          [[:during (at 5) (at 15) false]
                           [:at-start (at 30) (at 31) false]
                           [:at-end (at 25) (at 30) false]
                           [:before (at 0) (at 9) false]
                           [:after (at 11) (at 29) false]
                           [:entries (at 40) (at 50) true]]
                          [:during :at-start :at-end :before :after :entries])))
    (is (= #{:near}
           (guard/writers [{:at (.minusMillis (at 10) 1)}] [[:near (at 10) (at 11) false]] [:near :other]))
        "write a little before run counts, since clocks differ")))

(deftest writers-are-every-test-when-write-falls-outside-every-test
  (is (= #{:first :second}
         (guard/writers [{:at (at 20)}] [[:first (at 0) (at 10) false]] [:first :second]))
      "fixture or background write belongs to no test"))

(deftest describe-lists-ten-files-and-counts-rest
  (let [changed (mapv #(vector (str "f" %) :added) (range 12))
        text (guard/describe changed)]
    (is (= "  added f0\n  deleted g" (guard/describe [["f0" :added] ["g" :deleted]])))
    (is (= 11 (count (str/split-lines text))))
    (is (str/ends-with? text "  added f9\n  and 2 more"))
    (is (not (str/includes? (guard/describe (take 10 changed)) "more"))
        "exactly ten files, nothing more to count")))

(deftest folder-name-has-no-dot-parts
  (fixtures/with-temp-dir
    (fn [dir]
      (is (= (-> dir .getAbsoluteFile .toPath str) (guard/folder-name (str dir "/.")))))))

(deftest close-stops-recording
  (fixtures/with-temp-dir
    (fn [dir]
      (let [recording (guard/start-recording)]
        (guard/close-recording recording)
        (is (= "CLOSED" (str (.getState ^jdk.jfr.Recording recording)))))
      (let [file-guard (guard/start dir #{} {} {} #{})]
        (guard/close! file-guard)
        (is (= "CLOSED" (str (.getState ^jdk.jfr.Recording (:recording file-guard)))))))))

(deftest look-names-first-mutant-that-wrote-file-more-than-once
  (fixtures/with-temp-dir
    (fn [dir]
      (with-guard dir
        (fn [file-guard]
          (doseq [id ["m0" "m1"]]
            (let [started (java.time.Instant/now)]
              (write! dir "again.txt" id)
              (guard/mark-mutant! file-guard id started (java.time.Instant/now)))
            (Thread/sleep 20))
          (is (= "m0" (:by (guard/look file-guard nil)))
              "one of several writes in window is enough"))))))
