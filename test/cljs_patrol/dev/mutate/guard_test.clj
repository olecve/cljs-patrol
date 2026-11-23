(ns cljs-patrol.dev.mutate.guard-test
  (:require
   [cljs-patrol.dev.mutate.fixtures :as fixtures]
   [cljs-patrol.dev.mutate.guard :as guard]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn- write! [dir path text]
  (let [file (io/file dir path)]
    (io/make-parents file)
    (spit file text)))

(defn- tested [] nil)

(defn- set-time! [dir path millis] (.setLastModified (io/file dir path) millis))

(deftest snapshot-maps-files-and-skips-git-and-cache
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "src/a.clj" "(ns a)")
      (write! dir ".git/HEAD" "ref")
      (write! dir ".mutate-cache/a.edn" "{}")
      (let [snapshot (guard/snapshot dir)]
        (is (= ["src/a.clj"] (keys snapshot)))
        (is (= 6 (first (get snapshot "src/a.clj")))
            "size comes first")))))

(deftest changes-name-added-changed-and-deleted-files
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "kept.txt" "same")
      (write! dir "edited.txt" "old")
      (write! dir "gone.txt" "x")
      (set-time! dir "edited.txt" 1000000)
      (let [before (guard/snapshot dir)]
        (write! dir "edited.txt" "new")
        (set-time! dir "edited.txt" 2000000)
        (io/delete-file (io/file dir "gone.txt"))
        (write! dir "new.txt" "x")
        (is (= [["edited.txt" :changed] ["gone.txt" :deleted] ["new.txt" :added]]
               (guard/changes dir before (guard/snapshot dir) {})))))))

(deftest changes-compare-bytes-of-file-baseline-wrote
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "log.txt" "up")
      (set-time! dir "log.txt" 1000000)
      (let [before (guard/snapshot dir)
            hashes (:hashes (guard/start dir {} before #{}))]
        (set-time! dir "log.txt" 2000000)
        (is (empty? (guard/changes dir before (guard/snapshot dir) hashes))
            "same bytes written again")
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir) {}))
            "no hash, so new time alone is change")
        (write! dir "log.txt" "dn")
        (set-time! dir "log.txt" 3000000)
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir) hashes))
            "same size, other bytes")))))

(deftest start-looks-after-every-mutant-when-baseline-left-file-or-recorded-no-writers
  (fixtures/with-temp-dir
    (fn [dir]
      (let [before (guard/snapshot dir)
            _ (write! dir "left.txt" "x")
            after (guard/snapshot dir)]
        (is (false? (:every-mutant? (guard/start dir before before #{}))))
        (is (true? (:every-mutant? (guard/start dir before before nil)))
            "plain baseline recorded no writers")
        (is (true? (:every-mutant? (guard/start dir before after #{}))))
        (is (= ["left.txt"] (keys (:hashes (guard/start dir before after #{})))))
        (is (empty? (:hashes (guard/start dir after before #{})))
            "deleted file has no bytes to hash")))))

(deftest check-looks-only-after-mutants-whose-tests-could-write
  (let [guard {:every-mutant? false
               :writers #{:writer}}]
    (is (true? (guard/check? guard {:kind :subset
                                    :tests [:other :writer]})))
    (is (false? (guard/check? guard {:kind :subset
                                     :tests [:other]})))
    (is (true? (guard/check? guard {:kind :every}))
        "every test runs, writer too")
    (is (false? (guard/check? (assoc guard :writers #{}) {:kind :every})))
    (is (true? (guard/check? (assoc guard :every-mutant? true) {:kind :subset
                                                                :tests [:other]})))))

(deftest changed-files-compares-with-snapshot-after-baseline
  (fixtures/with-temp-dir
    (fn [dir]
      (let [file-guard (guard/start dir {} (guard/snapshot dir) #{})]
        (is (nil? (guard/changed-files file-guard)))
        (write! dir "new.txt" "x")
        (is (= [["new.txt" :added]] (guard/changed-files file-guard)))))))

(deftest entries-name-what-is-directly-in-root
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "a.txt" "x")
      (write! dir "sub/b.txt" "x")
      (is (= #{"a.txt" "sub"} (guard/entries dir))
          "names in subfolder are not read"))))

(deftest recording-gives-time-of-each-file-write
  (fixtures/with-temp-dir
    (fn [dir]
      (let [recording (guard/start-recording)
            started (java.time.Instant/now)
            _ (spit (io/file dir "a.txt") "x")
            ended (java.time.Instant/now)
            writes (guard/stop-recording recording)]
        (is (= #{#'tested} (guard/writers writes [[#'tested started ended false]])))
        (is (= 1 (count writes)))))))

(deftest writers-are-tests-with-write-in-their-time-or-changed-entries
  (let [at #(java.time.Instant/ofEpochSecond %)
        writes [(at 10) (at 30)]]
    (is (= #{:during :at-start :at-end :entries}
           (guard/writers writes [[:during (at 5) (at 15) false]
                                  [:at-start (at 30) (at 31) false]
                                  [:at-end (at 25) (at 30) false]
                                  [:before (at 0) (at 9) false]
                                  [:after (at 11) (at 29) false]
                                  [:entries (at 40) (at 50) true]])))))

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
