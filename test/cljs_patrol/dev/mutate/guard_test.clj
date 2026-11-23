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

(defn- set-time! [dir path millis] (.setLastModified (io/file dir path) millis))

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

(deftest editor-file-names-swap-lock-and-backup-files
  (is (every? guard/editor-file? ["core.clj.swp" "core.clj.swo" ".#core.clj" "core.clj~" "4913"]))
  (is (not-any? guard/editor-file? ["core.clj" "swp" "4914" "a.#b" "~a"])))

(deftest snapshot-maps-files-and-skips-what-skipped-dirs-name
  (fixtures/with-temp-dir
    (fn [dir]
      (doseq [path ["src/a.clj" ".git/HEAD" "runs/a.edn" ".lsp/db" ".clj-kondo/.cache/x" ".clj-kondo/config.edn"
                    "sub/.git/HEAD" "src/.a.clj.swp" "src/4913"]]
        (write! dir path "(ns a)"))
      (let [snapshot (guard/snapshot dir (guard/skipped-dirs dir (io/file dir "runs")))]
        (is (= #{"src/a.clj" ".clj-kondo/config.edn" "sub/.git/HEAD"} (set (keys snapshot)))
            "only root .git is skipped, only cache of .clj-kondo, and editor files")
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
            hashes (:hashes (guard/start dir #{} {} before))]
        (set-time! dir "log.txt" 2000000)
        (is (empty? (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same bytes written again")
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) {}))
            "no hash, so new time alone is change")
        (write! dir "log.txt" "dn")
        (set-time! dir "log.txt" 3000000)
        (is (= [["log.txt" :changed]] (guard/changes dir before (guard/snapshot dir #{}) hashes))
            "same size, other bytes")))))

(deftest start-hashes-files-baseline-added-or-changed
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "kept.txt" "x")
      (write! dir "edited.txt" "x")
      (write! dir "gone.txt" "x")
      (set-time! dir "edited.txt" 1000000)
      (let [before (guard/snapshot dir #{})
            _ (write! dir "left.txt" "x")
            _ (write! dir "edited.txt" "y")
            _ (io/delete-file (io/file dir "gone.txt"))
            after (guard/snapshot dir #{})
            file-guard (guard/start dir #{".git"} before after)]
        (is (= #{"left.txt" "edited.txt"} (-> file-guard :hashes keys set))
            "deleted file has no bytes to hash")
        (is (= after (:files file-guard)))
        (is (= #{".git"} (:skipped file-guard)))))))

(deftest look-gives-changes-since-baseline
  (fixtures/with-temp-dir
    (fn [dir]
      (write! dir "kept.txt" "x")
      (let [snapshot (guard/snapshot dir #{})
            file-guard (guard/start dir #{"skipped"} snapshot snapshot)]
        (is (nil? (guard/look file-guard)))
        (write! dir "skipped/a.txt" "x")
        (is (nil? (guard/look file-guard))
            "skipped folder stays out")
        (io/delete-file (io/file dir "kept.txt"))
        (write! dir "new.txt" "x")
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
