(ns cljs-patrol.fs-test
  (:require
   [cljs-patrol.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private extensions #{".cljs" ".cljc" ".clj"})

(defn- scratch-tree []
  (let [root (fs/tmp-file-path "fs-test-" "")]
    (doseq [dir ["src" "target/classes/app" "node_modules/thing" ".hidden" "out"]]
      (fs/mkdirs (fs/join-path root dir)))
    (doseq [leaf ["src/kept.clj" "src/kept.cljs" "src/skipped.txt"
                  "target/classes/app/stale.clj" "node_modules/thing/vendored.cljs"
                  ".hidden/secret.clj" "out/compiled.cljs"]]
      (spit (fs/join-path root leaf) "(ns x)"))
    root))

(deftest list-source-files-test
  (let [root (scratch-tree)
        found (set (map #(last (str/split % #"[/\\\\]")) (fs/list-source-files root extensions)))]
    (try
      (testing "every source file outside build output is returned"
        (is (= #{"kept.clj" "kept.cljs"} found)))

      (testing "a build directory is not descended into"
        (is (not (contains? found "stale.clj"))
            "target/classes holds a copy of src that goes on being reported after the original is fixed")
        (is (not (contains? found "vendored.cljs")))
        (is (not (contains? found "compiled.cljs"))))

      (testing "a dot-directory is skipped whatever it is called"
        (is (not (contains? found "secret.clj"))))

      (testing "an extension nothing asked for is left alone"
        (is (not (contains? found "skipped.txt"))))
      (finally (fs/delete-tree! root)))))

(deftest missing-dirs-test
  (let [root (scratch-tree)]
    (try
      (testing "a path that exists is not reported"
        (is (empty? (fs/missing-dirs [root (fs/join-path root "src")]))))

      (testing "every path that does not exist is reported, not just the first"
        (is (= ["nope" "also-nope"]
               (fs/missing-dirs ["nope" root "also-nope"]))))
      (finally (fs/delete-tree! root)))))
