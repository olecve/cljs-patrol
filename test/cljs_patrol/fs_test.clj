(ns cljs-patrol.fs-test
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import
   [com.google.common.jimfs Configuration Jimfs]
   [java.io File IOException StringWriter]
   [java.nio.file
    AccessDeniedException
    DirectoryIteratorException
    FileSystem
    FileSystemException
    FileSystems
    FileVisitor
    Files
    NoSuchFileException
    OpenOption
    Path]
   [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(def ^:private extensions #{".cljs" ".cljc" ".clj"})

(defn- scratch-tree []
  (let [root (temp-dir/create-dir "fs-test-")]
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

      (testing "the empty path is reported, not read as the working directory"
        (is (= [""] (fs/missing-dirs [""]))))
      (finally (fs/delete-tree! root)))))

(defn- java-io-source-files
  "Discovery as it was with `java.io.File`, to compare the text and the order of the paths it gives."
  [root-dir extensions]
  (letfn [(skipped? [^File f]
            (let [name (.getName f)]
              (and (.isDirectory f)
                   (or (contains? fs/skipped-dirs name)
                       (and (str/starts-with? name ".") (not (#{"." ".."} name)))))))
          (walk [^File f]
            (cond
              (skipped? f) []
              (.isDirectory f) (mapcat walk (.listFiles f))
              :else [f]))]
    (->> (walk (File. ^String root-dir))
         (filter (fn [^File f] (.isFile f)))
         (map (fn [^File f] (.getPath f)))
         (filterv (fn [^String p] (some #(str/ends-with? p %) extensions))))))

(deftest list-source-files-keeps-java-io-text-and-order
  (doseq [root ["src" "src/" "./src" "test-projects/a11y-app/src"]]
    (let [expected (java-io-source-files root extensions)]
      (is (seq expected)
          (str root " holds source files, otherwise the compare proves nothing"))
      (is (= expected (fs/list-source-files root extensions))
          (str root " gives the same paths in the same order")))))

(defn- unix-fs ^FileSystem [] (Jimfs/newFileSystem (Configuration/unix)))

(defn- write! [^Path p text]
  (fs/create-dirs! (fs/parent p))
  (Files/writeString p text (make-array OpenOption 0)))

(defn- read-text [^Path p]
  (Files/readString p))

(deftest path-test
  (with-open [jimfs (unix-fs)]
    (let [p (fs/path jimfs "/work" "src" "a.cljs")]
      (is (= "/work/src/a.cljs" (str p)))
      (is (identical? jimfs (.getFileSystem p))
          "path belongs to given file system"))))

(deftest resolve-path-test
  (with-open [jimfs (unix-fs)]
    (is (= "/work/src/a.cljs" (str (fs/resolve-path (fs/path jimfs "/work/src") "a.cljs"))))))

(deftest absolute-test
  (with-open [jimfs (unix-fs)]
    (is (= "/work/src" (str (fs/absolute (fs/path jimfs "src"))))
        "relative path resolves against working directory of its file system")
    (is (fs/absolute? (fs/path jimfs "/work/src")))
    (is (not (fs/absolute? (fs/path jimfs "src"))))))

(deftest parent-test
  (with-open [jimfs (unix-fs)]
    (is (= "/work/src" (str (fs/parent (fs/path jimfs "/work/src/a.cljs")))))
    (is (nil? (fs/parent (fs/path jimfs "a.cljs"))))))

(deftest exists-test
  (with-open [jimfs (unix-fs)]
    (let [p (fs/path jimfs "/work/a.cljs")]
      (is (not (fs/exists? p)))
      (write! p "(ns a)")
      (is (fs/exists? p)))))

(deftest create-dirs-test
  (with-open [jimfs (unix-fs)]
    (let [dir (fs/path jimfs "/work/a/b/c")]
      (fs/create-dirs! dir)
      (is (Files/isDirectory dir (make-array java.nio.file.LinkOption 0)))
      (fs/create-dirs! dir)
      (is (fs/exists? dir)
          "second call on existing folder does nothing")
      (write! (fs/path jimfs "/work/file") "x")
      (is (thrown? FileSystemException (fs/create-dirs! (fs/path jimfs "/work/file/sub")))
          "folder under regular file throws with reason"))))

(deftest relative-path-test
  (with-open [jimfs (unix-fs)]
    (let [base (fs/path jimfs "/work/src")]
      (is (= "x/a.cljs" (str (fs/relative-path base (fs/path jimfs "/work/src/x/a.cljs")))))
      (is (= "x/a.cljs" (str (fs/relative-path base (fs/path jimfs "src/x/a.cljs"))))
          "relative target is compared from working directory")
      (let [outside (fs/path jimfs "/other/a.cljs")]
        (is (identical? outside (fs/relative-path base outside))
            "target outside base comes back unchanged")))))

(deftest delete-if-exists-test
  (with-open [jimfs (unix-fs)]
    (let [p (fs/path jimfs "/work/a.cljs")]
      (write! p "x")
      (is (true? (fs/delete-if-exists! p)))
      (is (not (fs/exists? p)))
      (is (false? (fs/delete-if-exists! p))
          "missing file gives false"))))

(deftest move-over-test
  (with-open [jimfs (unix-fs)
              other (unix-fs)]
    (let [source (fs/path jimfs "/work/new")
          target (fs/path jimfs "/work/old")]
      (write! source "new")
      (write! target "old")
      (fs/move-over! source target)
      (is (= "new" (read-text target)))
      (is (not (fs/exists? source))))
    (testing "file system that cannot move atomically gets plain replace"
      (let [source (fs/path jimfs "/work/a")
            target (fs/path other "/work/b")]
        (write! source "a")
        (write! target "b")
        (fs/move-over! source target)
        (is (= "a" (read-text target)))
        (is (not (fs/exists? source)))))))

(deftest delete-recursively-test
  (with-open [jimfs (unix-fs)]
    (let [root (fs/path jimfs "/work/tree")
          outside (fs/path jimfs "/work/outside/kept.txt")]
      (write! (fs/path jimfs "/work/tree/a/b.txt") "x")
      (write! (fs/path jimfs "/work/tree/c.txt") "x")
      (write! outside "x")
      (Files/createSymbolicLink (fs/path jimfs "/work/tree/link")
                                (fs/parent outside)
                                (make-array FileAttribute 0))
      (is (nil? (fs/delete-recursively! root)))
      (is (not (fs/exists? root)))
      (is (fs/exists? outside)
          "folder behind link keeps its files")
      (is (nil? (fs/delete-recursively! root))
          "missing path does nothing"))))

(deftest source-paths-test
  (with-open [jimfs (unix-fs)]
    (doseq [leaf ["src/a.cljs" "src/x/b.cljc" "src/c.txt" "src/target/d.cljs" "src/.cache/e.cljs"
                  "elsewhere/f.cljs"]]
      (write! (fs/path jimfs "/work" leaf) "(ns x)"))
    (Files/createSymbolicLink (fs/path jimfs "/work/src/linked")
                              (fs/path jimfs "/work/elsewhere")
                              (make-array FileAttribute 0))
    (let [found (fs/source-paths (fs/path jimfs "/work/src") extensions)]
      (is (every? #(instance? Path %) found))
      (is (= #{"/work/src/a.cljs" "/work/src/x/b.cljc" "/work/src/linked/f.cljs"}
             (set (map str found)))
          "walk follows link to folder and skips build and dot folders"))))

(deftest missing-paths-test
  (with-open [jimfs (unix-fs)]
    (let [here (fs/path jimfs "/work")
          gone (fs/path jimfs "/gone")]
      (is (= [gone] (fs/missing-paths [here gone]))))))

(defn- set-permissions! [^Path p ^String text]
  (Files/setPosixFilePermissions p (PosixFilePermissions/fromString text)))

(defn- with-locked-folder
  "Make `root/src/locked` with no permissions, call `f` with `root` and the locked folder, and unlock it after.

  When the JVM can still read the locked folder, for example as root, `f` does not run. Then the test prints a SKIP line
  with `test-name` and makes no assertion, so it does not count as a pass."
  [test-name f]
  (let [root (temp-dir/create-dir "fs-locked-")
        locked (fs/path (FileSystems/getDefault) root "src" "locked")]
    (try
      (write! (fs/path (FileSystems/getDefault) root "src" "ok.cljs") "(ns ok)")
      (write! (fs/resolve-path locked "hidden.cljs") "(ns hidden)")
      (set-permissions! locked "---------")
      (if (Files/isReadable locked)
        (println (str "SKIP " test-name ": the JVM can read a folder with no permissions, so this test does not run"))
        (f root locked))
      (finally
        (try
          (set-permissions! locked "rwx------")
          (finally
            (temp-dir/delete-dir! root)))))))

(deftest source-paths-skips-unreadable-folder-with-one-warning
  (with-locked-folder
    "source-paths-skips-unreadable-folder-with-one-warning"
    (fn [root locked]
      (let [err (StringWriter.)
            found (binding [*err* err]
                    (fs/source-paths (fs/path (FileSystems/getDefault) root "src") extensions))
            lines (str/split-lines (str err))]
        (is (= [(str root "/src/ok.cljs")] (mapv str found))
            "other files are still found")
        (is (= 1 (count lines))
            (str "one warning, got:\n" err))
        (is (str/includes? (first lines) (str locked))
            "warning names folder")
        (is (str/ends-with? (first lines) "skipped it: AccessDeniedException")
            "warning names reason, and adds no detail when exception has none")))))

(deftest delete-recursively-throws-on-unreadable-folder
  (with-locked-folder
    "delete-recursively-throws-on-unreadable-folder"
    (fn [root _]
      (is (thrown? AccessDeniedException
                   (fs/delete-recursively! (fs/path (FileSystems/getDefault) root)))))))

(deftest path-functions-throw-on-nil
  (with-open [jimfs (unix-fs)]
    (let [p (fs/path jimfs "/work/a")
          q (fs/path jimfs "/work/b")]
      (write! p "x")
      (doseq [[label f] [["path without file system" #(fs/path nil "src")]
                         ["path without first part" #(fs/path jimfs nil)]
                         ["resolve-path without base" #(fs/resolve-path nil "a")]
                         ["resolve-path without leaf" #(fs/resolve-path p nil)]
                         ["absolute" #(fs/absolute nil)]
                         ["absolute?" #(fs/absolute? nil)]
                         ["parent" #(fs/parent nil)]
                         ["exists?" #(fs/exists? nil)]
                         ["create-dirs!" #(fs/create-dirs! nil)]
                         ["relative-path without base" #(fs/relative-path nil p)]
                         ["relative-path without target" #(fs/relative-path p nil)]
                         ["delete-if-exists!" #(fs/delete-if-exists! nil)]
                         ["move-over! without source" #(fs/move-over! nil q)]
                         ["move-over! without target" #(fs/move-over! p nil)]
                         ["delete-recursively!" #(fs/delete-recursively! nil)]
                         ["source-paths" #(fs/source-paths nil extensions)]
                         ["missing-paths with nil path" #(fs/missing-paths [nil])]]]
        (is (thrown? NullPointerException (f))
            label)))))

(defn- warnings-for-failing-folder
  "Run `source-paths` on a Jimfs tree where reading `/work/src/bad` throws `exception`, and give the paths and stderr."
  [exception]
  (with-open [jimfs (unix-fs)]
    (write! (fs/path jimfs "/work/src/ok.cljs") "(ns ok)")
    (write! (fs/path jimfs "/work/src/bad/hidden.cljs") "(ns hidden)")
    (let [err (StringWriter.)
          found (with-redefs [fs/directory-entries (fn [^Path dir]
                                                     (if (= "/work/src/bad" (str dir))
                                                       (throw exception)
                                                       (with-open [stream (Files/newDirectoryStream dir)]
                                                         (vec stream))))]
                  (binding [*err* err]
                    (mapv str (fs/source-paths (fs/path jimfs "/work/src") extensions))))]
      [found (str/split-lines (str err))])))

(deftest source-paths-warns-with-reason-of-file-system-exception
  (let [[found lines] (warnings-for-failing-folder (FileSystemException. "/work/src/bad" nil "Too many open files"))]
    (is (= ["/work/src/ok.cljs"] found))
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: FileSystemException (Too many open files)"] lines))))

(deftest source-paths-skips-folder-whose-listing-fails-midway
  (let [[found lines] (warnings-for-failing-folder (DirectoryIteratorException. (IOException. "device gone")))]
    (is (= ["/work/src/ok.cljs"] found)
        "folder is skipped and walk goes on")
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: IOException (device gone)"] lines)
        "one warning with cause and its message")))

(deftest delete-recursively-throws-when-folder-listing-failed
  (with-open [jimfs (unix-fs)]
    (let [folder (fs/path jimfs "/work/tree")
          ^FileVisitor visitor (#'fs/deleting-visitor)]
      (fs/create-dirs! folder)
      (is (thrown-with-msg? IOException #"listing failed"
                            (.postVisitDirectory visitor folder (IOException. "listing failed"))))
      (is (fs/exists? folder)
          "folder stays when its listing failed"))))

(def ^:private nul-path
  "A path with a NUL character, which no file system accepts."
  "a\u0000b")

(deftest string-adapters-test
  (let [root (temp-dir/create-dir "fs-adapters-")]
    (try
      (testing "absolute-path"
        (is (= (str (System/getProperty "user.dir") "/./src") (fs/absolute-path "./src/"))
            "text as java.io.File gave it"))

      (testing "absolute-path?"
        (is (fs/absolute-path? root))
        (is (not (fs/absolute-path? "src"))))

      (testing "parent-dir"
        (is (= "a/b" (fs/parent-dir "a/b/c.edn")))
        (is (nil? (fs/parent-dir "c.edn"))))

      (testing "join-path"
        (is (= "src/.cljs-patrol/baseline.edn" (fs/join-path "src/" ".cljs-patrol/baseline.edn")))
        (is (= "src/abs" (fs/join-path "src" "/abs"))
            "absolute leaf goes under base, as in java.io")
        (is (thrown? NullPointerException (fs/join-path nil "src"))
            "nil base throws, not path without root"))

      (testing "file-exists? and mkdirs"
        (let [dir (str root "/a/b")]
          (is (not (fs/file-exists? dir)))
          (is (true? (fs/mkdirs dir))
              "mkdirs gives true when it made folder")
          (is (fs/file-exists? dir))
          (is (false? (fs/mkdirs dir))
              "second mkdirs on existing folder gives false")
          (is (fs/file-exists? dir))))

      (testing "relativize"
        (is (= "x/a.cljs" (fs/relativize "src" "src/x/a.cljs")))
        (is (= "other//a.cljs" (fs/relativize "src" "other//a.cljs"))
            "target outside base keeps its text"))

      (testing "move-replace! and delete-file!"
        (let [source (str root "/new")
              target (str root "/old")]
          (spit source "new")
          (spit target "old")
          (fs/move-replace! source target)
          (is (= "new" (slurp target)))
          (is (true? (fs/delete-file! target)))
          (is (false? (fs/delete-file! target))
              "missing file gives false")))

      (testing "delete-file! that cannot delete"
        (let [folder (str root "/full")]
          (fs/mkdirs folder)
          (spit (str folder "/a.txt") "x")
          (is (false? (fs/delete-file! folder))
              "folder that is not empty gives false and does not throw")
          (is (fs/file-exists? folder))))

      (testing "delete-tree!"
        (let [tree (str root "/tree")]
          (fs/mkdirs (str tree "/x"))
          (spit (str tree "/x/a.txt") "x")
          (is (nil? (fs/delete-tree! tree)))
          (is (not (fs/file-exists? tree)))))

      (testing "list-source-files gives a vector of strings"
        (is (vector? (fs/list-source-files "test-projects/a11y-app/src" extensions)))
        (is (every? string? (fs/list-source-files "test-projects/a11y-app/src" extensions))))
      (finally (temp-dir/delete-dir! root)))))

(deftest string-adapters-read-empty-path-as-no-file
  (let [calls (atom [])
        record (fn [& args] (swap! calls conj args) nil)]
    (with-redefs [fs/delete-recursively! record
                  fs/delete-if-exists! record
                  fs/create-dirs! record
                  fs/move-over! record]
      (is (= "" (fs/absolute-path "")))
      (is (false? (fs/absolute-path? "")))
      (is (nil? (fs/parent-dir "")))
      (is (false? (fs/file-exists? "")))
      (is (= [""] (fs/missing-dirs [""])))
      (is (false? (fs/mkdirs "")))
      (is (= "/x" (fs/join-path "" "x"))
          "empty base stands for root, as in java.io")
      (is (= "src/a.cljs" (fs/relativize "" "src/a.cljs")))
      (is (= "" (fs/relativize "src" "")))
      (is (false? (fs/delete-file! "")))
      (is (nil? (fs/delete-tree! "")))
      (is (thrown? NoSuchFileException (fs/move-replace! "" "x")))
      (is (thrown? NoSuchFileException (fs/move-replace! "x" "")))
      (is (= [] (fs/list-source-files "" extensions))))
    (is (empty? @calls)
        "no file operation runs on working directory")))

(deftest string-adapters-read-invalid-path-as-no-file
  (is (false? (fs/file-exists? nul-path)))
  (is (= [nul-path] (fs/missing-dirs [nul-path])))
  (is (= nul-path (fs/absolute-path nul-path))
      "absolute-path gives text back unchanged"))
