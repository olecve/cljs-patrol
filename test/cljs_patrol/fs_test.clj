(ns cljs-patrol.fs-test
  (:require
   [cljs-patrol.disk :as disk]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.memory-fs :as memory-fs]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.java.io :as io]
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

(deftest missing-paths-reports-every-missing-path-in-order
  (with-open [file-system (memory-fs/unix)]
    (let [root (fs/path file-system "/work")
          src (fs/path file-system "/work/src")
          nope (fs/path file-system "nope")
          also-nope (fs/path file-system "/also-nope")]
      (fs/create-dirs! src)
      (testing "a path that exists is not reported"
        (is (empty? (fs/missing-paths [root src]))))

      (testing "every path that does not exist is reported, not just the first"
        (is (= [nope also-nope]
               (fs/missing-paths [nope root also-nope])))))))

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

(deftest path-test
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work" "src" "a.cljs")]
      (is (= "/work/src/a.cljs" (str p)))
      (is (identical? jimfs (.getFileSystem p))
          "path belongs to given file system"))))

(deftest resolve-path-test
  (with-open [jimfs (memory-fs/unix)]
    (is (= "/work/src/a.cljs" (str (fs/resolve-path (fs/path jimfs "/work/src") "a.cljs"))))
    (is (= "/abs" (str (fs/resolve-path (fs/path jimfs "/work/src") "/abs")))
        "absolute leaf replaces base, as in NIO")))

(deftest parse-path-test
  (with-open [jimfs (memory-fs/unix)]
    (is (= (fs/path jimfs "src/a.cljs") (fs/parse-path jimfs "src//a.cljs/"))
        "text makes path on given file system")
    (is (nil? (fs/parse-path jimfs ""))
        "empty string names no file")
    (is (nil? (fs/parse-path jimfs "a\u0000b"))
        "text that file system does not accept names no file")))

(deftest absolute-text-test
  (with-open [jimfs (memory-fs/unix)]
    (is (= "/work/src/a.cljs" (fs/absolute-text jimfs "src//a.cljs")))
    (is (= "/abs/a.cljs" (fs/absolute-text jimfs "/abs/a.cljs")))))

(deftest read-text-reads-bytes-that-are-not-utf-8-as-replacement-character
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work/a.cljs")]
      (fs/create-dirs! (fs/parent p))
      (Files/write p (byte-array [0x61 0xC3 0x28 0x62]) (make-array OpenOption 0))
      (is (= "a\uFFFD(b" (fs/read-text p))))))

(deftest text-writer-writes-lone-surrogate-as-question-mark
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work/a.txt")
          text "a\u00e9\uD83D"]
      (fs/create-dirs! (fs/parent p))
      (memory-fs/write! p "old text that is longer")
      (with-open [w (fs/text-writer p)]
        (.write w text))
      (is (= (seq (let [out (java.io.ByteArrayOutputStream.)]
                    (with-open [w (io/writer out)]
                      (.write w text))
                    (.toByteArray out)))
             (seq (Files/readAllBytes p)))
          "same bytes as clojure.java.io/writer, and old content replaced"))))

(defn- nfd ^String [^String text]
  (java.text.Normalizer/normalize text java.text.Normalizer$Form/NFD))

(deftest source-paths-name-files-as-java-io-listed-them
  (let [root (temp-dir/create-dir "fs-nfd-")
        dir (fs/path (FileSystems/getDefault) root (nfd "\u00e9"))]
    (try
      (disk/write! (fs/resolve-path dir (nfd "\u00e9.cljs")) "(ns e)")
      (let [found (fs/source-paths (fs/path (FileSystems/getDefault) root) extensions)]
        (is (= (java-io-source-files root extensions) (mapv str found))
            "same text as File.listFiles, which is NFC on macOS")
        (is (= ["(ns e)"] (map fs/read-text found))
            "path names real file"))
      (finally
        (temp-dir/delete-dir! root)))))

(deftest source-paths-keep-name-that-nfc-does-not-reach
  (with-open [jimfs (memory-fs/unix)]
    (let [nfd-file (fs/path jimfs "/work/src" (nfd "\u00e9.cljs"))
          nfc-file (fs/path jimfs "/work/src/\u00e9.cljs")
          only-nfd (fs/path jimfs "/work/other" (nfd "\u00e9.cljs"))]
      (memory-fs/write! nfd-file "(ns nfd)")
      (memory-fs/write! nfc-file "(ns nfc)")
      (memory-fs/write! only-nfd "(ns only)")
      (is (= #{"(ns nfd)" "(ns nfc)"} (set (map fs/read-text (fs/source-paths (fs/path jimfs "/work/src") extensions))))
          "NFD and NFC names of two files stay two files")
      (is (= [only-nfd] (fs/source-paths (fs/path jimfs "/work/other") extensions))
          "NFD name stays when NFC name names no file"))))

(deftest path-throws-on-empty-path
  (with-open [jimfs (memory-fs/unix)]
    (is (thrown? IllegalArgumentException (fs/path jimfs "")))
    (is (thrown? IllegalArgumentException (fs/path jimfs "" "")))))

(defn- empty-path ^Path [^FileSystem file-system]
  (.getPath file-system "" (make-array String 0)))

(deftest absolute-test
  (with-open [jimfs (memory-fs/unix)]
    (is (= "/work/src" (str (fs/absolute (fs/path jimfs "src"))))
        "relative path resolves against working directory of its file system")
    (is (fs/absolute? (fs/path jimfs "/work/src")))
    (is (not (fs/absolute? (fs/path jimfs "src"))))))

(deftest parent-test
  (with-open [jimfs (memory-fs/unix)]
    (is (= "/work/src" (str (fs/parent (fs/path jimfs "/work/src/a.cljs")))))
    (is (nil? (fs/parent (fs/path jimfs "a.cljs"))))))

(deftest exists-test
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work/a.cljs")]
      (is (not (fs/exists? p)))
      (memory-fs/write! p "(ns a)")
      (is (fs/exists? p)))))

(deftest create-dirs-test
  (with-open [jimfs (memory-fs/unix)]
    (let [dir (fs/path jimfs "/work/a/b/c")]
      (fs/create-dirs! dir)
      (is (Files/isDirectory dir (make-array java.nio.file.LinkOption 0)))
      (fs/create-dirs! dir)
      (is (fs/exists? dir)
          "second call on existing folder does nothing")
      (memory-fs/write! (fs/path jimfs "/work/file") "x")
      (is (thrown? FileSystemException (fs/create-dirs! (fs/path jimfs "/work/file/sub")))
          "folder under regular file throws with reason"))))

(deftest relative-path-test
  (with-open [jimfs (memory-fs/unix)]
    (let [base (fs/path jimfs "/work/src")]
      (is (= "x/a.cljs" (str (fs/relative-path base (fs/path jimfs "/work/src/x/a.cljs")))))
      (is (= "x/a.cljs" (str (fs/relative-path base (fs/path jimfs "src/x/a.cljs"))))
          "relative target is compared from working directory")
      (is (= "../../other/a.cljs" (str (fs/relative-path base (fs/path jimfs "/other/a.cljs"))))
          "target outside base gives path up from base")
      (is (thrown-with-msg? IllegalArgumentException #"target is the base" (fs/relative-path base base))
          "target that is base throws, since result is empty path"))))

(deftest delete-if-exists-test
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work/a.cljs")]
      (memory-fs/write! p "x")
      (is (true? (fs/delete-if-exists! p)))
      (is (not (fs/exists? p)))
      (is (false? (fs/delete-if-exists! p))
          "missing file gives false"))))

(deftest move-over-test
  (with-open [jimfs (memory-fs/unix)
              other (memory-fs/unix)]
    (let [source (fs/path jimfs "/work/new")
          target (fs/path jimfs "/work/old")]
      (memory-fs/write! source "new")
      (memory-fs/write! target "old")
      (fs/move-over! source target)
      (is (= "new" (memory-fs/read-text target)))
      (is (not (fs/exists? source))))
    (testing "file system that cannot move atomically gets plain replace"
      (let [source (fs/path jimfs "/work/a")
            target (fs/path other "/work/b")]
        (memory-fs/write! source "a")
        (memory-fs/write! target "b")
        (fs/move-over! source target)
        (is (= "a" (memory-fs/read-text target)))
        (is (not (fs/exists? source)))))))

(deftest delete-recursively-test
  (with-open [jimfs (memory-fs/unix)]
    (let [root (fs/path jimfs "/work/tree")
          outside (fs/path jimfs "/work/outside/kept.txt")]
      (memory-fs/write! (fs/path jimfs "/work/tree/a/b.txt") "x")
      (memory-fs/write! (fs/path jimfs "/work/tree/c.txt") "x")
      (memory-fs/write! outside "x")
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
  (with-open [jimfs (memory-fs/unix)]
    (doseq [leaf ["src/a.cljs" "src/x/b.cljc" "src/c.txt" "src/target/classes/d.cljs" "src/node_modules/thing/e.cljs"
                  "src/out/f.cljs" "src/.cache/g.cljs" "elsewhere/h.cljs"]]
      (memory-fs/write! (fs/path jimfs "/work" leaf) "(ns x)"))
    (Files/createSymbolicLink (fs/path jimfs "/work/src/linked")
                              (fs/path jimfs "/work/elsewhere")
                              (make-array FileAttribute 0))
    (let [found (fs/source-paths (fs/path jimfs "/work/src") extensions)]
      (is (every? #(instance? Path %) found))
      (is (= #{"/work/src/a.cljs" "/work/src/x/b.cljc" "/work/src/linked/h.cljs"}
             (set (map str found)))
          "walk follows link to folder, skips target, node_modules, out and dot folders, and leaves other extensions"))))

(defn- set-permissions! [^Path p ^String text]
  (Files/setPosixFilePermissions p (PosixFilePermissions/fromString text)))

(defn- with-locked-folder
  "Make `root/src/locked` with no permissions, call `f` with `root` and the locked folder, and unlock it after.

  When the JVM can still read the locked folder, for example as root, `f` does not run. Then the test prints a SKIP line
  with `test-name` and makes no assertion. The runner counts that test as a pass, so the SKIP line is the only signal
  that it did not run."
  [test-name f]
  (let [root (temp-dir/create-dir "fs-locked-")
        locked (fs/path (FileSystems/getDefault) root "src" "locked")]
    (try
      (disk/write! (fs/path (FileSystems/getDefault) root "src" "ok.cljs") "(ns ok)")
      (disk/write! (fs/resolve-path locked "hidden.cljs") "(ns hidden)")
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

(deftest path-functions-throw-on-nil-and-empty-path
  (with-open [jimfs (memory-fs/unix)]
    (let [p (fs/path jimfs "/work/a")
          q (fs/path jimfs "/work/b")]
      (memory-fs/write! p "x")
      (is (thrown? NullPointerException (fs/path nil "src")))
      (is (thrown? NullPointerException (fs/path jimfs nil)))
      (is (thrown? NullPointerException (fs/resolve-path p nil)))
      (doseq [[label f] [["resolve-path" #(fs/resolve-path % "a")]
                         ["absolute" fs/absolute]
                         ["absolute?" fs/absolute?]
                         ["parent" fs/parent]
                         ["exists?" fs/exists?]
                         ["create-dirs!" fs/create-dirs!]
                         ["relative-path base" #(fs/relative-path % p)]
                         ["relative-path target" #(fs/relative-path p %)]
                         ["delete-if-exists!" fs/delete-if-exists!]
                         ["move-over! source" #(fs/move-over! % q)]
                         ["move-over! target" #(fs/move-over! p %)]
                         ["delete-recursively!" fs/delete-recursively!]
                         ["read-text" fs/read-text]
                         ["text-writer" fs/text-writer]
                         ["source-paths" #(fs/source-paths % extensions)]
                         ["missing-paths" #(fs/missing-paths [%])]
                         ["file-system-of" fs/file-system-of]]]
        (is (thrown? NullPointerException (f nil))
            (str label " on nil"))
        (is (thrown? IllegalArgumentException (f (empty-path jimfs)))
            (str label " on empty path")))
      (is (fs/exists? p)
          "file in working directory stays after failed calls, delete-recursively! on empty path included"))))

(deftest relative-path-throws-on-different-roots
  (with-open [windows (Jimfs/newFileSystem (-> (Configuration/windows)
                                               .toBuilder
                                               (.setRoots "C:\\" (into-array String ["D:\\"]))
                                               .build))]
    (is (thrown? IllegalArgumentException
                 (fs/relative-path (fs/path windows "C:\\work") (fs/path windows "D:\\x"))))))

(defn- warnings-for-failing-folder
  "Run `source-paths` on a Jimfs tree where reading `/work/src/bad` throws `exception`, and give the paths and stderr."
  [exception]
  (with-open [jimfs (memory-fs/unix)]
    (memory-fs/write! (fs/path jimfs "/work/src/ok.cljs") "(ns ok)")
    (memory-fs/write! (fs/path jimfs "/work/src/bad/hidden.cljs") "(ns hidden)")
    (let [err (StringWriter.)
          original (var-get #'fs/directory-entries)
          found (with-redefs [fs/directory-entries (fn [dir]
                                                     (if (= "/work/src/bad" (str dir))
                                                       (throw exception)
                                                       (original dir)))]
                  (binding [*err* err]
                    (mapv str (fs/source-paths (fs/path jimfs "/work/src") extensions))))]
      [found (str/split-lines (str err))])))

(deftest source-paths-warns-with-reason-of-file-system-exception
  (let [[found lines] (warnings-for-failing-folder (FileSystemException. "/work/src/bad" nil "Too many open files"))]
    (is (= ["/work/src/ok.cljs"] found))
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: FileSystemException (Too many open files)"] lines))))

(deftest source-paths-warns-with-class-only-for-exception-without-reason
  (let [[found lines] (warnings-for-failing-folder (AccessDeniedException. "/work/src/bad"))]
    (is (= ["/work/src/ok.cljs"] found))
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: AccessDeniedException"] lines))))

(deftest source-paths-warns-with-class-only-for-blank-reason
  (let [[_ lines] (warnings-for-failing-folder (DirectoryIteratorException. (IOException. " ")))]
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: IOException"] lines))))

(deftest source-paths-skips-folder-whose-listing-fails-midway
  (let [[found lines] (warnings-for-failing-folder (DirectoryIteratorException. (IOException. "device gone")))]
    (is (= ["/work/src/ok.cljs"] found)
        "folder is skipped and walk goes on")
    (is (= ["WARN: could not read folder /work/src/bad, skipped it: IOException (device gone)"] lines)
        "one warning with cause and its message")))

(deftest delete-recursively-throws-when-folder-listing-failed
  (with-open [jimfs (memory-fs/unix)]
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

      (testing "missing-dirs"
        (is (= ["nope" "also-nope"] (fs/missing-dirs ["nope" root "also-nope"]))
            "folder that exists is not reported, and each missing one is"))

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
                  fs/io-file record
                  fs/move-over! record]
      (is (= (System/getProperty "user.dir") (fs/absolute-path ""))
          "empty path gives working directory, as in java.io")
      (is (false? (fs/absolute-path? "")))
      (is (nil? (fs/parent-dir "")))
      (is (false? (fs/file-exists? "")))
      (is (= [""] (fs/missing-dirs [""])))
      (is (false? (fs/mkdirs "")))
      (is (= "/x" (fs/join-path "" "x"))
          "empty base stands for root, as in java.io")
      (is (= "src/a.cljs" (fs/relativize "" "src/a.cljs")))
      (is (= "src/a.cljs" (fs/relativize "" (str (System/getProperty "user.dir") "/src/a.cljs")))
          "empty base stands for working directory, as in java.io")
      (is (= "" (fs/relativize "src" "")))
      (is (false? (fs/delete-file! "")))
      (is (nil? (fs/delete-tree! "")))
      (is (thrown? NoSuchFileException (fs/move-replace! "" "x")))
      (is (thrown? NoSuchFileException (fs/move-replace! "x" "")))
      (is (= [] (fs/list-source-files "" extensions))))
    (is (empty? @calls)
        "no file operation runs on working directory")))

(deftest string-adapters-keep-results-for-nul-path
  (let [calls (atom [])
        record (fn [& args] (swap! calls conj args) nil)]
    (with-redefs [fs/delete-recursively! record
                  fs/move-over! record]
      (is (= (str (System/getProperty "user.dir") "/" nul-path) (fs/absolute-path nul-path))
          "absolute-path keeps text of java.io")
      (is (false? (fs/absolute-path? nul-path)))
      (is (true? (fs/absolute-path? (str "/" nul-path)))
          "absolute NUL path gives true, as in java.io")
      (is (= "x" (fs/parent-dir (str "x/" nul-path)))
          "parent-dir keeps text of java.io")
      (is (false? (fs/file-exists? nul-path)))
      (is (= [nul-path] (fs/missing-dirs [nul-path])))
      (is (false? (fs/mkdirs nul-path)))
      (is (= (str nul-path "/x") (fs/join-path nul-path "x")))
      (is (= (str "src/" nul-path) (fs/join-path "src" nul-path)))
      (is (= "src/a.cljs" (fs/relativize nul-path "src/a.cljs")))
      (is (= nul-path (fs/relativize "src" nul-path)))
      (is (false? (fs/delete-file! nul-path)))
      (is (nil? (fs/delete-tree! nul-path)))
      (is (thrown? NoSuchFileException (fs/move-replace! nul-path "x")))
      (is (= [] (fs/list-source-files nul-path extensions))))
    (is (empty? @calls)
        "no file operation runs")))

(deftest mkdirs-gives-false-on-file-or-broken-link
  (let [root (temp-dir/create-dir "fs-mkdirs-")]
    (try
      (let [file (str root "/file")
            link (fs/path (FileSystems/getDefault) root "link")]
        (spit file "x")
        (is (false? (fs/mkdirs file))
            "regular file gives false, as in java.io")
        (Files/createSymbolicLink link
                                  (fs/path (FileSystems/getDefault) root "gone")
                                  (make-array FileAttribute 0))
        (is (false? (fs/mkdirs (str link)))
            "broken link gives false, as in java.io")
        (is (false? (fs/mkdirs (str file "/sub")))
            "folder under regular file gives false, as in java.io"))
      (finally (temp-dir/delete-dir! root)))))
