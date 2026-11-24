(ns cljs-patrol.disk
  "Paths on the real disk for tests.

  They come from NIO and not from `cljs-patrol.fs`, because a mutant can change `fs`."
  (:import
   [java.nio.file Files Path]
   [java.nio.file.attribute PosixFilePermissions]))

(defn path
  "Make a `Path` on the default file system from the text of `first-part` and of each of `more`."
  ^Path [first-part & more]
  (Path/of (str first-part) (into-array String (map str more))))

(defn typed-path
  "Make a {:path :text} map, as `core/run-all` takes for a source folder, with the text of the `Path` as its typed text."
  [first-part & more]
  (let [p (apply path first-part more)]
    {:path p
     :text (str p)}))

(defn with-locked-file
  "Take all permissions from the file `file`, call `f`, and give the permissions back after.

  When the JVM can still read the locked file, for example as root, `f` does not run. Then the test prints a SKIP line
  with `test-name` and makes no assertion. The runner counts that test as a pass, so the SKIP line is the only signal
  that it did not run."
  [test-name ^Path file f]
  (Files/setPosixFilePermissions file (PosixFilePermissions/fromString "---------"))
  (try
    (if (Files/isReadable file)
      (println (str "SKIP " test-name ": the JVM can read a file with no permissions, so this test does not run"))
      (f))
    (finally
      (Files/setPosixFilePermissions file (PosixFilePermissions/fromString "rw-------")))))
