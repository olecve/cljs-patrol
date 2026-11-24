(ns cljs-patrol.memory-fs
  "In-memory file systems for tests, from Jimfs.

  A test that needs no real disk makes its files here, so it writes nothing to the disk and has nothing to delete.
  The helpers call NIO and not `cljs-patrol.fs`, because a mutant can change `fs`."
  (:import
   [com.google.common.jimfs Configuration Jimfs]
   [java.nio.file FileSystem Files OpenOption Path]
   [java.nio.file.attribute FileAttribute]))

(defn unix
  "Make a new, empty file system with Unix paths and the working directory `/work`.

  Jimfs models Unix behavior, which can differ from the real disk. Close the file system when the test is done."
  ^FileSystem []
  (Jimfs/newFileSystem (Configuration/unix)))

(defn windows
  "Make a new, empty file system with Windows paths and the working directory `C:\\work`.

  Close the file system when the test is done."
  ^FileSystem []
  (Jimfs/newFileSystem (Configuration/windows)))

(defn path
  "Make a `Path` on `file-system` from the text of `first-part` and of each of `more`."
  ^Path [^FileSystem file-system first-part & more]
  (.getPath file-system (str first-part) (into-array String (map str more))))

(defn typed
  "Make a {:path :text} map, as `core/run-all` takes for a source folder, with the text of `p` as its typed text."
  [^Path p]
  {:path p
   :text (str p)})

(defn write!
  "Write `text` as UTF-8 to the file `p`, and create the folders above it."
  ^Path [^Path p ^String text]
  (when-let [parent (.getParent p)]
    (Files/createDirectories parent (make-array FileAttribute 0)))
  (Files/writeString p text (make-array OpenOption 0)))

(defn read-text
  "Read the file `p` as UTF-8."
  ^String [^Path p]
  (Files/readString p))

(defn with-baseline-path
  "Call `f` with the `Path` of a baseline file that does not exist yet, on a new Unix file system."
  [f]
  (with-open [file-system (unix)]
    (f (path file-system "/work/baseline.edn"))))
