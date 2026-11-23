(ns cljs-patrol.temp-dir
  "Temporary folders for tests, made by the JDK and not by the code under test.

  A test that took its folder from `cljs-patrol.fs` wrote its files into the working directory
  when a mutant made that path relative.")

(defn create-dir
  "Create a new, empty folder in the OS temporary folder, its name starting with `prefix`, and return its absolute path."
  [prefix]
  (str (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(def ^:private shared-dir
  "One temporary folder for the files of [[file-path]] in this JVM, which the JVM deletes when it exits, if it is empty."
  (delay (doto (java.io.File. ^String (create-dir "cljs-patrol-"))
           (.deleteOnExit))))

(def ^:private file-count (atom 0))

(defn file-path
  "Return the absolute path of a new file whose name ends with `file-name`, in a temporary folder, without creating it.

  The test deletes the file, or the folder of that name, when it is done."
  [file-name]
  (str (java.io.File. ^java.io.File @shared-dir (str (swap! file-count inc) "-" file-name))))
