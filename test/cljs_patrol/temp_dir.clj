(ns cljs-patrol.temp-dir
  "Temporary folders for tests, made by the JDK and not by the code under test.

  A test that took its folder from `cljs-patrol.fs` wrote its files into the working directory
  when a mutant made that path relative.")

(defn create-dir
  "Create a new, empty folder in the OS temporary folder, its name starting with `prefix`, and return its absolute path."
  [prefix]
  (str (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn file-path
  "Return the absolute path of a file named `file-name` in a new temporary folder, without creating the file.

  The JVM deletes the folder when it exits, if the test deleted what it put there."
  [file-name]
  (let [dir (java.io.File. ^String (create-dir "cljs-patrol-"))]
    (.deleteOnExit dir)
    (str (java.io.File. dir ^String file-name))))
