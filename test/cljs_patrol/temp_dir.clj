(ns cljs-patrol.temp-dir
  "Temporary folders for tests, made and deleted by the JDK and not by the code under test.

  A test that took its folder from `cljs-patrol.fs` wrote its files into the working directory
  when a mutant made that path relative."
  (:import
   [java.nio.file FileVisitResult Files Path SimpleFileVisitor]
   [java.nio.file.attribute FileAttribute]))

(defn create-dir
  "Create a new, empty folder in the OS temporary folder, its name starting with `prefix`, and return its absolute path.

  The test deletes the folder when it is done, with [[delete-dir!]], [[with-dir]] or [[delete-created]]."
  [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn delete-dir!
  "Delete the folder `dir` with all it holds, and do nothing when it is gone.

  The walk does not follow a link, so a link to a folder outside goes, and that folder stays."
  [dir]
  (let [path (-> dir str java.io.File. .toPath)]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (Files/walkFileTree path
                          (proxy [SimpleFileVisitor] []
                            (visitFile [^Path file _]
                              (Files/delete file)
                              FileVisitResult/CONTINUE)
                            (postVisitDirectory [^Path folder _]
                              (Files/delete folder)
                              FileVisitResult/CONTINUE))))))

(defn with-dir
  "Call `f` with a new temporary folder, a java.io.File whose name starts with `prefix`, and delete the folder after."
  [prefix f]
  (let [dir (java.io.File. ^String (create-dir prefix))]
    (try
      (f dir)
      (finally (delete-dir! dir)))))

(def ^:private created
  "The folders that [[file-path]] made, which [[delete-created]] deletes."
  (atom []))

(defn file-path
  "Return the absolute path of a file named `file-name` in a new temporary folder, without creating the file.

  A namespace that calls it deletes the folder with the fixture [[delete-created]]."
  [file-name]
  (let [dir (create-dir "cljs-patrol-")]
    (swap! created conj dir)
    (str (java.io.File. ^String dir ^String file-name))))

(defn delete-created
  "Run `f`, a test, then delete each folder that [[file-path]] made, with all it holds."
  [f]
  (try
    (f)
    (finally
      (run! delete-dir! (first (reset-vals! created []))))))
