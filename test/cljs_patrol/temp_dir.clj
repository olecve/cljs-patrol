(ns cljs-patrol.temp-dir
  "Temporary folders for tests, made by the JDK and not by the code under test.

  A test that took its folder from `cljs-patrol.fs` wrote its files into the working directory
  when a mutant made that path relative.")

(defn create-dir
  "Create a new, empty folder in the OS temporary folder, its name starting with `prefix`, and return its absolute path.

  The test deletes the folder when it is done."
  [prefix]
  (str (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

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
      (doseq [dir (first (reset-vals! created []))]
        (run! #(.delete ^java.io.File %) (reverse (file-seq (java.io.File. ^String dir))))))))
