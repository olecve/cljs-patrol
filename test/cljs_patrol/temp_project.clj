(ns cljs-patrol.temp-project
  "A project written to a temporary directory, for tests that run the whole analysis."
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.temp-dir :as temp-dir]
   [clojure.java.io :as io]))

(defn run-group
  "Write `files`, a map of relative path to source, as a project, run `rule-group` over it, and delete it.

  Return {:results the group's results, :errors what the run wrote to stderr}."
  [rule-group files]
  (let [dir (io/file (temp-dir/create-dir "cljs-patrol"))
        errors (java.io.StringWriter.)]
    (try
      (doseq [[path source] files]
        (let [file (io/file dir path)]
          (io/make-parents file)
          (spit file source)))
      {:results (binding [*err* errors]
                  (-> (core/run (disk/typed-path dir) [rule-group]) :group-results first))
       :errors (str errors)}
      (finally
        (temp-dir/delete-dir! dir)))))
