(ns cljs-patrol.dev.java-io-check
  "Find file access through `java.io` in the code that clj-kondo reads, as ADR 0013 asks for `src/`.

  It reads the analysis of clj-kondo, so each import shape, a full class name, a constructor, a static call and a type
  hint count. clj-kondo's `:discouraged-var` finds the vars of `clojure.java.io`, but not this Java interop.

  Known gaps: the analysis of clj-kondo 2026.08.04 has no entry for `(memfn toFile)` or for an array type hint such as
  `^\"[Ljava.io.File;\"`, so this check does not find them."
  (:gen-class))

(def ^:private flagged-class
  "The `java.io` classes that this check flags.

  These are `java.io.File`, each `java.io` class whose name starts with `File`, such as `FileReader`, and
  `java.io.RandomAccessFile`."
  #"java\.io\.(File\w*|RandomAccessFile)")

(defn- place [{:keys [filename row col]}]
  (if (and row (pos? row))
    (str filename ":" row ":" col)
    filename))

(defn hits
  "Give each use of a flagged `java.io` class and each `.toFile` call in the clj-kondo `analysis`, sorted by place.

  A use with no row, such as the second entry clj-kondo gives for a `reify`, is left out when the same file has a use of
  that class with a row."
  [analysis]
  (let [class-hits (->> (:java-class-usages analysis)
                        (filter #(re-matches flagged-class (:class %)))
                        (map (fn [{:keys [class filename row col]}]
                               {:filename filename
                                :row row
                                :col col
                                :text class})))
        located (set (keep #(when (:row %) [(:filename %) (:text %)]) class-hits))]
    (->> (concat
          (remove #(and (nil? (:row %)) (located [(:filename %) (:text %)])) class-hits)
          (for [{:keys [method-name filename name-row name-col]} (:instance-invocations analysis)
                :when (= "toFile" method-name)]
            {:filename filename
             :row name-row
             :col name-col
             :text ".toFile"}))
         distinct
         (sort-by (juxt :filename :row :col)))))

(defn hit-line [hit]
  (str (place hit) ": " (:text hit)))

(defn lint
  "Lint with clj-kondo and the analysis this check reads.

  The `options` go to `clj-kondo.core/run!`, such as `:lint` and `:filename`. Their `:config` cannot turn off the
  analysis this check reads."
  [options]
  ((requiring-resolve 'clj-kondo.core/run!)
   (-> options
       (assoc-in [:config :analysis :java-class-usages] true)
       (assoc-in [:config :analysis :instance-invocations] true))))

(def ^:private untrusted-finding-types
  "The clj-kondo finding types after which the analysis can miss code.

  These are a file that clj-kondo cannot read or parse, and a missing path."
  #{:file :syntax})

(defn lint-problems
  "Give a line for each reason that the clj-kondo `result` cannot prove the code clean.

  These are a finding of a file that clj-kondo cannot read or parse, a missing path, and a run with no file. Other
  findings, such as an unresolved symbol, leave the analysis whole."
  [{:keys [findings summary]}]
  (concat
   (for [finding findings
         :when (untrusted-finding-types (:type finding))]
     (str (place finding) ": clj-kondo error: " (:message finding)))
   (when (zero? (:files summary 0))
     ["clj-kondo linted no file."])))

(defn report
  "Print each problem and hit of the clj-kondo `result`, and give the exit code.

  The code is 2 when the check cannot run, 1 on a hit, and 0 otherwise."
  [result]
  (let [problems (lint-problems result)
        found (hits (:analysis result))]
    (run! println problems)
    (run! #(println (hit-line %)) found)
    (cond
      (seq problems) (do (println "The check cannot run. Fix the clj-kondo errors above.")
                         2)
      (seq found) (do (println "Found java.io file access. Use the Path functions of cljs-patrol.fs.")
                      1)
      :else 0)))

(defn run-cli
  "Check `paths`, print each problem and hit, and give the exit code of [[report]], or 2 when there is no path."
  [paths]
  (if (empty? paths)
    (do (println "Give the paths to check, such as src.")
        2)
    (report (lint {:lint paths}))))

(defn -main [& paths]
  (let [exit (run-cli paths)]
    (shutdown-agents)
    (System/exit exit)))
