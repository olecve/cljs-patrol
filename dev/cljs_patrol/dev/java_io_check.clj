(ns cljs-patrol.dev.java-io-check
  "Find file access through `java.io` in the code that clj-kondo reads, as ADR 0013 asks for `src/`.

  It reads the analysis of clj-kondo, so each import shape, a full class name, a constructor, a static call and a type
  hint count. clj-kondo's `:discouraged-var` finds the vars of `clojure.java.io`, but not this Java interop."
  (:gen-class))

(def ^:private java-io-file-class
  "The class `java.io.File`, and each `java.io` class whose name starts with `File`, such as `FileReader`."
  #"java\.io\.File\w*")

(defn hits
  "Give each use of a `java.io` file class and each `.toFile` call in the clj-kondo `analysis`, sorted by place."
  [analysis]
  (->> (concat
        (for [{:keys [class filename row col]} (:java-class-usages analysis)
              :when (re-matches java-io-file-class class)]
          {:filename filename
           :row row
           :col col
           :text class})
        (for [{:keys [method-name filename name-row name-col]} (:instance-invocations analysis)
              :when (= "toFile" method-name)]
          {:filename filename
           :row name-row
           :col name-col
           :text ".toFile"}))
       distinct
       (sort-by (juxt :filename :row :col))))

(defn hit-line [{:keys [filename row col text]}]
  (str filename ":" row ":" col ": " text))

(defn -main
  "Lint `paths` with clj-kondo, print each hit, and exit 1 when there is one."
  [& paths]
  (let [lint! (requiring-resolve 'clj-kondo.core/run!)
        {:keys [analysis]} (lint! {:lint paths
                                   :cache false
                                   :config {:analysis {:java-class-usages true
                                                       :instance-invocations true}}})
        found (hits analysis)]
    (run! #(println (hit-line %)) found)
    (when (seq found)
      (println "Found java.io file access. Use the Path functions of cljs-patrol.fs."))
    (shutdown-agents)
    (System/exit (if (seq found) 1 0))))
