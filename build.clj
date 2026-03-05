(ns build
  (:require
   [clojure.tools.build.api :as b]))

(def lib 'cljs-patrol/cljs-patrol)
(def version "0.1.0")
(def main-ns 'cljs-patrol.core)
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber
  "Builds a standalone executable jar.
  Run with: clojure -T:build uber
  Then run with: java -jar target/cljs-patrol-0.1.0.jar <source-dir>"
  [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
    (b/compile-clj {:basis basis
                    :src-dirs ["src"]
                    :class-dir class-dir})
    (b/uber {:basis basis
             :class-dir class-dir
             :main main-ns
             :uber-file uber-file}))
  (println (str "Built " uber-file)))
