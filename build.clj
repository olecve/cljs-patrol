(ns build
  (:require
   [clojure.tools.build.api :as b]))

(def lib 'cljs-patrol/cljs-patrol)
(def version (or (System/getProperty "cljs-patrol.version") "dev"))
(def main-ns 'cljs-patrol.core)
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s.jar" (name lib) version))
(def native-binary (format "target/%s" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber
  "Builds a standalone executable jar.
  Run with: clojure -T:build uber
  Then run with: java -jar target/cljs-patrol-<version>.jar <source-dir>"
  [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/compile-clj {:basis basis
                    :src-dirs ["src"]
                    :class-dir class-dir})
    (b/uber {:basis basis
             :class-dir class-dir
             :main main-ns
             :uber-file uber-file}))
  (println (str "Built " uber-file)))

(defn native
  "Builds a native binary using GraalVM native-image.
  Requires GraalVM with native-image on PATH (Oracle GraalVM 21+ bundles it).
  Run with: clojure -T:build native
  Then run: ./target/cljs-patrol <source-dir>"
  [_]
  (uber nil)
  (let [{:keys [exit]}
        (b/process {:command-args
                    ["native-image"
                     "-jar" uber-file
                     "-o" native-binary
                     "--no-fallback"
                     "--features=clj_easy.graal_build_time.InitClojureClasses"
                     "-H:+ReportExceptionStackTraces"
                     "-H:IncludeResources=cljs_patrol/.*"
                     "-J-Xmx4g"]})]
    (if (zero? exit)
      (println (str "Built native binary: " native-binary))
      (do (println "native-image failed")
          (System/exit 1)))))
